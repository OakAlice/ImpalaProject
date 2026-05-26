##################
# MadgwickCompass

# Overview
# We read in the data, run it through a function, save it, and then use it

# Requires:
# Smoothed and corrected data

# Use the method from here:
# https://ahrs.readthedocs.io/en/latest/filters/madgwick.html#module-ahrs.filters.madgwick 
# read in the data
import os
import glob
import numpy as np
import pandas as pd
from ahrs.filters import Madgwick

# Read in the data
files = [f for f in glob.glob("C:/Users/PC/Documents/ImpalaProject/Data/RawData/Collar_8/Chunked/*.csv") 
         if "_quaternion" not in os.path.basename(f)]

for file in files:
    data = pd.read_csv(file)
    
    # Convert accelerometer to m/s²
    data["RawAX.sm"] = data["RawAX.sm"] * 9.81
    data["RawAY.sm"] = data["RawAY.sm"] * 9.81
    data["RawAZ.sm"] = data["RawAZ.sm"] * 9.81
    
    # convert the gyroscrope
    # according to what Chris Bird said to me based on the data sheet
    # Divide the gyroscope by 2^15 and then multiply by either [250 or 500] * pi / 180
    data["RawGX"] = data["RawGX"] / 2**15 * 500 * np.pi / 180
    data["RawGY"] = data["RawGY"] / 2**15 * 500 * np.pi / 180
    data["RawGZ"] = data["RawGZ"] / 2**15 * 500 * np.pi / 180
    
    # magnetometer has already been converted

    # convert all to numpy
    gyr = data[["RawGX", "RawGY", "RawGZ"]].to_numpy()
    acc = data[["RawAX.sm", "RawAY.sm", "RawAZ.sm"]].to_numpy()
    mag = data[["RawMX.sm", "RawMY.sm", "RawMZ.sm"]].to_numpy()
    
    # Convert all the data to the expected NED&NED orientation
    # ours is acc&gyro NWU and mag NED
    acc[:,1] *= -1
    acc[:,2] *= -1
    gyr[:,1] *= -1
    gyr[:,2] *= -1

    # Run Madgwick filter over all rows
    q = np.array([1.0, 0.0, 0.0, 0.0])  # initial quaternion (w, x, y, z)
    quaternions = np.zeros((len(data), 4))
    
    madgwick = Madgwick(frequency=50.0)
    
    for i in range(len(data)):
        q = madgwick.updateMARG(q=q, gyr=gyr[i], acc=acc[i], mag=mag[i], dt=1/50)
        quaternions[i] = q

    # Build output dataframe
    q_df = pd.DataFrame(quaternions, columns=["w", "x", "y", "z"])
    q_df.insert(0, "utc_datetime", data["utc_datetime"].values)
    
    # now convert these to roll pitch and yaw
    # note that the next equation requires input in degrees 
    def quaternion_to_euler(q_w, q_x, q_y, q_z):

        Roll = np.arctan2(
            2 * (q_w*q_x + q_y*q_z),
            1 - 2*(q_x**2 + q_y**2)
        )
        Roll = np.degrees(Roll)
        
        Pitch = np.arcsin(
            np.clip(
                2*(q_w*q_y - q_z*q_x),
                -1.0,
                1.0
            )
        )
        Pitch = np.degrees(Pitch)
        
        Yaw = np.arctan2(
            2*(q_w*q_z + q_x*q_y),
            1 - 2*(q_y**2 + q_z**2)
        )
        Yaw = np.degrees(Yaw)
    
        return Roll, Pitch, Yaw

    # Apply to quaternion dataframe
    q_df["Roll"], q_df["Pitch"], q_df["Yaw"] = quaternion_to_euler(
        q_df["w"].to_numpy(),
        q_df["x"].to_numpy(),
        q_df["y"].to_numpy(),
        q_df["z"].to_numpy()
    )

    # Strip date from filename and save
    basename = os.path.basename(file) 
    date_str = basename.split("_")[2] 
    date_str = date_str.split(".")[0] 
    out_name = f"{date_str}_quaternions.csv"
    out_path = os.path.join("C:/Users/PC/Documents/ImpalaProject/Data/RawData/Collar_8/Chunked", out_name)
    
    q_df.to_csv(out_path, index=False)
    print(f"Saved: {out_path}")
