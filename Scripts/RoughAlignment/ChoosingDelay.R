


# Save with the right delay -----------------------------------------------

chosen_delay <- 20

###cleaning up accel files
matlab_origin <- 719529  # MATLAB datenum for 1970-0       1-01
# Convert to MATLAB fractional days
accel_segment[, t_matlab := as.numeric(gps_time_est) / 86400 + matlab_origin]
# Keep just MATLAB time and g-columns
out <- accel_segment[, .(t_matlab, AX_g, AY_g, AZ_g)]

write.csv(out, 'DJI_20240702082054_0038_D_recoded.csv')



