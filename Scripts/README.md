# Understanding the repo

This repo contains the code for the complete analysis of the impala data from the raw files all the way through to ecological analysis. Where possible, code was made generalised and transferable. Scripts are organised into sub-folders by topic.

# Part 1: DataWrangling
This section of the code is for taking the raw artemis board reads and converting it to usable data.

- **Input**: Raw txt files _dataLog.txt and _serialLog.csv from the Artemis logger.
- **Output**: Time-corrected, cleaned data, in 24hr chunks.
- **Scripts**:
    - *Main_ReadData.R*
        - Loops through the collars
        - Acceleromter:
            - Removes the irrelevant files
            - Stitches together the raw files read from the artemis board
            - Formats the accelerometer data: rescales units (information on conversion in the attached datasheet), removed misreads with median filter, lowpass butterworth filter to remove noise, and rolling mean smooths
            - Calculates the VDBA (necessary for later)
            - Removes unnecessary columns
            - Saves as "Board_Accel.csv"
        - GPS:
            - Reads in the txt files, removing debug misreads and gibberish.
            - Saves as "Board_GPS.csv"
        - Alignment: 
            - Joins the accelerometer and GPS data together based on the rtc_datetime (internal clock).
            - Interpolates between gps hits to assign utc_timestamp across accelerometer data.
    - *DataRead_Functions.R*: Functions to assist with the above
    - *UnitScales.R*: BREAKOUT SCRIPT. Determine what units the boards were in (the true units dont match the settings OLA file... oopsy)
    - *DetermineOrientation.R*: BREAKOUT SCRIPT. Check all boards were in the same orientation.

## Part 2: BehaviouralDetection
Section of the code for generating a behavioural classification model.

### Section 2.1: GenerateTrainingData
Creating the labelled examples of accelerometer data. Used for training the machine learning model.

- **Input**: Videos of the collar-wearing animals, times of videos, and the cleaned accelerometer 24hr chunks.
- **Output**: Csv with the labelled data: summary features and column "Activity"
- **Scripts**:
    - *Main_GenerateTrainingData.R*: Arranges and organisises the scripts for this section.
    - *VideoInfoExtraction.R*: Pull out the times and durations of all videos, apply the corrections so time is in utc.
    - *ExtractingAccelSegments.R*: FULLY INTERACTIVE. User clips the accelerometer to align with the videos. 
    - **SyncStation**: SUBFOLDER. FULLY INTERACTIVE. BREAKOUT SCRIPT. 
        - Matlab GUI for applying the behavioural labels to the accelerometer segments based on the time-synced videos. Full instructions for use of this script are provided in the subfolder.
    - *CleanLabelledData.R*: Get the txt files from the previous matlab phase, reattach them to the rest of the IMU data (originally didnt include gyro etc).
    - *ExploreLabelledData.R*: Explore the data to see whether the training data is good or not. If there are misreads or errors, return to the matlab phase and progress again.
    - *Functions_GenerateFeatures.R*: Functions to generate features across each window of the cleaned labelled data. Function call is in main script.

### Section 2.2: ModelDesign
Tune, train, and validate a machine learning model to detect the target behaviours.

- **Input**: Training data (prepared in the previous section)
- **Output**: Trained a validated behavioural classification machine learning model.
- **Scripts**:



- *GenerateTrainingData*: Synchronise videos with the accelerometers, annotate behaviours describing the accelerometer data, generate features across each window of the annotated data.
- *ModelDesign*: Tune, train, and validate a machine learning model to detect the target behaviours.
- *MakePredictions*: Apply the model to the unlabelled data. Also post-process the predictions to remove anomalies.
- **SyncStation**: Matlab gui used to annotate the time-synced videos and accelerometer trace.
- **DeadReckoning**: Use the GPS, predicted behaviours, and *Gundogs* R package to reconstruct exact movement paths.
- **EcologicalAnalysis**: Combine the dead-reckoned exact-path with energetic/activity analysis (VDBA), and behavioural predictions. Answer ecological questions. 

For a more detailed breakdown, see the instructions file.