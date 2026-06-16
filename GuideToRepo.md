# Understanding the repo

This repo contains the code for the complete analysis of the impala data from the raw files all the way through to ecological analysis.

- [Understanding the repo](#understanding-the-repo)
  - [User Notes](#user-notes)
  - [Analysis Sections](#analysis-sections)
    - [Part 1: DataWrangling](#part-1-datawrangling)
    - [Part 2: BehaviouralDetection](#part-2-behaviouraldetection)
      - [Section 2.1: GenerateTrainingData](#section-21-generatetrainingdata)
      - [Section 2.2: ModelDesign](#section-22-modeldesign)
      - [Section 2.3: MakePredictions](#section-23-makepredictions)
    - [Section 3: DeadReckoning](#section-3-deadreckoning)

## User Notes
This repo is the amalgamation of several years worth of work combined, not always in the most elegant way. I have tried throughout to ensure consistent conventions. The main thing to note is whether a script can be "sourced" and run as a whole (e.g., feature generation, or applying the predictions) or whether it is manual (e.g., the entire process of trainign data creation is highly manual and cannot be automated as it requires human input at every stage). While the overall process and individual code segments can be reused for other projects, this specific workflow is extremely customised for the impala data problems.

**NOTE TO SELF: PUT ALL FUNCTIONS FROM EACH SECTION INTO PER-SECTION EASILY SOURCABLE FILES????????**


## Analysis Sections
Scripts are organised into sub-folders by topic.

In the parent directory we have the main script as well as functions that are used throughout the other scripts.

- *Main.R*: Main script that directs the analysis process.
- *PlotFunctions.R*: My custom plotting theme and colours.

### Part 1: DataWrangling
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

### Part 2: BehaviouralDetection
Section of the code for generating a behavioural classification model.

#### Section 2.1: GenerateTrainingData
Creating the labelled examples of accelerometer data. Used for training the machine learning model.

- **Input**: Videos of the collar-wearing animals, times of videos, and the cleaned accelerometer 24hr chunks.
- **Output**: Csv with the labelled data: summary features and column "Activity"
- **Scripts**:
    - *CreateTrainingData.R*: BREAKOUT SCRIPT. Arranges and organisises the scripts for this section. -> This is a fully manual/interactive break-out script.
    - *VideoInfoExtraction.R*: Pull out the times and durations of all videos, apply the corrections so time is in utc.
    - *ExtractingAccelSegments.R*: FULLY INTERACTIVE. User clips the accelerometer to align with the videos. 
    - **SyncStation**: SUBFOLDER. FULLY INTERACTIVE. IN MATLAB NOT R. 
        - Matlab GUI for applying the behavioural labels to the accelerometer segments based on the time-synced videos. Full instructions for use of this script are provided in the subfolder.
    - *CleanLabelledData.R*: Get the txt files from the previous matlab phase, reattach them to the rest of the IMU data (originally didnt include gyro etc). Also uses logic to adjust some of the annotations (i.e., using VDBA to differntiate between movement and non-movement within the feeding bouts).
    - Bonus interactive check scripts:
        - *ExploreLabelledData.R*: BREAKOUT SCRIPT. Explore the data to see whether the training data is good or not. If there are misreads or errors, return to the matlab phase and progress again.
        - *DecideClasses_TrainingData.R*: BREAKOUT SCRIPT. Look at base class seperability to see whether probable that the ML will be able to pull it apart.
    - *GenerateFeaturesTrainData.R*: Make features across each window of data (user defines setting such as which features, window length, overlap, etc. in the Main.R)
    - *Functions_GenerateFeatures.R*: Functions to generate features across each window of the cleaned labelled data. Function call is in main script.

#### Section 2.2: ModelDesign
Tune, train, and validate a machine learning model to detect the target behaviours. Also trial different post-processing approaches to smooth errors.

- **Input**: Training data (prepared in the previous section)
- **Output**: Trained a validated behavioural classification machine learning model and post-processing smoother.
- **Scripts**:
    - *Main_DesignModel.R*: Directs the cross-validation tuning, training, and testing of the model, as well as final model generation.
    - *Main_DevelopPostProcessing.R*: Trial multiple post-processing strategies and decide on the one that gets best performance gains.
    - *BuildSingleModel.R*: Hyperparameter tuning and results generating for each data subset.
    - *Functions_PostProcessing.R*: Each of the smoothing methods.
    - *Functions_TuneTrainTestModel.R*: Main function for hyperparameter optimisation.

#### Section 2.3: MakePredictions
Apply ML model and postprocessing smoother to unlabelled data to make behavioural predictions for each second. Largely uses functions from earlier in the script...

- **Input**: Training data (prepared in the previous section)
- **Output**: Trained a validated behavioural classification machine learning model and post-processing smoother.
- **Scripts**:
    - *Main_Unlabelled.R*: Generate features and apply the predictions, including post-processing.

### Section 3: DeadReckoning
Use onboard-estimated quaterions (or Magdwick-generated quaternions) to calculate the attitude of the collar at every instant, combine with behavioural & speed predictions, as well as verified GPS (smoothed based on predicted behaviour) via [Gundog.Tracks](https://link.springer.com/article/10.1186/s40317-021-00245-z) to dead-reckon exact path.




... write up in progress.