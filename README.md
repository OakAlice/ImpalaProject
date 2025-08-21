# ImpalaProject

Code for behavioural analysis of impala data.

## Part 1: Data Wrangling
Much of this code is concerned with extracting the data from the Artemis boards' duel accelerometer and GPS loggers. Due to challenges with timestamps, data corruption, and inconsistent formatting, this was very challenging. Problem was solved with heavy assist from Chris Clemente (his parallel code for the same solution is available at: [https://github.com/cclemente/Collar_data_extraction](https://github.com/cclemente/Collar_data_extraction)) and I have updated my code to reflect his changes.
* [Main_RoughAlign.R](https://github.com/OakAlice/ImpalaProject/blob/main/Scripts/RoughAlignment/Main_RoughAlignment.R): Initialisation code for extracting information from txt files and aligning the timestamps
* [Combining Accel Files](https://github.com/OakAlice/ImpalaProject/blob/main/Scripts/RoughAlignment/CombingArtemisAccelFiles.R): Combining the txt accelerometer files safely (as opposed to my [original cmd-line method](https://github.com/OakAlice/ImpalaProject/blob/main/Scripts/CombiningAllBoardAccelFiles.txt) which was very fast, but more error prone)
* [Combining GPS Files](https://github.com/OakAlice/ImpalaProject/blob/main/Scripts/RoughAlignment/CombiningArtemisGPSFiles.R): Code for carefully extracting valid satellite hits from the artemis boards
* [Synchronising the accelerometer with the GPS](https://github.com/OakAlice/ImpalaProject/blob/main/Scripts/RoughAlignment/CombiningArtemisAccel%26GPS.R): Matching the timestamps and interpolating between sat hits - saves output in 1 day chunks

## Part 2: Creation of Training Data
This section of the code is for roughly aligning each of the videos with the corresponding section of the accelerometer data. This was finicky as every camera had a different datetime encoding and therefore there is a lot of manual work required.
* [Extracting All Video Metadata](https://github.com/OakAlice/ImpalaProject/blob/main/Scripts/RoughAlignment/VideoInfoExtraction.R)
* [Matching the Video to Accelerometer Section](https://github.com/OakAlice/ImpalaProject/blob/main/Scripts/RoughAlignment/FlaggingAccelSections.R)
* These matched data sources were then imported into the custom matlab GUI designed by [Chris Clemente](https://github.com/cclemente/Animal_accelerometry/tree/main/Matlab_scripts). A newer version of the GUI ([Sync Station GUI for Data Annotation](https://github.com/OakAlice/ImpalaProject/tree/main/Scripts/Sync_Station)) includes minor modifications making it more accesible for Mac users, as well as enabling multiple layers of simultaneous behavioural annotation.

## Part 3: Developing Supervised Classification Model
Code for this section of the project largely duplicated from projects I worked on previously but has been updated to the specific use-case of the impala data.

## Acknowledgements 
Project was conceptualised and funding aquired by Chris Clemente. Collars designed and built by Jasmin Annett, Robin Maag, Chris Clemente, and Chris Bird. Ethics obtained and managed by Jasmin Annett. Data collected by Jasmin Annett, Robin Maag, Chris Clemente, and Taylor Dick. Training data annotated by Senna Stewart. Data analysis by Chris Clemente, Senna Stewart, and myself.
