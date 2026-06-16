# ImpalaProject

Code for analysis of fine-scale ecology of free-roaming impala. Includes machine learning behavioural classification, dead-reckoning, and habitat integration.

![What I'm trying to achieve lol](/Figures/GitCover.png)

## Data 
Wild impala were captured and collared in 3 sites in South Africa (Tenbosch, Mjejane, and Pullen), across 2 weeks in July 2024. Matched 9-axis IMU data (accelerometer, gyroscope, magnetometer) and GPS was collected for 10 individuals with accelerometer only for an additional 4. All data was sampled at 50Hz. Most accelerometer data was collected at +/- 16g, however, some accidentally sampled at +/- 4g and had to have additional corrections applied. Video was collected from as many individuals as possible during the collar deployment period. Metadata for the individuals can be found in [Metadata](https://github.com/OakAlice/ImpalaProject/blob/functioning/Notes/Metadata.csv). *More information on the location to be added soon.*

## Using this repo
Detailed instructions and information for how to navigate and use the repo can be found in the [GuideToRepo](https://github.com/OakAlice/ImpalaProject/blob/functioning/GuideToRepo.md). Best efforts have been made to ensure code is well-documented, modular, and transferable, but unique quirks of this data meant that some code is very customised and interactive.

## Acknowledgements 
- Project was conceptualised and funding aquired by Chris Clemente. 
- Collars designed and built by Chris Bird, Jasmin Annett, Robin Maag, and Chris Clemente. 
- Ethics obtained and managed by Jasmin Annett. 
- Data collected by Jasmin Annett, Robin Maag, Chris Bird, Chris Clemente, and Taylor Dick. 
- Training data annotated by Amelia Nelson and me. 
- Dead Reckoning work begun by Luke Jessup and Jojo Schultz, based on code from Richard Gunner, continued by myself and innovated by Chris Bird (introduced quaternions). 
- Supervision and statistics support by David Schoeman. 
- Machine learning behavioural analysis, dead reckoning, data wrangling, general code pipeline, and write up by me. 
