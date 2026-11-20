This repo is a impala customised instantiation of the same code found in the [data wrangling and dead reckoning](https://github.com/OakAlice/IntegratedCollarAnalysis) and [machine learning behavioural classification](https://github.com/OakAlice/AniML/tree/main) repositories.

## Preparing the data
The original plan had been to collect IMU data with our custom collars - data I refer to as Artemis data. This data has a very specific formatting and can be read in using a workflow written by Christofer Clemente, Chris Bird, and I. During this stage, misreads must be managed, and the GPS clocks used to update the internal accelerometer clock.

## Behavioural annotation
Because field work is so hard, we accidentally forgot to perform calibration checks on these collars while out in the field (yeeek!). Therefore, we have to align them based on the GPS timestamp matching to the videos. This is pretty good, but not quite perfect. Therefore, we have developed a workflow that allows us to manually clip a section of data to within a few seconds of the likely video start time. These clipped sections are then fed into the custom matlab GUI "SyncStation" and behavioural annotations for each second of the video applied.

## Machine learning behavioural classification
Using the annotated data prepared in the previous step, a supervised machine learning program to detect specific fine scale behaviours is developed. This code is a variant of the main codebase [aniML](https://github.com/OakAlice/AniML/tree/main). Detailed instructions can be found at the parent repository.

The specifics of this project mainly required customisation around behaviour recombination and optimising for the ecologically important behaviours. There was a lot of confusion between some of the posturally-equivalent behaviours that required extra data cleaning and tweaking.

## Dead reckoning 
Using the data prepared earlier, with the magnetometer, accelerometer, and GPS, we can reconstruct exact movement paths. This was largely handled by Chris Bird and then implemented by me.
