# Understanding the repo

This repo contains the code for the complete analysis of the impala data from the raw files all the way through to ecological analysis. Where possible, code was made generalised and transferable. Scripts are organised into sub-folders by topic. Here is a basic overview:

- **WranglingData**: reading in, formatting, and aligning the data sources (i.e., gps and IMU).
- **BehaviouralDetection**: Machine learning for accelerometer-based behavioural classification
    - *GenerateTrainingData*: Synchronise videos with the accelerometers, annotate behaviours describing the accelerometer data, generate features across each window of the annotated data.
    - *ModelDesign*: Tune, train, and validate a machine learning model to detect the target behaviours.
    - *MakePredictions*: Apply the model to the unlabelled data. Also post-process the predictions to remove anomalies.
- **SyncStation**: Matlab gui used to annotate the time-synced videos and accelerometer trace.
- **DeadReckoning**: Use the GPS, predicted behaviours, and *Gundogs* R package to reconstruct exact movement paths.
- **EcologicalAnalysis**: Combine the dead-reckoned exact-path with energetic/activity analysis (VDBA), and behavioural predictions. Answer ecological questions. 