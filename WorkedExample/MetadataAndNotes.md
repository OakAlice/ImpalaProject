# Metadata
Metadata for the data provided to ChrisB so he could help resolve the dead reckoning.

## Columns
RawAX, RawAY, RawAZ: The raw acceleration measurements.
RawGX, RawGY, RawGZ: The raw gyroscope measurements.
RawMX, RawMY, RawMZ: The raw magnetometer measurements.
RawAX.scaled, RawAY.scaled, RawAZ.scaled: The acceleration unites / 2048.
RawMX.scaled, RawAY.scaled, RawAZ.scaled: The magnetometer units / 2048 and then * 0.15
RawAX.cl, RawAY.cl, RawAZ.cl, RawMX.cl, RawMY.cl, RawMZ.cl: cleaned data -> .scaled columns median filtered and then lowpass butterworth filtered

