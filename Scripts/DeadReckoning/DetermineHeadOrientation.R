#################
# DetermineHeadOrientation

# Overview:
# Figure out the math to figure out whether the head is up or down

# Requires:
# accel_data smoothed and formatted by the DeadReckoningPerDay.R flow
# at the point where this file is linked

# Note:
# This is a play script not included in the automated workflow

#################

dat <- accel_data[180000:190000,]

ggplot(dat, aes(x = utc_datetime)) +
  geom_path(aes(y = RawAY, colour = "Y")) +
  geom_path(aes(y = RawAX, colour = "X")) + 
  geom_path(aes(y = RawAZ, colour = "Z")) 


ggplot(dat, aes(x = utc_datetime)) +
  geom_path(aes(y = RawGY, colour = "Y")) +
  geom_path(aes(y = RawGX, colour = "X")) + 
  geom_path(aes(y = RawGZ, colour = "Z")) + 
  ylim(-50, 50)

accel_data$meanAX <- rollapply(accel_data$RawAX, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
accel_data$meanAY <- rollapply(accel_data$RawAY, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
accel_data$meanAZ <- rollapply(accel_data$RawAY, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
accel_data$headpos <- ifelse(accel_data$meanAY > accel_data$meanAX, "Up", "Down")

dat <- accel_data[170000:200000,]

ggplot(dat, aes(x = utc_datetime, colour = headpos, group = 1)) +
  geom_path(aes(y = RawAY)) +
  geom_path(aes(y = RawAX)) + 
  geom_path(aes(y = RawAZ))
