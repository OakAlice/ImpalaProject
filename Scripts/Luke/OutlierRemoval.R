OutlierRemoval <- function(df,outliers){
  
  outliers <- rbind(outliers$selected_outliers_xy,outliers$selected_outliers_yz)
  outliers <- unique(outliers)
  idcs <- rep(0, nrow(outliers))
  temp <- data.frame(df$mX, df$mY, df$mZ)
  names(temp) <- c("mX", "mY", "mZ")
  
  tolerance <- 0.01
  # Initialize a list to store indices
  idcs <- vector("list", nrow(outliers))
  
  dX <- as.numeric(temp$mX)
  dY <- as.numeric(temp$mY)
  dZ <- as.numeric(temp$mZ)
  
  # Loop through each row in the outliers dataframe
  for (i in 1:nrow(outliers)) {
    oX <- as.numeric(outliers[i, 1])
    oY <- as.numeric(outliers[i, 2])
    oZ <- as.numeric(outliers[i, 3])
    
    # Find indices matching the xyz values and store them in the list
    idcs[[i]] <- which(abs(df$mX - oX) <= tolerance & abs(df$mY - oY) <= tolerance & abs(df$mZ - oZ) <= tolerance)
  }
  idcs <- unlist(idcs)
  df <- df[-idcs, ]
  
  return(df)
}