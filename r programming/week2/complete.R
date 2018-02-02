complete <- function(directory, id=1:332) {
        obs <- c() 
        i <- 1
        for(monitor in id) {
                data = read.csv(paste(directory, "\\", str_pad(monitor, 3, pad = "0"), ".csv", sep=""))
                hasObservation <- sum(!is.na(data$sulfate) & !is.na(data$nitrate))
                obs[i] <- hasObservation
                i <- i + 1
        }
        
        data.frame(id, obs)
}