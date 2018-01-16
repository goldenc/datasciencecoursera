library(stringr)

pollutantmean <- function(directory, pollutant, id=1:332) {
        combinedData = NULL
        for(monitor in id) {
                data = read.csv(paste(directory, "\\", str_pad(monitor, 3, pad = "0"), ".csv", sep=""))
                if(!is.null(combinedData)) {
                        combinedData = rbind(data, combinedData)
                }
                else {
                        combinedData = data
                }
        }
        
        mean(combinedData[[pollutant]], na.rm = TRUE)
}