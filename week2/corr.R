corr <- function(directory, threshold = 0) {
        completedCases <- complete(directory)
        results <- c()
        for(monitor in completedCases$id) {
                if(completedCases$obs[[monitor]] > threshold) {
                        data = read.csv(paste(directory, "\\", str_pad(monitor, 3, pad = "0"), ".csv", sep=""))
                        complete <- data[!is.na(data$nitrate) & !is.na(data$sulfate), ]
                        corResult <- cor(complete$sulfate, complete$nitrate)
                        results <- c(results, corResult)
                }
        }
        
        results
}
