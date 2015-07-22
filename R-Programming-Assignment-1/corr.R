corr <- function(directory, threshold = 0) {
    mydata <- list.files(directory, full.names=T)
    dataset <- data.frame()
    data <- c()
    for (i in 1:332) {
        dataset <- read.csv(mydata[i])
        nobs <- sum(complete.cases(dataset))
        if (nobs > threshold) {
            cleandata <- na.omit(dataset)
            data <- c(data,cor(cleandata[,'sulfate'],cleandata[,'nitrate']))
        }
    }
    print(data)
}