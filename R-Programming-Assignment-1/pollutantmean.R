pollutantmean <- function(directory, pollutant, id = 1:332) {
    mydata <- list.files(directory, full.names=T)
    data <- data.frame()
    for(i in id){
        data <- rbind(data,read.csv(mydata[i]))
    }
    means <- mean(data[ ,pollutant], na.rm=T)
    means
}