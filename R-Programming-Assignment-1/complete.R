complete <- function(directory, id = 1:332) {
    mydata <- list.files(directory, full.names=T)
    dataset <- data.frame()  
    nobs <- c()
    x <- c()
    for (i in id) {
        dataset <- read.csv(mydata[i])
        nobs<- c(nobs,sum(complete.cases(dataset)))
        x <- c(x, i)
    }
    complete_case <- data.frame(id=x,nobs=nobs)
    complete_case
}