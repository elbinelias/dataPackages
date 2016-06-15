pollutantmean <- function(directory, pollutant, id=1:332){
    header <- c("Date","sulfate","nitrate","ID")
    fullfile <- data.frame(matrix(nrow=0,ncol=4,dimnames=list(NULL,header)))
    for (i in id) {
            if (i<10) {
                fname <- paste("00",i,sep="")
            }
            if (i<100 && i>9) {
                fname <- paste("0",i,sep="")
            }
            if (i>100) {
                fname <- i
            }
            file <- read.csv(paste(directory,"/",fname,".csv",sep=""), header=TRUE, sep=",") 
            fullfile <- rbind(fullfile,file)
    }
    #write.csv(fullfile,file="/users/Elbin/Documents/DataScienceWorkspace/Assignments/Session2/Week2/output3.csv",sep=",")
    if (pollutant=="sulfate"){
    fullmean <- mean(fullfile$sulfate,na.rm=TRUE)
    }
    if (pollutant=="nitrate"){
        fullmean <- mean(fullfile$nitrate,na.rm=TRUE)
    }
    return(fullmean)
}
