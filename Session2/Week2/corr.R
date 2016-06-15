corr <- function(directory, threshold=0){
    corVector=numeric()
    id <- 1:332
    for (i in id){
        if (i<10) {
            fname <- paste("00",i,sep="")
        }
        if (i<100 && i>9) {
            fname <- paste("0",i,sep="")
        }
        if (i>100) {
            fname <- i
        }
        file <- read.csv(paste(directory,"/",fname,".csv",sep=""),header=TRUE,sep=",")
        compCase <- complete.cases(file)
        if(sum(compCase)>threshold){
            corVal <- cor(file$sulfate,file$nitrate,use="complete.obs")
            corVector <- c(corVector,corVal)
        }
       # write.csv(fullfile,file="/users/Elbin/Documents/DataScienceWorkspace/Assignments/Session2/Week2/output3.csv",sep=",")
    }
    return(corVector)
}