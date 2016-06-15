complete <- function(directory,id=1:332){
    header <- c("id","nobs")
    fullfile <- data.frame(matrix(nrow=0,ncol=2,dimnames=list(NULL,header)))
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
        fullfile <- rbind(fullfile,data.frame(id=i,nobs=sum(compCase)))
    }
    return (fullfile)
}