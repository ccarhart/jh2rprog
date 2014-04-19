corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    dfcomplete <- complete(directory)
    result<-numeric()
    for (i in dfcomplete$id) {
        fid <-as.character(i)
        if (nchar(fid) == 1 ){
            fid<-paste("00", fid, sep="")
        }
        else if( nchar(fid) == 2) {
            fid<-paste("0", fid, sep="")
        }
        pfile <-( paste(directory,"/", fid,".csv",sep=""))
        dframe <- read.csv(pfile)
        completerows<-complete.cases(dframe)
        if(sum(completerows)>threshold) {
            result<-c(result, cor(dframe[completerows,2], dframe[completerows,3]) )
        }
    }
    return(result)
}

