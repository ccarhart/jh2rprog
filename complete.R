complete <- function(directory, id = 1:332) {
    for(i in id){
        fid <-as.character(i)
        if (nchar(fid) == 1 ){
            fid<-paste("00", fid, sep="")
        }
        else if( nchar(fid) == 2) {
            fid<-paste("0", fid, sep="")
        }
        
        pfile <-( paste(directory,"/", fid,".csv",sep=""))
        dframe <- read.csv(pfile)
        if (!exists("results")){
            results<-data.frame(id=dframe[1,4],nobs=sum(complete.cases(dframe))) 
        }
        else {
            results<-rbind(results, c(dframe[1,4],sum(complete.cases(dframe)) ))
        }
    }
    return( results) 
}