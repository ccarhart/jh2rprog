pollutantmean<- function(directory, pollutant, id = 1:332L) {
 
    if(pollutant == 'sulfate') {
        col = 2
    }
    else if (pollutant == 'nitrate') {
        col = 3
    }
    else {
        stop("Pollutant must be sulfate or nitrate. Good Bye")
    }
    
    for(i in id){
        fid <-as.character(i)
        if (nchar(fid) == 1 ){
            fid<-paste("00", fid, sep="")
        }
        else if( nchar(fid) == 2) {
            fid<-paste("0", fid, sep="")
        }
        
        pfile <-( paste(directory,"/",fid,".csv",sep=""))
        if ( !exists("dframe") ){
            dframe <- read.csv(pfile)
        }
        else {
            tmp_dframe <-read.csv(pfile)
            dframe<-rbind(dframe, tmp_dframe)
            rm(tmp_dframe)
        }
    } 
    
    bad <-is.na(dframe[,col])
    return( mean(dframe[!bad,col]))

}