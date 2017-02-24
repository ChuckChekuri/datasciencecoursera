pollutantmean <- function(directory, polutant, id=1:332){
	first = TRUE
	for (i in id) {
		fn <- if (i < 10 ) { 
			paste("00",i,".csv", sep="")
		      }else if ( i < 100) {
			paste("0",i,".csv",sep = "")
		      } else {
			paste(i,".csv",sep="")
		      }
		if (first) {
			dat <- read.csv(file.path(directory, fn))
			first <- FALSE
		} else {
			dat <- rbind(dat, read.csv(file.path(directory, fn)))
		}
	}
	mean(dat[!is.na(dat[[polutant]]),polutant])
}
