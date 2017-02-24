complete <- function(directory, id=1:332){
	df <- data.frame()
	for (i in id) {
		fn <- if (i < 10 ) { 
			paste("00",i,".csv", sep="")
		}else if ( i < 100) {
			paste("0",i,".csv",sep = "")
		} else {
			paste(i,".csv",sep="")
		}
		dat <- read.csv(file.path(directory, fn))
		lidx <- !is.na(dat$sulfate) & !is.na(dat$nitrate)
		df <- rbind(df, c(i,sum(lidx)))
	}
	colnames(df) <- c("id","nobs")
	rownames(df) <- NULL
	df
}