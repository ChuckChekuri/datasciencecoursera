corr <- function(directory, threshold=0) {
	cr <- NULL
	nf = length(dir(directory))
	completeobs <- complete(directory)
	ids <- completeobs[completeobs$nobs > threshold,"id"]
	for (i in ids) {
		fn <- if (i < 10 ) { 
			paste("00",i,".csv", sep="")
		} else if ( i < 100) {
			paste("0",i,".csv",sep = "")
		} else {
			paste(i,".csv",sep="")
		}
		dat <- read.csv(file.path(directory, fn))
		#if (first == TRUE)  {
		cr <- c ( cr, cor(x=dat$sulfate, y=dat$nitrate, use="complete.obs"))
		#	first = FALSE
		#} else {
		#	cr <- rbind(cr, cor(x=dat$sulfate, y=dat$nitrate, use="complete.obs"))
		# }
	}
	cr
}

