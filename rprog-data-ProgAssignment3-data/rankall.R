rankall = function(outcome, num = "best") {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	outcomes <- c("heart attack", "heart failure", "pneumonia")
	if (!outcome %in% outcomes ) stop("invalid outcome")
	if (num == "best")  num <- 1 
	if (suppressWarnings(is.na(as.numeric(num))))
		if (num != "worst") stop("invalid rank")
	# get the column number based on passed in outcome
	# columns for the mortality rates in the same order as outcomes
	m_cols <- c(11,17,23)
	outcol <-m_cols[which(outcomes == outcome)]
	df <- split(data[,c(2,7,outcol)], data$State)
	# convert the character to number
	h <- lapply(df, function(y) {
	     x <- as.data.frame(y)
	     x[,3] <- suppressWarnings(as.numeric(x[,3]))
	     # remove the NAs
	     x <- na.omit(x)
	     # order the  hospitals by mortality rate
	     x <- x[order(x[,3],x[,1]),]
	     if (num == "worst") num <- nrow(x)
	     h <- if (num > nrow(x) ) NA else x[num,1]
	})
	hospital <- unlist(h)
	state <- names(df)
	as.data.frame(cbind(hospital, state))
}

