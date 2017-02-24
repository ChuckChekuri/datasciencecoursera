
rankhospital = function(state, outcome, rank) {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	states <- unique(data$State)
	if (!state %in% states) stop("invalid state")
	outcomes <- c("heart attack", "heart failure", "pneumonia")
	if (!outcome %in% outcomes ) stop("invalid outcome")
	# Hopital.Name - col 2
	
	# get the column number based on passed in outcome
	# columns for the mortality rates in the same order as outcomes
	m_cols <- c(11,17,23)
	outcol <-m_cols[which(outcomes == outcome)]
	# subset the data by state and select hospital and metric
	df <- data[data$State == state, c(2,outcol)]
	# convert the character to number
	df[,2] <- suppressWarnings(as.numeric(df[,2]))
	# remove the NAs
	df <- na.omit(df)
	# order the  hospitals by mortality rate
	df <- df[order(df[,2],df[,1]),]
	# return Hospital Name
	if (rank == "best")  rank <- 1
	if (rank == "worst") rank <- nrow(df)
	if (suppressWarnings(is.na(as.numeric(rank))))
		stop("invalid rank")
	if (rank > nrow(df) ) NA else df[rank,1]
}

