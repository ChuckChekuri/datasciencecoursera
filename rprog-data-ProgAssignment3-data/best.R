best = function(state, outcome) {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	states <- unique(data$State)
	if (!state %in% states) stop("invalid state")
	outcomes <- c("heart attack", "heart failure", "pneumonia")
	if (!outcome %in% outcomes ) stop("invalid outcome")

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
	# get the hospital names with min 
	h <- df[df[,2] == min(df[,2]),1]
	# Order and return the first hospital
	h[order(h[1])][1]
}