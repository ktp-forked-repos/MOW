# Piotr Jastrzebski
# Marcin Nazimek

library('matlab')

# Value k should be odd!
knn.doIt <- function(k, oneSample, trainSet){

	
	# Subtract on each row of trainSet values of oneSample (class from trainSet must be excluded!)
	tmp = trainSet[,1:(ncol(trainSet)-1)] - repmat(oneSample, c(nrow(trainSet),1))
	
	# Calculate on each value the ,,distance'' (square root ommited)
	values = apply(tmp, 1, ownFunctions.sumsq)
	
	# Got to be like this below, since nested function didn't work :|
	uniqueVal = unique(values)
	
	# Find k smalles values and return their indices
	nSmallestIndices = which(uniqueVal %in% (sort(uniqueVal))[1:k])

	
	# Check if sum of positive values is greater than k/2
	if(sum(trainSet[nSmallestIndices,ncol(trainSet)])>k/2){
		return(1)
	}
	else{
		return(0)
	}
}