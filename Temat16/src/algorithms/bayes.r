# Piotr Jastrzebski
# Marcin Nazimek

# Naive Bayes classifier
# Written according to this: http://en.wikipedia.org/wiki/Naive_Bayes_classifier#Sex_classification
bayes.doIt <- function(oneSample, trainSet){

	
	# Divide the trainSet into spam and notSpam
	spam  = subset(trainSet, trainSet[,ncol(trainSet)]==1)
	notSpam = subset(trainSet, trainSet[,ncol(trainSet)]==0)

	# Calculate the mean value and standard deviation for spam and notSpam
	meanSpam = colMeans(spam[,1:(ncol(spam)-1)])
	varSpam = apply(spam[,1:(ncol(spam)-1)],2,var)

	meanNotSpam = colMeans(notSpam[,1:(ncol(notSpam)-1)])
	varNotSpam = apply(notSpam[,1:(ncol(notSpam)-1)],2,var)
	
	# Prepare vectors for the conditional probabilities 
	spamPred = vector(mode = "numeric", length = length(oneSample))
	notSpamPred = vector(mode = "numeric", length = length(oneSample))
	
	# For each feature calculate the conditional probabilities 
	for(number in 1:length(oneSample)){
		spamPred[number] = (exp(-((oneSample[number]-meanSpam[number])^2)/(2*varSpam[number]))/sqrt(2*pi*varSpam[number]))
		notSpamPred[number] = (exp(-((oneSample[number]-meanNotSpam[number])^2)/(2*varNotSpam[number]))/sqrt(2*pi*varNotSpam[number]))			
	}

	# Caluclate the product of probability for spam and not spam 
	spamProd = prod(spamPred)
	notSpamProd = prod(notSpamPred)
	

	if(spamProd>notSpamProd){
		return(1)
	}
	else{
		return(0)
	}
}