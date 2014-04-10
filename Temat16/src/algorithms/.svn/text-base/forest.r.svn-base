# Piotr Jastrzebski
# Marcin Nazimek

forest.makeForest <- function(spamTrain){
	
	library(randomForest)
	
	#spamTrain <- read.csv('data/trainSet.txt', head=FALSE, sep = ",", dec=".")
	
	names(spamTrain)[ncol(spamTrain)]<-c("class") 
	
	fdata = factor(spamTrain$class)
	spamTrain$class  <- fdata
	
	r = randomForest(class~., data=spamTrain, importance=TRUE, do.trace=FALSE)	
	

	
	return(r)
}
