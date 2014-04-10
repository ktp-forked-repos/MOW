# Piotr Jastrzebski
# Marcin Nazimek

tree.makeTree <- function(spamTrain){

	
	library(rpart)
	
	#spamTrain <- read.csv('data/trainSet.txt', head=FALSE, sep = ",", dec=".")
	
	names(spamTrain)[ncol(spamTrain)]<-c("class") 
	tree = rpart(class ~.,method="class", data = spamTrain)	
	

	
	return(tree)

}

