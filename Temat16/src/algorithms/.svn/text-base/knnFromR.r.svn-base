# Piotr Jastrzebski
# Marcin Nazimek


library('e1071')

knnFromR.doIt <- function(spamTrain,spamTest,n){

	 
	 names(spamTrain)[ncol(spamTrain)]<-c("class") 
	 names(spamTest)[ncol(spamTest)]<-c("class") 	

	 library('DMwR')
	 
	 ## A 3-nearest neighbours model with no normalization
	 result <- kNN(class ~ .,spamTrain,spamTest,norm=FALSE,k=n)
	 
	 ## The resulting confusion matrix
	 t <-table(spamTest[,'class'],result)
	 
	 return(t)

 }

