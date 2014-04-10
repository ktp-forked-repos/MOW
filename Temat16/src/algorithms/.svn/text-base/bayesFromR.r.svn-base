# Piotr Jastrzebski
# Marcin Nazimek

library('e1071')

# Naive Bayes classifier
# Implemented in R package
 bayesFromR.makeClassifier <- function(spamTrain){
 
 
     names(spamTrain)[ncol(spamTrain)]<-c("class") 
 
     fdata = factor(spamTrain$class)
     spamTrain$class  <- fdata
 
     m <- naiveBayes(class ~ ., spamTrain)

	 
     return(m)
 }

