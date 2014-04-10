# Piotr Jastrzebski
# Marcin Nazimek


source('src/utils/load.r')


main.main <- function(){
	

	print('START')
	
	# Loads train and test set into data frames
	trainSet <- load.loadFromFile('data/trainSet.txt')
	testSet <- load.loadFromFile('data/testSet.txt')
	
	#evaluate.evaluate(testSet, trainSet)
	
	install.packages("classifiersComparator_2.1.tar.gz", repos=NULL,type="src")
	library('classifiersComparator')
	classifiersComparator(trainSet,testSet)
	
	print('STOP')
}
