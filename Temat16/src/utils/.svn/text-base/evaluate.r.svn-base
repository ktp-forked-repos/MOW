# Piotr Jastrzebski
# Marcin Nazimek



evaluate.evaluate <-function(testDataFrame, trainDataFrame){


	testSet = as.matrix(testDataFrame)
	trainSet = as.matrix(trainDataFrame)
	
	##### OUR ALGORITHMS #####
		# [TP|FP|FN|TN]
		# http://www.eol.ucar.edu/rsf/NEXRAD/dq_fy98/fig_2.4.gif
	
	   # kNN classification test

		result  <- rep(NA, nrow(testSet))

		expected = testSet[, ncol(testSet)]
	   for(number in 1:nrow(testSet)){
			result[number]  = knn.doIt(3, testSet[number,1:(ncol(testSet)-1)], trainSet)
	   }
	   print("kNN n=3 result table [TF|FN]")
	   print("                     [FP|TP]")
	   resultMatrix <-table(result,expected)
	   print(resultMatrix)
	   
	   # kNN classification test
	   
	   result  <- rep(NA, nrow(testSet))
	   
	   expected = testSet[, ncol(testSet)]
	   for(number in 1:nrow(testSet)){
		   result[number]  = knn.doIt(5, testSet[number,1:(ncol(testSet)-1)], trainSet)
	   }
	   print("kNN n=5 result table [TF|FN]")
	   print("                     [FP|TP]")
	   resultMatrix <-table(result,expected)
	   print(resultMatrix)
	   
	   
	   # kNN classification test
	   
		result  <- rep(NA, nrow(testSet))
		expected = testSet[, ncol(testSet)]
		
	   for(number in 1:nrow(testSet)){
		   result[number]  = knn.doIt(7, testSet[number,1:(ncol(testSet)-1)], trainSet)
	   }
	   print("kNN n=7 result table [TF|FN]")
	   print("                     [FP|TP]")
	   resultMatrix <-table(result,expected)
	   print(resultMatrix)
	   
	   
	   
	   # Naive Bayes classification test
	   
	   result  <- rep(NA, nrow(testSet))
	   expected = testSet[, ncol(testSet)]
	   
	   for(number in 1:nrow(testSet)){
		   result[number]  = bayes.doIt(testSet[number,1:(ncol(testSet)-1)], trainSet)
	   }
	   print("Naive Bayes result table [TF|FN]")
	   print("                         [FP|TP]")
	   resultMatrix <-table(result,expected)
	   print(resultMatrix)
	 
	   
	
	##### R LANGUAGE ALGORITHMS #####
	
	# Random Forest classification test
   	
	result  <- rep(NA, nrow(testSet))
	expected = testSet[, ncol(testSet)]
	r <- forest.makeForest(trainDataFrame)
	for(number in 1:nrow(testSet)){
		result[number]  = predict(r, testDataFrame[number,1:ncol(testDataFrame)-1])
	}
	print("[R]   Random Forest result table [TF|FN]")
	print("                                 [FP|TP]")
	resultMatrix <-table(result-1,expected)
	print(resultMatrix)

	
	
	# Tree Classification

	result  <- rep(NA, nrow(testSet))
	expected = testSet[, ncol(testSet)]
	tree <- tree.makeTree(trainDataFrame)
	for(number in 1:nrow(testSet)){
		result[number]  = predict(tree, testDataFrame[number,1:ncol(testDataFrame)-1], type = "class")
	}
	print("Tree Classification result table [TF|FN]")
	print("                                 [FP|TP]")
	resultMatrix <-table(result-1,expected)
	print(resultMatrix)

	

	# Bayes from R Classification

	result  <- rep(NA, nrow(testSet))
	expected = testSet[, ncol(testSet)]
	classifier <- bayesFromR.makeClassifier(trainDataFrame)
	for(number in 1:nrow(testSet)){
		result[number]  = predict(classifier, testDataFrame[number,1:ncol(testDataFrame)-1])
	}
	print("[R] Bayes classification result table [TF|FN]")
	print("                                      [FP|TP]")
	resultMatrix <-table(result,expected)
	print(resultMatrix)

	

	# Knn n = 3from R Classification

	print("[R] kNN n=3 result table [TF|FN]")
	print("                         [FP|TP]")
	print(knnFromR.doIt(trainDataFrame,testDataFrame,3))
	

	# Knn n = 5 from R Classification

	print("[R] kNN n=5 result table [TF|FN]")
	print("                         [FP|TP]")
	print(knnFromR.doIt(trainDataFrame,testDataFrame,5))
	
	
	# Knn n = 7 from R Classification

	print("[R] kNN n=7 result table [TF|FN]")
	print("                         [FP|TP]")
	print(knnFromR.doIt(trainDataFrame,testDataFrame,7))
	
	
	###############################################################
		

}