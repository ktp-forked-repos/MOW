# Piotr Jastrzebski
# Marcin Nazimek

# Loads a dataset from the file
load.loadFromFile <- function(filename){

	return(read.csv(file=filename, head=FALSE, sep = ",", dec="."))
}