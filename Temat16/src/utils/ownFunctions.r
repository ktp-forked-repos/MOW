# Piotr Jastrzebski
# Marcin Nazimek

#Like matlab sumsq
ownFunctions.sumsq <- function(x){
	sum(x*x)
} 

#Like matlab repmat
ownFunctions.repmat = function(X,m,n){
	mx = dim(X)[1]
	nx = dim(X)[2]
	matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}