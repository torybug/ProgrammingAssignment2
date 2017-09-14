
InverseFunction <- function(m) {
	InverseMatrix <- data.frame()
	InverseMatrix <- lapply(m,rbind(InverseMatrix,m[,i])
return(InverseMatrix)
}

NewFunction <-function(x){
	z<<- x +7
	return(z)
}

makeCacheMatrix <- function(x=numeric()){
	inv <- NULL
	set <- function(y){
		x <<-y
		inv <<-NULL
	}
	get <- function() x
	setinverse <-function(inverse) inv<<-inverse
	getinverse <-function() inv
	list(set=set,get=get,
		setinverse=setinverse,getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        	inv <- data.frame()
		inv <- lapply(inv,rbind(inv,data[,i]))
        x$setinverse(inv)
        inv
}