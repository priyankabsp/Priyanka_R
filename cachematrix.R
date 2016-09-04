## This function gives us the inverse of a matrix. 
## We are using cache memory spacing to provide the calculated inverse.
## Ones the inverse if been calculated and if 
## there is no change in the input matrix, this function will not 
## calculate the inverse again and returns the already calculated inverse.

## Function makeCacheMatrix calculates the inverse of a matrix stores the result in cache.

makeCacheMatrix <- function(x = matrix()) {

		## initialising the inverse of x (inverse_val) as NULL
		inverse_val<- NULL

		## Assigning y as  x
		set <- function (y){
			x<<-y
			inverse_val<-NULL
		}
		## Calling function get for setting the value of x
		get<-function () x
		
		## Calculating inverse of matrix x
		setinverse<- function(inverse) inverse_val<<-inverse
		
		## Asssigning inverse of matrix x
		getinverse <- function() inverse_val
		
		##Creating list for all the functions
		list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## In this function we will be passing the value of x and 
## to calculate inverse of x, we will be calling makeCacheMatrix funciton 
cacheSolve <- function(x, ...) {
        
		## Looks for the cache value from makeCacheMatrix function
		inverse_val<-x$getinverse()
		
		## checks if the inverse for x is been calculated earlier or not
	     if (!is_null(inverse_val)){
		 ## if inverse already been created, returns the cache stored value
		 	message("getting cached data")
		 	return (inverse_val)
		 }
		 
		 ## assigning the value
		 data<- x$get()
		 
		 ## Calculating the inverse of x, if not calculated earlier
		 inverse_val<- solve(data, ...)
		 
		 ## Storing the value of x in cache
		 x$setinverse(inverse_val)
		 
		 ## Returns the inverse value
		 inverse_val
}
