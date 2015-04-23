## Functions that calculate the inverse of a matrix and stores it in the global environment.
##
## Example of a square matrix that can be inverted and used for this as a test
##
## a<-matrix(c(1,1,4,0,3,1,4,4,0),nrow=3,ncol=3)
## a
##      [,1] [,2] [,3]
## [1,]    1    0    4
## [2,]    1    3    4
## [3,]    4    1    0
##
## Invert it:
## solve(a)
##             [,1]        [,2]    [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625
##
## Invert it again to show it's invertable:
## solve(solve(a))
##      [,1] [,2] [,3]
## [1,]    1    0    4
## [2,]    1    3    4
## [3,]    4    1    0
##
## Example of using the same matrix with the cachedmatrix code
##
## a<-matrix(c(1,1,4,0,3,1,4,4,0),nrow=3,ncol=3)
## matrix<-makeCacheMatrix(a)
## cacheSolve(matrix)
## -- sets m to the matrix a as shown above
## cacheSolve(matrix)
## -- retrieves m from the global environment
##
## Actual output of this set of functions from the R console:
##
## a<-matrix(c(1,1,4,0,3,1,4,4,0),nrow=3,ncol=3)
## > source("cachematrix.R")
## > a
##      [,1] [,2] [,3]
## [1,]    1    0    4
## [2,]    1    3    4
## [3,]    4    1    0
## > matrix.nick<-makeCacheMatrix(a)
## > cacheSolve(matrix.nick)
######## Note: here the matrix is cached into the global environment so we DO NOT see the line ######## "getting cached data" before the inverse matrix is displayed
##             [,1]        [,2]    [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625
## > cacheSolve(matrix.nick)
######## Note: here the matrix is retrieved from the global environment so we DO see the line ######## "getting cached data" before the inverse matrix is displayed
## getting cached data
##             [,1]        [,2]    [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625

## makeCacheMatrix is a function that defines 4 additional functions that help manage
## a variable in the global environment that is used to cache the inverse of a matrix
## so that the inverse does not need to be calculated each time it is needed.
makeCacheMatrix <- function(x = matrix()) {

		## set the matrix to null
		m <- NULL
	
		## define the set function
		## by virtue of the <<- operator the variables x and m are searched for
		## in parent environments.  If found they are reassigned.  if not they are
		## created in the global environment. The matrix y is assigned to x in
		## the global environment and m is effectively reset
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
		## define the get function.
		## because <<- was used prior to this definition the reference to x
		## is in the global environment
        get <- function() x
		
		## define the setmatrix function
		## the argument matrix is assigned to m which is in the global environment
		## this is a little confusing because matrix is a function in the base
		## namespace but here it is just a variable
        setmatrix <- function(matrix) m <<- matrix
		
		## define the getmatrix function
		## returns m from the global environment
        getmatrix <- function() m
		
		## return a named list that contains the functions
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
	
}

## cacheSolve is a function that solves (literally, it uses solve(matrix)) a matrix and
## saves the inverse in a variable in the global environment -- effectively caching the
## inverse of the matrix.
cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
		## call getmatrix from the context of x in the global environment
		## note here that m is not the value in the global environment,
		## it's local (I think)
        m <- x$getmatrix()
		
		## if it is defined inform the user that the cached value
		## was referenced and return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		## otherwise get the matrix from x in the global context
        data <- x$get()
		
		## then solve the inverse of x
        m <- solve(data)
		
		## now set the inverse matrix in the global environment
        x$setmatrix(m)
		
		## return m -- note it's the same value as what is in the
		## global environment
        m
}
