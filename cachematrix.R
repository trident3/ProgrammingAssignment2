#####################################################################################
# Programming Assignment 2: Lexical Scoping from coursera R programming course.
# 
#
# function makeCacheMatrix(x = matrix)
# A special "CacheMatrix" is created. This is used for computing the inverse of a matrix.
# The inverse is cached back into the "CacheMatrix" so that subsequently the inverse does not have to
# be recalculated, instead the cached result can be used.
#
# function cacheSolve(x)
# Return the inverse of a matrix, caching the result so that if cacheSolve(x) is called again then
# the cached result is returned instead of recalculating the inverse.
#
# functinon testMatrixCacheSolve()
# Testing that cacheSolve and makeCacheMatrix work
#####################################################################################


#####################################################################################
# function makeCacheMatrix(x = matrix)
# A special "Matrix" is created. This matrix consists of a list of functions which are:
# set : Set the value of the matrix,
# get : Get the value of the matrix,
# getinvm : Return a cached value which is the inverse of the matrix,
# setinvm : Sets the cached value to be the invese of the matrix.
# Note that there is no verification that calling setinvm actually sets the cached value to be the inverse.
#####################################################################################
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   
   setinvm <- function(m) m <<- m
   getinvm <- function() m
   
   list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
}

#####################################################################################
# function cacheSolve(x)
# Return the inverse of a matrix, caching the result so that if cacheSolve(x) is called again then
# the cached result is returned instead of recalculating the inverse.
#
# The parameter 'x' was previously created using the makeCacheMatrix function.
# Example: 
# cacheM = makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
# InvOfcacheM = cacheSolve(cacheM)  # This will calculate the inverse of cacheM, cache the result and return it.
# InvOfcacheM = cacheSolve(cacheM)  # This will return the cached result.
#####################################################################################
cacheSolve <- function(x, ...) {
   m <- x$getinvm()
   if (!is.null(m)) {
      message("Getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data)
   x$setinvm(m)
   m
}

testMatrixCacheSolve <- function() {
   # Testing that cacheSolve and makeCacheMatrix work
   # First: Create a matrix m1 and determine it's inverse solvedc using solve(). Verify that the inverse, i.e.
   # the matrix product of m1 and solvedm1 produce the identity matrix.
   print("Creating matrix m1")
   m1 = rbind(c(1, -1/4), c(-1/4, 1))
   print(m1)
   print("Calculating inverse of m1")
   solvedm1 = solve(m1)
   print("Inverse of m1 is")
   print(solvedm1)
   print("Verifying the inverse of m1")
   print(solvedm1 %*% m1)
   # Now create a special "Matrix", cacheMat.
   # Solve it using the cacheSolve function.
   # The first time cacheSolve() is called, it will use solve() and cache the result.
   # On second and subsequent calls to cacheSolve, the precalculated inverse should be returned.
   print("Testing matrix caching functions")
   print("Creating special Matrix cacheMat")
   cacheMat = makeCacheMatrix(m1)
   print("Calculating inverse of cacheMat")
   solvedm2 = cacheSolve(cacheMat)
   print(solvedm2)
   print("Verifying the inverse of cacheMat. The identity matrix should be returned.")
   print(solvedm2 %*% m1)
   
   print("Calculating the inverse of cacheMat again. This should return a cached result")
   solvedm3 = cacheSolve(cacheMat)
   print("Verifying that the cached result is valid. The identity matrix should be returned")
   print(solvedm3 %*% c)
   
   print("Calculating the inverse of cacheMat again. This should return a cached result")
   solvedm4 = cacheSolve(cacheMat)#
   print("Verifying that the cached result is valid. The identity matrix should be returned")
   print(solvedm4 %*% c)
}
