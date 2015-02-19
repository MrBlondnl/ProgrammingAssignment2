# This function returns a list of 4 functions with which the a matrix or the inverse of a matrix can be set to cache and be retrieved
# 1. the Set function allows you to set a matrix to the cache
# 2. the Get function allows you go get the matrix from the cache
# 3. the Setinverse function allows you to set the inverse of the matrix to the cache
# 4. the Getinverse function allows you to retreive the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the mean to NULL during the first call to makeCacheMAtrix
  # this is needed because ig getmean() is called immediately after the makeCacheMatrix function is constructed, without a call to setinverse
  # we know we must first calculate the inverse in cacheSolve.
    
  i <- NULL    
  
  set <- function(y) {      # function to set a new value for the underlying matrix this invalidates the cached inverse,                      
    
    x <<- y                 # we use the <<- operator to set the value of x and i because we want to modify x and i defined in the enclosing environment (created when makeCacheMatrix was first called), not in the environment local to set(), in which x and i are undefined.
    i <<- NULL              # we must reset i to NULL since we are modifying the underlying matrix and the cached value is no longer the valid 
  }
  get <- function() x                           #  This is a function to use if you want to retreive the matrix from the list.
  setinverse <- function(inverse) i <<- inverse # set the inverse of the matrix x.  Called by cacheSolve, # use the <<- operator because we want to modify the i defined in the enclosing function makeCacheMatrix, not the i local to setinverse, which would be undefined.
  getinverse <- function() i                    # returns the inverse.  Will be null if setinverse has not been called or if set is called after the last call to setinverse
  
  # List below returns value of the makeCacheMatrix functions as a list
  # of functions that we want to expose as public.  these are accessed with the $ operator.  Any variables
  # declared inside makeCacheMatrix but not exported as part of this list
  # are private...they are inaccessible to any caller of MakeCacheMatrix
  
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#this function checks wether the inverse (i) of a matrix has already been cached. If so it returns it from cache, if not, it will calculate it and set it to the cache. 
# the MakeCacheMatrix has to have executed in order to get this function to work as it uses functions from the list defined there.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # here the inverse of the matrix is retreived from Cache
  # if it is not Null it will use the cached value, and tell you so by showing the message. If it is Null it will be passed to the second part of the function for further processing.
  
  i <- x$getinverse() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()       # in this second part the matrix is being loaded in from cache
  i <- solve(data, ...) # afterwards the inverse of the matrix is calculated in this line
  x$setinverse(i)       # and finally the inverse matrix will be sent to cache using this function.
  i                     # the inverse of the matrix is shown as the output of the function.
}