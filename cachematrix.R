
# # a list of 4 function :
# get the value of the matrix
# set the value of the matrix
# set the value of the inverse
# get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # initialize inv as NULL
        set <- function(y) {  # function1: set y as the new x in the Parent ENV., and reset inverse as NULL in the Parent Env. 
                x <<- y
                inv <<- NULL
        }
        get <- function() x  # funtion2: print the matrix
        setinverse <- function(z) inv <<- z  #function3: set the inverse value to m in makeCacheMatrix Env. (z is just an intermedia value)
        getinverse <- function() inv  # function4: print the inverse 
        list( set = set, 
              get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## calculate and cachethe inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()   # set and test if inverse is exist
        if(!is.null(inv)) {
                message("getting cached Inverse")
                return(inv)
        }
        data <- x$get()  # get x by assigning to "data"
        inv <- solve(data, ...)  # get the inverse of x
        x$setinverse(inv)  #cache the inverse into makeCacheMatrix
        inv
}





#####################################
#####  example 
# 
# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }
# 
# 
# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }
# 
# 
# ##########################
# ### unit test
# # 
# # 
# # 
# amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# amatrix$get()         # Returns original matrix
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# 
# cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# amatrix$getinverse()  # Returns matrix inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# [,1] [,2]
# [1,] -0.13333333  0.2
# [2,]  0.01010101  0.0
# 
#   amatrix$get()         # Returns matrix
# [,1] [,2]
# [1,]    0   99
# [2,]    5   66
# 
#   amatrix$getinverse()  # Returns matrix inverse
# [,1] [,2]
# [1,] -0.13333333  0.2
# [2,]  0.01010101  0.0
# 
