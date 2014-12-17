## Coursera R Programming course
## Programming assignment 2
## goal it so write 2 functions
## function 1: input is a matrix and output is a list containing functions and a matrix.  Function 1 creates functions to store and retrieve a matrix  
##              from the global environment (via the <<- operator)
## function 2: input is a matrix and output is the inverse of the matrix, where the inverse is computed if it is not already stored in a 
##              global environment variable



## takes matrix to be inverted as input, returns list of  3 functions (to compute the inverse, to set the inverse to a global parameter, and to retrieve
## the inverse from the global parameter) and 1 matrix (the inverse of the input)

makeCacheMatrix <- function(x = matrix()) 
  {
  #initialize the inverse
  inv <- NULL
  #function to initialize the global environment variable inv (and the matrix to be inverted)
  set<-function(y)
  { ##IF the inverse doesn't exist, the driver function (cacheSolve) will use set to store the values in global environment
    x<<-y
    inv<<-NULL
  }
  setInv<-function(inverse) inv<<-inverse ##This function stores the computed inverse in the already initialized inv variable in the global env.
  get<-function(){x} #this function returns the matrix to be inverted from global environment
  getInv<-function(){inv} #this function returns the inverse that has be computed from the global environment
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Now that we have a function makeCacheMatrix to put the output where it belongs, the funciton below will use it (with the assigned global var names
## from makeCacheMatrix to) store the computed inverse of a matrix (if it's not computed already) or retrieve the inverse of the matrix
## per the assignment description, not checks for invertibility are performed
## arg x is not a matrix, it is the list of functions plus matrix created by makecacheMatrix
cacheSolve <- function(x, ...) {
       
        
  
        ## before computing the inverse, check if inverse is saved in global environment (via get function in makeCacheMatrix)
       inv<-x$getInv()
       if (is.null(inv))
       {
         matrix<-x$get()
         neg1<-solve(matrix)
         x$setInv(neg1)
         neg1
       }
       ##really should check if constructed matrix is the same as the matrix used for cacheing or not . . .
       ## but not requested in the assignment
       else {
             message ("getting cached data")
             return(inv)
       }
}
