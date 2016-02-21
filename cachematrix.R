## These two functions, together, will compute the inverse of a matrix and then
## cache it in memory. If it is necessary to produce that inverse again, the
## function will returned the cached copy, rather than computing it another time.

## makeCacheMatrix takes as an input a square matrix. Given that square matrix, it
## outputs a list where each element in the list (which can be called via the $
## symbol) is also the result of the sub-function that was run when it was first 
## called. E.g., if you write "test <- makeCacheMatrix(matrix1)" (and if your matrix1
## is a square matrix), then "test" becomes a list, and typing "test$get" will return the 
## value of matrix1 in the console. Typing "test$set(x)" where x is a new matrix will set
## the value of the matrix in it to a new matrix. test$getsolution returns the value "s",
## which is set to be the solution of the stored matrix.

## solvematrix confused me at first. I couldn't figure out how it was solving the matrix.
## What I finally realized is that the actual solving of the matrix occurs in the other
## function, cacheSolve; and solvematrix is then used to pass the result of cacheSolve 
## to this list.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  solvematrix <- function(solved) s <<- solved
  getsolution <- function() s
  list(set = set, get = get, solvematrix = solvematrix, getsolution = getsolution)

}


## cacheSolve can do one of two things. We pass our list from the above function to it
## (which we assigned in the previous comments to a value "test", so I'll be using that
## here, as well). It looks at our list  to see if the value "s" is NULL, through the 
## command "test$getsolution". If s is not NULL, it means that the matrix has already 
## been solved, so the function uses the value of s and "returns" that. Because it 
## uses a "return" command, the function stops at that point; it is done.

## If "s" is NULL, then the conditional statement is never run, so that "return" command
## is never called. In that case, the function creates a new variable, "data," and passes
## the value of "test$get" to it, which is our matrix. It then runs the "solve" function
## on our matrix, and uses "test$solvematrix" to pass that result back into our list "test".
## Then, if this function is called again, the conditional statement in the first half
## will see that s is not NULL, and will return the cached value, instead.

cacheSolve <- function(x, ...) {
  s <- x$getsolution()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$solvematrix(s)
  s
}
