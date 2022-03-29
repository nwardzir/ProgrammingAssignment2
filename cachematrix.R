## These functions manipulates 'special matrix' objects.
## These functions are part of 

## makeCacheMatrix allows to create a special matrix object that contains a matrix and its inverse
## More precisely, it returns a list with 4 embedded methods:
##    set allows to set a new matrix. It resets the inverse to NULL. Arguments are
##      matrice : class matrix
##    get allows to retrieve the matrix (but not the inverse). No argument.
##    set_inverse allows to update the inverse of the matrix. It doesn't change the matrix. Arguments are:
##      a_matrix : class matrix
##    get_inverse allows to retrieve the inverse of the matrix (but not the matrix itself). No argument.

makeCacheMatrix <- function(matrice = matrix()) {
  inverse <- NULL
  set <- function(y = matrix()) {
    matrice <<- y
    inverse <<- NULL
  }
  get <- function () matrice
  set_inverse <- function (a_matrix = matrix()) {
    inverse <<- a_matrix
  }
  get_inverse <- function () inverse 
  list (set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## cacheSolve inverses matrices using the 'special_matrix' object. 
## It checks in the cache if there is already an inverse matrix. If not, it calculates it and cache it in the 'special_matrix' object
##    Arguments to this function are the same than for the 'solve' function, except for the first argument that is a 'special matrix' object

cacheSolve <- function(special_matrix, ...) {
  inverse <- special_matrix$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrice <- special_matrix$get()
  new_inverse <- solve (matrice, ...)
  special_matrix$set_inverse(new_inverse)
  new_inverse
}
