makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x,...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}


# Example Usage:

# Create a sample matrix
matrix_data <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

# Create the special matrix object
my_matrix <- makeCacheMatrix(matrix_data)

# Calculate the inverse for the first time
inverse_matrix <- cacheSolve(my_matrix)
print("Inverse (first calculation):")
print(inverse_matrix)

# Calculate the inverse again (should retrieve from cache)
inverse_matrix_cached <- cacheSolve(my_matrix)
print("Inverse (cached):")
print(inverse_matrix_cached)


# Change the matrix and recalculate

new_matrix_data <- matrix(c(5,6,7,8), nrow = 2, ncol = 2)
my_matrix$set(new_matrix_data) # set with new matrix data
inverse_matrix_new <- cacheSolve(my_matrix) #recalculating
print("New Inverse:")
print(inverse_matrix_new)

inverse_matrix_new_cached <- cacheSolve(my_matrix) #retrieving from cache
print("New Inverse (cached):")
print(inverse_matrix_new_cached)