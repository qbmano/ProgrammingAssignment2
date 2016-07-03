##Overall, these two functions in unison cache a value for a solved/inverted matrix and are then able
##to call this value. Also, if the value was not set in the first function, the second function can set
##it using functions declared in the first function. Finally, these functions use R's lexical scoping
##to set the variable m (using the <<- operator) from the second function, thereby changing it in the
##first function. These functions demonstrate the usefulness of understanding scoping rules in R.

##This function sets up m for the next function so it can test if the matrix has already been solved.
##The functions declared in this function are used to solve the matrix and retrieve the (solved) value
##in the next function. The list at the end makes it so the functions can be called with
##x$function_name in the next function.
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolved <- function(solve) m <<- solve
        getsolved <- function() m
        list(set = set, get = get, setsolved = setsolved, getsolved = getsolved)
}

##This function first test if the matrix (x) has already been solved. If so, it returns it. If not, it
##solves it using functions declared in makeCacheMatrix, and then returns it.
cacheSolve <- function(x, ...)
{
        m <- x$getsolved
        if (!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolved(m)
        m
}
