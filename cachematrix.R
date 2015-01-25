## Put comments here that give an overall description of what your
## functions do

## A simple fuction that checks is a chached matrix exisits and if it is identical to the matrix
## that was passed as an argument to the function.
## If the matrix is identical to the existing chached matrix it simply returns it 
## otherwise it creates a basic inversable matrix 
## in either case it also send the matrix to the inverse function that inverses it 
## if it was not already inversed and save the result to cache

makeCacheMatrix <- function(x = matrix()) {
         mex<- matrix()      
        if (exists("MCX")){
                if (identical(x,MCX) && !identical(x,mex)) {
                        print("Returning Cached Matrix")
                        MIX<<-solve(MCX)
                        return(MCX)}
                else if (identical(x,mex)){ 
                        print("Creating 100X100 inversable matrix")
                        MCX<<-matrix(rnorm(10000),100,100)
                        MIX<<-solve(MCX)
                        return(MCX)}
        
                }
                
                print("Caching recived Matrix and returning it")
                MCX<<- x
                MIX<<-solve(MCX)
                MCX
}


## this function checkes is there is a cheched matrix and if it is identical to the recived one, if it is identical
## it simply returned the cached inversed matrix if not it inverses and returns the result

cacheSolve <- function(x) {
        if (exists("MCX") && exists("MIX")){
                if(identical(MCX,x)){
                        print("Returning cached inversed matrix")
                        return(MIX)}
        } 
        print("inversing matrix and returning inversed matrix")
        MIX<<-solve(x)
        MIX
}
