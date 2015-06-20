makeCacheMatrix<-function(x=matrix())
{
    #set inversematrx as NULL
    inversematrix<-NULL
   
   #set initial
   set<-function(y)
   {
       x<<-y
       inversematrix<<-NULL
   }
   get<-function() x
   setinverse<-function(inverse) inversematrix<<-inverse
   getinverse<-function() inversematrix
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
cacheSolve<-function(x,...)
{
    #get inversematrix
    inversematrix<-x$getinverse()
    #judge whether inversematrix is NULL
    if(!is.null(inversematrix))
    {
        message("getting cached data")
        return(inversematrix)
    }
    #solve for inversematrix
    matrix<-x$get()
    inverse<-solve(matrix)
    x$setinverse<-inverse
    inverse
}
