#数据前处理及命令函数，返回一个包含四个函数的列表
makeCacheMatrix <- function(x = matrix()) {
  #x为输入数据，m为结果输出
  m <- NULL
  #set函数起赋值作用，“<<-”为全局赋值语句
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get函数返回原始数据，setsolve函数讲输入变量传递给最终变量m
  #getsolve函数返回当前结果m的值
  get <- function() x
  setsolve <- function(solvea) m <<- solvea
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#数据处理函数，需调用makeCachMatrix函数
cacheSolve <- function(x, ...) {
  #x为makeCachMatrix处理结果
  #载入makeCachMatrix处理结果，判断m是否有被赋值记录
  #如有，提示。如未，计算。
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
#以下注释为试运行命令
#numbers = matrix(c(1,2,1,2,3,4,5,1,2,3,4,1,2,8,1,5),nrow=4,ncol=4)
#cachedNumbers = makeCacheMatrix(numbers)
#cacheSolve(cachedNumbers)
#matrix(numbers)
