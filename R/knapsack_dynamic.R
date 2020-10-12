
#' Dynamic Implementation of Knapsack Algorithm
#'
#' @param x data.frame, contains variables weight(w) and value(v)
#' @param W weight threshold for the knapsack
#'
#'
#' @return Total and Maximum value within the threshold of W
#'
#' @seealso
#' \code{\link{brute_force_knapsack}}
#' \code{\link{greedy_knapsack}}
#' @export
#'
#' @examples
#' knapsack_dynamic(x = knapsack_objects[1:100,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:500,], W = 2000)
knapsack_dynamic<-function(x,W){
  stopifnot( is.data.frame(x),
             is.numeric(W),
             W>0,
             is.numeric(x$w),
             is.numeric(x$v),
             x$w >=0,
             x$v >= 0)

  w<-c(0,x[,1])
  v<-c(0,x[,2])
  n<-nrow(x)
  mat<- matrix(0,nrow=n+1,ncol=W+1)
  mat[1,]<-0
  for (i in 2:n+1){
    for (j in 0 :W+1){
      if (x$w[i-1]>j ){
        mat[i,j]<-mat[i-1,j]
      }else{
        mat[i,j]<-max(mat[i-1, j], mat[i-1, j-w[i]] + v[i])
      }
    }
  }
  i <- (n+1)
  m <- max(mat[i,])
  value <- m
  elm <- c()
  j <- 1
  while(i > 1) {
    if(value %in% mat[i-1,])
    {
      i <- i - 1
    } else
    {
      elm[j] <- i-1
      j <- j + 1
      i <- i - 1
      value <- value - x$v[i]
    }
  }

  result<-list(value=round(mat[n+1,W+1]),
               elements=as.numeric(sort(elm)))
  return(result)
}
