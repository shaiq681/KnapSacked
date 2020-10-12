
#' Brute Force Knapsack Algorithm
#'
#' The function returns maximum value within all the possible solutions
#'
#' @param x data.frame, contains variables weight(w) and value(v)
#' @param W weight threshold for the knapsack
#' @param ... arguments
#' @param parallel boolean, if 'T' then implements parallelization
#'
#' @return Total and Maximum value within the threshold of W
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack problem}
#'
#' @seealso
#' \code{\link{slow_brute_force}}
#' \code{\link{greedy_knapsack}}
#' \code{\link{knapsack_dynamic}}
#' @export
#'
#' @examples
#'
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = FALSE)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel = FALSE)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000, parallel = FALSE)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000, parallel = FALSE)
brute_force_knapsack <- function(x, W = NULL,..., parallel = FALSE){
  stopifnot( is.data.frame(x),
             is.numeric(W),
             W>0,
             is.numeric(x$w),
             is.numeric(x$v),
             x$w >=0,
             x$v >= 0)
  # get no. of rows
  value <- NULL
  rows <- nrow(x) # Rows of Dataframe
  updated_index <- c()
  row_value <- c()
  cores <- parallel::detectCores() - 1

  # Define Function to Calculate Weight Sum
  sum_weight <- function(column,W,df){
    if (sum(column)<= W & !is.null(sum(column))){
      index <- c(column)
      return(match(index, df[,1]))
    }

  }
  # Function definition ends

  if (parallel ==  T){

    # Start Cluster
    cl <- parallel::makeCluster(cores,type = "PSOCK")

    for (i in 1:rows){

      comb_matrix <- combn(x[,1],m = i)


      true_weight <- parallel::parApply(cl = cl,X = comb_matrix, MARGIN = 2,
                                        FUN = sum_weight,W = W,df=x)


      # Remove Nulls
      true_weight <- true_weight[lengths(true_weight)!=0]

      updated_index <- c(updated_index,true_weight)

    }
    # Stop Cluster
    parallel::stopCluster(cl)
  }
  else{
    for (i in 1:rows){
      comb_matrix <- combn(x[,1],m = i)
      true_weight <- apply(comb_matrix, MARGIN = 2,FUN = sum_weight,W = W,df=x)
      # Remove Nulls
      true_weight <- true_weight[lengths(true_weight)!=0]

      updated_index <- c(updated_index,true_weight)

    }
  }
  # Get Value

  for (i in seq(updated_index)){

    updated_value <-sum(x[updated_index[[i]],2])

    value <- max(updated_value,value)

    if (value == updated_value){
      row_value <- updated_index[[i]]
    }
  }

  result <- list(
    value = round(value),
    elements = row_value
  )

  return(result)

}
