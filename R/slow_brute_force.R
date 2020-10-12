
#' Basic and slow implementation of Brute-Force Algorithm
#'
#' The function returns maximum value within all the possible solutions
#'
#' @param x data.frame, contains variables weight(w) and value(v)
#' @param W weight threshold for the knapsack
#'
#' @return Total and Maximum value within the threshold of W
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack problem}
#'
#' @seealso
#' \code{\link{brute_force_knapsack}}
#'
#' @importFrom utils combn
#' @export
#'
#' @examples
#'
#' slow_brute_force(x = knapsack_objects[1:8,], W = 3500)
#' slow_brute_force(x = knapsack_objects[1:12,], W = 3500)
slow_brute_force <- function(x,W){

  stopifnot( is.data.frame(x),
             is.numeric(W),
             W>0,
             is.numeric(x$w),
             is.numeric(x$v),
             x$w >=0,
             x$v >= 0)

  rows <- nrow(x)
  max_value_row_index <- c()

  create_combination <- function(x,i){
    comb_matrix<- combn(as.integer(rownames(x[,])), m=i) # Generate the index number combination


    return(comb_matrix)
  }

  sum_vector<- function(l, W, df){

    total_wt <-sum(df[c(l),1]) # Wt of combination
    total_value <- sum(df[c(l),2]) # Value of Combination
    if ((total_wt <= W) & (!is.na(total_wt)))  {
      return(total_value)
    }

    else{
      return(0)
    }

  }

  max_value_and_index <- function(i,W,x){

    combo_matrix <- create_combination(x = x,i =i) # Generate the combination matrix
    m<-apply(combo_matrix, MARGIN = 2, FUN = sum_vector, W = W, df = x)

    if (max(m) == 0){

      invisible() # When dont need anything to be returned.
    }
    else{

      m <- which.max(m)

      max_value_index <-combo_matrix[,m] # Maximum value index

      max_combo_value <- sum(x[combo_matrix[,m],2])

      if ( !(is.null(max_combo_value))){

        result <- list(  max_combo_value,as.vector(max_value_index))

      }
      else{
        invisible()
      }


    }

  }


  comb_vals <-lapply(X = 1:rows, FUN = max_value_and_index, x=x,W=W)

  comb_vals <- unlist(x = comb_vals, use.names = T, recursive = F)

  value <- seq(from=1, to = length(comb_vals), by = 2)
  weights <- seq(from = 2, to = length(comb_vals), by = 2)

  max_val <- max(unlist(comb_vals[value]))

  weight_index <- which(max_val == comb_vals[value]) *2

  all_weights <- unlist(comb_vals[weight_index])



  return(
    list(
      value = round(max_val),
      elements = all_weights
    )
  )

}
