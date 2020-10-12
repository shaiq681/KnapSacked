#' Dataset for demonstration
#'
#' @format A data.frame with 2000 rows and two variables:
#' \describe{
#'  \item{w}{Integer,  weight}
#'  \item{v}{float, value of each object}
#' }
#' @export
"knapsack_objects"

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
