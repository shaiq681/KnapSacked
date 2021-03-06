% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brute_force_knapsack.R
\name{brute_force_knapsack}
\alias{brute_force_knapsack}
\title{Brute Force Knapsack Algorithm}
\usage{
brute_force_knapsack(x, W = NULL, ..., parallel = FALSE)
}
\arguments{
\item{x}{data.frame, contains variables weight(w) and value(v)}

\item{W}{weight threshold for the knapsack}

\item{...}{arguments}

\item{parallel}{boolean, if 'T' then implements parallelization}
}
\value{
Total and Maximum value within the threshold of W
}
\description{
The function returns maximum value within all the possible solutions
}
\examples{

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = FALSE)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel = FALSE)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000, parallel = FALSE)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000, parallel = FALSE)
}
\references{
\url{https://en.wikipedia.org/wiki/Knapsack problem}
}
\seealso{
\code{\link{slow_brute_force}}
\code{\link{greedy_knapsack}}
\code{\link{knapsack_dynamic}}
}
