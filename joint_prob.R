#' @title Calculate Joint Probability
#'
#' @description
#' This function computes the joint probability of observing specific values \( x_e \) and \( x_s \) for the random variables \( X_e \) and \( X_s \), respectively, given the total number of trials \( n \) and a probability vector \( p \). The probability vector \( p \) should be of length 4, representing the probabilities of all possible outcomes in a multinomial distribution.
#'
#'
#' @param xe Integer. The observed value of the random variable \( X_e \)
#' @param xs Integer. The observed value of the random variable \( X_s \).
#' @param n Integer. The total number of trials or sample size.
#' @param p Numeric vector. A vector of length 4 representing the probabilities associated with each outcome in the multinomial distribution. The elements of \( p \) should sum to 1.
#' @return Numeric. The joint probability \( P(X_e = x_e, X_s = x_s) \).
#'
#'
#' @export
#'
#' @examples joint_prob(2,2,5,c(0.1,0.1,0.2,0.6))
#'
#'
#'
joint_prob <- function(xe,xs,n,p)
{
  sum <- 0
  for(i in max(0,xe+xs-n) : min(xe,xs))
  {
    if(i<0 || xe-i<0 || xs-i<0 || n-xe-xs+i<0)
    {
      sum<-sum+0
    }
    else{
      x <- c( i , xe-i , xs-i , n-xe-xs+i )
      sum <- sum + dmultinom(x, size = NULL, prob = p, log = FALSE)
      ##cat("s = ", sum,"x =", x, "\n")
    }
  }
  sum
}
