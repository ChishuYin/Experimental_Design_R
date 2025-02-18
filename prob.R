#' @title Calculate Multinomial Distribution Parameters
#'
#' @description
#' Computes the parameters of a multinomial distribution represented by a 4-element vector \code{p = [p1, p2, p3, p4]}.
#' The function takes the probability of efficacy (\code{pe}), the probability of safety (\code{ps}), and the odds ratio (\code{phi})
#' to determine the joint probabilities under the constraints:
#'
#' \itemize{
#'   \item \code{p1 + p2 = pe}
#'   \item \code{p1 + p3 = ps}
#'   \item \code{p1 + p2 + p3 + p4 = 1}
#' }
#'
#'
#' @param pe Numeric. Probability of efficacy (\code{pe}), where \code{0 <= pe <= 1}.
#' @param ps Numeric. Probability of safety (\code{ps}), where \code{0 <= ps <= 1}.
#' @param phi Numeric. Odds ratio (\code{phi}) representing the relationship between efficacy and safety outcomes, defined as \code{phi = (p1 * p4) / (p2 * p3)}.
#'
#' @returns A numeric vector of length 4 representing the multinomial distribution parameters \code{[p1, p2, p3, p4]}.
#' @export
#'
#' @examples prob(0.7,0.5,1.5)
#'
prob <- function(pe,ps,phi)
{
  if(phi==1)
  {
    p<-c(pe*ps,pe-pe*ps,ps-pe*ps,1-pe-ps+pe*ps)
    p
  }
  else{
    a<- 1+(phi-1)*(pe+ps)
    b <- -4*phi*(phi-1)*pe*ps
    p <- matrix(nrow = 2, ncol = 2)
    p[1,1] <- round(( a - ( a^2+b )^(0.5) ) / (2*(phi-1)),digits = 5)
    p[1,2] <- round(pe-p[1,1],digits = 5)
    p[2,1] <- round(ps-p[1,1],digits = 5)
    if(round(1 - p[1,1] - p[1,2] - p[2,1],digits = 5)<0)
    {
      p[2,2] <- 0
      p[1,1] <- p[1,1] + round(1 - p[1,1] - p[1,2] - p[2,1],digits = 5)/3
      p[1,2] <- p[1,2] + round(1 - p[1,1] - p[1,2] - p[2,1],digits = 5)/3
      p[2,1] <- p[2,1] + round(1 - p[1,1] - p[1,2] - p[2,1],digits = 5)/3
    }
    else{
      p[2,2] <- round(1 - p[1,1] - p[1,2] - p[2,1],digits = 5)
    }

    p <- c(p[1,1],p[1,2],p[2,1],p[2,2])
    p}
}
