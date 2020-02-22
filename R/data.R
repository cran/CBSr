#' Sample participant data from a binary intertemporal choice task (aka delay discounting task)
#'
#' A dataset containing one sample participant's 120 binary choices between a delayed monetary option (\code{Amt1} in \code{Delay1}) and a immediate monetary option ($20 now).
#' The immediate monetary option was always '$20 now' across all trials
#'
#' @format A data frame with 120 rows and 3 variables:
#' \describe{
#'   \item{Amt1}{Delayed reward amount, in dollars}
#'   \item{Delay1}{Delay until the receipt of \code{Amt1}, in days}
#'   \item{Choice}{Choice between binary options. \code{Choice==1} means participnat chose the delayed option (i.e., \code{Amt1} in \code{Delay1} days). \code{Choice==0} means participnat chose the immediate option (i.e., $20 now)}
#' }
#' @source Kable, J. W., Caulfield, M. K., Falcone, M., McConnell, M., Bernardo, L., Parthasarathi, T., ... & Diefenbach, P. (2017). No effect of commercial cognitive training on brain activity, choice behavior, or cognitive performance. Journal of Neuroscience, 37(31), 7390-7402.
"ITCdat"

#' Sample participant data from a binary risky choice task (aka risk aversion task)
#'
#' A dataset containing one sample participant's 120 binary choices between a probabilistic monetary option (\code{Amt1} with \code{Prob1} chance of winning) and a certain monetary option ($20 for sure).
#' The certain monetary option was always '$20 for sure' across all trials
#'
#' @format A data frame with 120 rows and 3 variables:
#' \describe{
#'   \item{Amt1}{Probabilistic reward amount, in dollars}
#'   \item{Prob1}{Probability of winning \code{Amt1}, if it were to be chosen}
#'   \item{Choice}{Choice between binary options. \code{Choice==1} means participnat chose the probabilistic option (i.e., \code{Amt1} with \code{Delay1} chance of winning). \code{Choice==0} means participnat chose the certain option (i.e., $20 for sure)}
#' }
#' @source Kable, J. W., Caulfield, M. K., Falcone, M., McConnell, M., Bernardo, L., Parthasarathi, T., ... & Diefenbach, P. (2017). No effect of commercial cognitive training on brain activity, choice behavior, or cognitive performance. Journal of Neuroscience, 37(31), 7390-7402.
"RCdat"
