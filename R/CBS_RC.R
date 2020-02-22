#' CBS_RC
#'
#' Fit either a 1-piece or 2-piece CBS latent utility function to binary risky choice data.
#'
#' The input data has n choices (ideally n > 100) between two reward options.
#' Option 1 is receiving \code{Amt1} with probability \code{Prob1} and Option 2 is receiving \code{Amt2} with probability \code{Prob2} (e.g., $40 with 53\% chance vs. $20 with 90\% chance).
#' One of the two options may be certain (i.e., prob = 1; e.g., $40 with 53\% chance vs. $20 for sure).
#' \code{choice} should be 1 if option 1 is chosen, 0 if option 2 is chosen.
#'
#' @param choice Vector of 0s and 1s. 1 if the choice was option 1, 0 if the choice was option 2.
#' @param Amt1 Vector of positive real numbers. Reward amount of choice 1.
#' @param Prob1 Vector of positive real numbers between 0 and 1. Probability of winning the reward of choice 1.
#' @param Amt2 Vector of positive real numbers. Reward amount of choice 2.
#' @param Prob2 Vector of positive real numbers between 0 and 1. Probability of winning the reward of choice 2.
#' @param numpiece Either 1 or 2. Number of CBS pieces to use.
#' @param numfit Number of model fits to perform from different starting points. If not provided, numfit = 10*numpiece
#' @return A list containing the following:
#' \itemize{
#'     \item \code{type}: either 'CBS1' or 'CBS2' depending on the number of pieces
#'     \item \code{LL}: log likelihood of the model
#'     \item \code{numparam}: number of total parameters in the model
#'     \item \code{scale}: scaling factor of the logit model
#'     \item \code{xpos}: x coordinates of the fitted CBS function
#'     \item \code{ypos}: y coordinates of the fitted CBS function
#'     \item \code{AUC}: area under the curve of the fitted CBS function. Normalized to be between 0 and 1.
#' }
#' @examples
#' # Fit example Risky choice data with 2-piece CBS function.
#' # Load example data (included with package).
#' # Each row is a choice between option 1 (Amt with prob) vs option 2 (20 for 100%).
#' Amount1 = RCdat$Amt1
#' Prob1 = RCdat$Prob1
#' Amount2 = 20
#' Prob2 = 1
#' Choice = RCdat$Choice
#'
#' # Fit the model
#' out = CBS_RC(Choice,Amount1,Prob1,Amount2,Prob2,2)
#'
#' # Plot the choices (x = Delay, y = relative amount : 20 / risky amount)
#' plot(Prob1[Choice==1],20/Amount1[Choice==1],type = 'p',col="blue",xlim=c(0, 1), ylim=c(0, 1))
#' points(Prob1[Choice==0],20/Amount1[Choice==0],type = 'p',col="red")
#'
#' # Plot the fitted CBS
#' x = seq(0,1,.01)
#' lines(x,CBSfunc(out$xpos,out$ypos,x))
#' @export


CBS_RC <- function(choice,Amt1,Prob1,Amt2,Prob2,numpiece,numfit=NULL){
  CBS_error(choice,Amt1,Prob1,Amt2,Prob2,numpiece,numfit) # error checking
  minpad = 1e-04; maxpad = 1-minpad # pad around bounds because the solving algorithm tests values around the bounds
  if(is.null(numfit)){numfit = 10*numpiece} # if not provided, use default number of numfit

  # checking to make sure probability is within 0 and 1
  if(any(Prob1>1) | any(Prob2>1)){stop("prob not within [0 1]")}

  # number of parameters in the model
  numparam <- 6*numpiece-1

  # parameter bounds
  lb <- c(-36,rep(minpad,numparam-1)); ub <- c(36,rep(maxpad,numparam-1))
  if (numpiece == 1){ # active parameters (5): logbeta, x2, x3, y2, y3
    A = NULL; B = NULL; # no linear constraints
    confun = NULL # no non-linear constraints
  } else if (numpiece == 2){ # active parameters (11): logbeta, x2,x3,x4,x5,x6, y2,y3,y4,y5,y6
    # linear constraints:
    A = rbind(c(0,1,0,-1,0,0, numeric(5)),  # x2-x4<0
              c(0,0,1,-1,0,0, numeric(5)),  # x3-x4<0
              c(0,0,0,1,-1,0, numeric(5)),  # x4-x5<0
              c(0,0,0,1,0,-1, numeric(5)),  # x4-x6<0
              c(numeric(6), 1,0,-1,0,0),  # y2-y4<0
              c(numeric(6), 0,1,-1,0,0),  # y3-y4<0
              c(numeric(6), 0,0,1,-1,0),  # y4-y5<0
              c(numeric(6), 0,0,1,0,-1))  # y4-y6<0


    B = rep(-minpad,8)
    confun = twopiece_nonlincon
  }
  # optimizer input and options
  funcinput <- list(objfun = function(x) RCnegLL(x,Amt1,Prob1,Amt2,Prob2,choice,(numparam+1)/2), confun = confun, A = A, B = B,
                    Aeq = NULL, Beq = NULL, lb = lb, ub = ub, tolX = 1e-04,tolFun = 1e-04, tolCon = minpad, maxnFun = 1e+07, maxIter = 400)

  # fitting
  mdl <- CBS_fitloop(funcinput,RCrandstartpoint(numpiece,numfit))

  # organizing output
  LL <- -mdl$fn*length(choice)
  xpos <- c(0,mdl$par[2:((numparam+1)/2)],1)
  ypos <- c(0,mdl$par[((numparam+1)/2 +1):numparam],1)
  return( list("type"=paste("CBS",numpiece,sep=""), "LL" = LL, "numparam" = numparam, "scale" = exp(mdl$par[1]),"xpos"=xpos, "ypos"=ypos, "AUC" = CBSfunc(xpos,ypos)) )
}

#' RCnegLL
#'
#' Calculates per-trial neg log-likelihood of a CBS RC model.
#' \code{cutoff} marks the index of x that corresponds to last xpos
#' @noRd

RCnegLL <- function(x,A1,V1,A2,V2,Ch,cutoff){
  yhat1 <- CBSfunc(c(0,x[2:cutoff],1), c(0,x[(cutoff+1):length(x)],1), V1)
  yhat2 <- CBSfunc(c(0,x[2:cutoff],1), c(0,x[(cutoff+1):length(x)],1), V2)
  return(negLL_logit(x[1],A1,yhat1,A2,yhat2,Ch))
}

#' Rrandstartpoint
#'
#' Provides starting points for the CBS fitting function.
#' @noRd

RCrandstartpoint <- function(numpiece,numpoints){
  sp <- seq(0.12,0.88,length.out=numpoints)
  if(numpiece == 1){return( cbind(numeric(numpoints),1-sp,1-sp,sp,sp) )}
  else {return( cbind(numeric(numpoints),1-sp-0.11,1-sp-0.11,1-sp,1-sp+0.11,1-sp+0.11,sp-0.11,sp-0.11,sp,sp+0.11,sp+0.11) )}
}
