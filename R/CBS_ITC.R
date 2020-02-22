#' CBS_ITC
#'
#' Fit either a 1-piece or 2-piece CBS latent utility function to binary intertemporal choice data.
#'
#' The input data has n choices (ideally n > 100) between two reward options.
#' Option 1 is receiving \code{Amt1} in \code{Delay1} and Option 2 is receiving \code{Amt2} in \code{Delay2} (e.g., $40 in 20 days vs. $20 in 3 days).
#' One of the two options may be immediate (i.e., delay = 0; e.g., $40 in 20 days vs. $20 today).
#' \code{choice} should be 1 if option 1 is chosen, 0 if option 2 is chosen.
#'
#' @param choice Vector of 0s and 1s. 1 if the choice was option 1, 0 if the choice was option 2.
#' @param Amt1 Vector of positive real numbers. Reward amount of choice 1.
#' @param Delay1 Vector of positive real numbers. Delay until the reward of choice 1.
#' @param Amt2 Vector of positive real numbers. Reward amount of choice 2.
#' @param Delay2 Vector of positive real numbers. Delay until the reward of choice 2.
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
#'     \item \code{normD} : The domain of CBS function runs from 0 to \code{normD}. Specifically, this is the constant used to normalize all delays between 0 and 1, since CBS is fitted in a unit square first and then scaled up.
#' }
#' @export

CBS_ITC <- function(choice,Amt1,Delay1,Amt2,Delay2,numpiece,numfit=NULL){
  CBS_error(choice,Amt1,Delay1,Amt2,Delay2,numpiece,numfit) # error checking
  minpad = 1e-04; maxpad = 1-minpad # pad around bounds because the solving algorithm tests values around the bounds
  if(is.null(numfit)){numfit = 10*numpiece} # if not provided, use default number of numfit

  # normalizing delay to [0 1] for easier parameter search
  if(any(Delay1 > 1) | any(Delay2 > 1)){
    nD <- max(Delay1,Delay2); Delay1 <- Delay1/nD; Delay2 <- Delay2/nD
  }
  else{ nD <- 1 }

  # parameter bounds
  lb <- c(-36,rep(minpad,6*numpiece-1)); ub <- c(36,rep(maxpad,6*numpiece-1))
  if (numpiece == 1){ # active parameters (6): logbeta, x2, x3, y2, y3, y4
    A = rbind(c(0,0,0,0,-1,1),c(0,0,0,-1,0,1)); B = rep(-minpad,2) # linear constraints: y4-y3<0, y4-y2<0
    confun = NULL # no non-linear constraints
  } else if (numpiece == 2){ # active parameters (12): logbeta, x2,x3,x4,x5,x6, y2,y3,y4,y5,y6,y7
    # linear constraints:
    A = rbind(c(0,1,0,-1,0,0, numeric(6)),  # x2-x4<0
              c(0,0,1,-1,0,0, numeric(6)),  # x3-x4<0
              c(0,0,0,1,-1,0, numeric(6)),  # x4-x5<0
              c(0,0,0,1,0,-1, numeric(6)),  # x4-x6<0
              c(numeric(6), -1,0,1,0,0,0),  # y4-y2<0
              c(numeric(6), 0,-1,1,0,0,0),  # y4-y3<0
              c(numeric(6), 0,0,-1,1,0,0),  # y5-y4<0
              c(numeric(6), 0,0,-1,0,1,0),  # y6-y4<0
              c(numeric(6), 0,0,0,-1,0,1),  # y7-y5<0
              c(numeric(6), 0,0,0,0,-1,1))  # y7-y6<0
    B = rep(-minpad,10)
    confun = twopiece_nonlincon
  }
  # optimizer input and options
  funcinput <- list(objfun = function(x) ITCnegLL(x,Amt1,Delay1,Amt2,Delay2,choice,3*numpiece), confun = confun, A = A, B = B,
                    Aeq = NULL, Beq = NULL, lb = lb, ub = ub, tolX = 1e-04, tolFun = 1e-04, tolCon = minpad, maxnFun = 1e+07, maxIter = 400)

  # fitting
  mdl <- CBS_fitloop(funcinput,ITCrandstartpoint(numpiece,numfit))

  # organizing output
  LL <- -mdl$fn*length(choice)
  xpos <- c(0,mdl$par[2:(3*numpiece)],1)
  ypos <- c(1,mdl$par[(3*numpiece+1):(6*numpiece)])
  return( list("type" = paste("CBS",numpiece,sep=""), "LL" = LL, "numparam" = 6*numpiece, "scale" = exp(mdl$par[1]),"xpos"= nD*xpos, "ypos"=ypos, "AUC" = CBSfunc(xpos,ypos),"normD"=nD) )
}

#' ITCnegLL
#'
#' Calculates per-trial neg log-likelihood of a CBS ITC model.
#' \code{cutoff} marks the index of x that corresponds to last xpos
#' @noRd

ITCnegLL <- function(x,A1,V1,A2,V2,Ch,cutoff){
  yhat1 <- CBSfunc(c(0,x[2:cutoff],1), c(1,x[(cutoff+1):length(x)],1), V1)
  yhat2 <- CBSfunc(c(0,x[2:cutoff],1), c(1,x[(cutoff+1):length(x)],1), V2)
  return(negLL_logit(x[1],A1,yhat1,A2,yhat2,Ch))
}

#' ITCrandstartpoint
#'
#' Provides starting points for the CBS fitting function.
#' @noRd

ITCrandstartpoint <- function(numpiece,numpoints){
  sp <- seq(0.12,0.88,length.out=numpoints)
  if(numpiece == 1){return( cbind(numeric(numpoints),sp,sp,sp,sp,rep(0.01,numpoints)) )}
  else {return( cbind(numeric(numpoints),sp-0.11,sp-0.11,sp,sp+0.11,sp+0.11,sp+0.11,sp+0.11,sp,sp-0.11,sp-0.11,rep(0.01,numpoints)) )}
}
