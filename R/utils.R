#' twopiece_nonlincon
#'
#' Provides non-linear equality and inequality constraint for a 2-piece CBS.
#' @noRd

twopiece_nonlincon <- function(x){
  minhandle = 0.1
  x3 = x[3]; x4 = x[4]; x5 = x[5]; y3 = x[8]; y4 = x[9]; y5 = x[10]
  # non-linear inequalities: 0.1^2 -(x4-x3)^2 -(y4-y3)^2 < 0, 0.1^2-(x5-x4)^2-(y5-y4)^2 < 0
  c_ineq = c(minhandle^2 -(x4-x3)^2 -(y4-y3)^2, minhandle^2 -(x5-x4)^2 -(y5-y4)^2)
  # non-linear equalities: (x4-x3)/(y4-y3) = (x5-x4)/(y5-y4)
  ceq = (x4-x3)*(y5-y4)-(x5-x4)*(y4-y3)
  return(list(c=c_ineq,ceq=ceq))
}


#' negLL_logit
#'
#' Calculates per-trial negative log-likelihood of a binary logit model: logit(p(y=1)) = exp(scale) x (A1 x yhat - A2 x yhat)
#' @noRd

negLL_logit <- function(scale,A1,yhat1,A2,yhat2,Ch){
  DV <- A1*yhat1 - A2*yhat2 # diff between utilities
  DV[Ch==0] = -DV[Ch==0] # utility difference toward choice
  reg = -exp(scale)*DV # scale parameter
  logp = -log(1+exp(reg)) # directly calculating logp
  logp[reg>709] = -reg[reg>709]; # log(realmax) is about 709.7827. (e.g., try log(exp(709)) vs. log(exp(710)))
  return(-mean(logp)) # making per-trial LL
}


#' CBS_error
#'
#' Checks for input errors on CBS_ITC and CBS_RC
#' @noRd

CBS_error <- function(choice,Amt1,Var1,Amt2,Var2,numpiece,numfit){
  if(any(choice != 0 & choice != 1)){stop("Choice should be a vector of 0 or 1")}
  if(any(Amt1 < 0) | any(Amt2 < 0)){stop("Negative amounts are not allowed")}
  if(any(Var1 < 0) | any(Var2 < 0)){stop("Negative delays or probabilities are not allowed")}
  if(numpiece != 1 && numpiece !=2){stop("Sorry! Only 1-piece and 2-piece CBS functions are supported at the moment.")}
  if(!is.null(numfit) && numfit<2){stop("Too few starting points (numfit)")}
}


#' CBS_fitloop
#'
#' Loop through multiple starting points and returns the best model.
#' This code is basically my substitute for MATLAB's multistart feature, which NlcOptim package does not have.
#' Also, NlcOptim package causes errors at certain situations that doesn't seem to be due to my misuse.
#' It seems that it fails when numerical derivatives and/or constraint matrices are close to singular, which is not something I can know/control a priori.
#' Hence, until I figure out a better way to deal with it (or NlcOptim package is updated to be more robust), we just try a different starting point.
#' From extensive testing on all data I have, it seems to happen once or twice every few hundred fittings.
#' I also tried other optimization packages that support non-linear inequality AND equality constraints, but they were all much slower than NlcOptim, which is already slower than MATLAB's fmincon.
#' Other developers, who want to see what errors I'm talking about, set the 'silent=TRUE' to FALSE in the try statement below and run the CBS_RC example script.
#' @noRd

CBS_fitloop <- function(inputlist,startingpoints){
  start_time <- Sys.time()
  successcounter = 0
  bestmdl <- NULL
  for(i in 1:dim(startingpoints)[1]){ # looping through starting points. Each row of the matrix is a starting point.
    newmdl <- NULL
    inputlist$X = startingpoints[i,] # new starting point
    try(newmdl <- do.call(NlcOptim::solnl,inputlist),silent=TRUE) # try fitting
    if(!is.null(newmdl)){ # if the fitting succeeded
      successcounter = successcounter+1
      if(is.null(bestmdl)){bestmdl <- newmdl} # if current best model was Null, change it to the new fit
      else{ # current best model exists
        if(newmdl$fn < bestmdl$fn){ bestmdl <- newmdl } # if new fit is better, change current model to new fit
      }
    }
  }
  if(is.null(bestmdl)){stop("No convergence! Consider using more starting points (numfit)")}
  else{message(paste(successcounter,"out of",dim(startingpoints)[1],"models converged with a local solution"))}
  message(paste("Fitting Time :",round(Sys.time() - start_time,2),"seconds"))
  return(bestmdl)
}
