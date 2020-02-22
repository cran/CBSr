#' CBSfunc
#'
#' Calculate either the Area Under the Curve (AUC) of a CBS function, or calculate the y coordinates of CBS function given x.
#' @param xpos Vector of real numbers of length 1+3n (n = 1, 2, 3, ...), corresponding to Bezier points' x-coordinates of a CBS function
#' @param ypos Vector of real numbers of length 1+3n (n = 1, 2, 3, ...), corresponding to Bezier points' y-coordinates of a CBS function
#' @param x Vector of real numbers, corresponding to x-coordinates of a CBS function. Default value is Null.
#' @return If x is provided, return y coordinates corresponding to x. If x is not provided, return AUC.
#' @examples
#' CBSfunc(c(0,0.3,0.6,1),c(0.5, 0.2, 0.7, 0.9))
#' CBSfunc(c(0,0.3,0.6,1),c(0.5, 0.2, 0.7, 0.9),seq(0,1,0.1))
#' @export

CBSfunc <- function(xpos,ypos,x = NULL){
  xpos <- as.double(xpos); ypos <- as.double(ypos)
  if (length(xpos) != length(ypos)){stop("length of xpos and ypos different!")}
  if (length(xpos) < 4){stop("length of xpos and ypos too short. They must have at least 4 elements")}
  if (length(xpos)%%3 != 1){stop("unexpected length of xpos and ypos. They should be 3n+1 (n = 1, 2, ...)")}
  if (is.null(x)){ #x is not provided. hence calculating AUC
    return(CBSAUC(xpos,ypos))
  } else { # x is provided. hence calculating yhat
    return(rJava::.jcall("CBScalc", returnSig = "[D","getyhat",xpos,ypos,rJava::.jarray(as.double(x))))
  }
}


#' CBSAUC
#'
#' calculates area under the curve of the entire CBS chain by calculating local AUCs for each piece and adding them up
#' @noRd

CBSAUC <- function(xpos,ypos){
  AUC = 0;
  for(i in seq(1,length(xpos)-1,3)){
    AUC = AUC + partialAUC(xpos[i],xpos[i+1],xpos[i+2],xpos[i+3],ypos[i],ypos[i+1],ypos[i+2],ypos[i+3])
  }
  return(AUC)
}


#' partialAUC
#'
#' calculated area under the curve of a single piece of CBS using analytic formula
#' checks for CBS function constraint.
#' @noRd

partialAUC <- function(x1,x2,x3,x4,y1,y2,y3,y4){
  check1 =  -sqrt((x4-x3)*(x2-x1)) < (x3-x2)
  check2 = x1 <= x2
  check3 = x3 <= x4
  if (!check1 || !check2 || !check3){
    warning("CBS x coordinates not a monotonic function of t. Multiple y for x may exist. AUC may be inaccurate")
  }
  return((6*x2*y1-6*x1*y2-10*x1*y1-3*x1*y3+3*x3*y1-x1*y4-3*x2*y3+3*x3*y2+x4*y1-3*x2*y4+3*x4*y2-6*x3*y4+6*x4*y3+10*x4*y4)/20)
}
