Package CBSr

R Package for using Cubic Bezier Spline as a function approximator especially in modeling latent utility functions. While Cubic Bezier Splines (CBS) are heavily used in the graphics software industry, it can also be used as a flexible function approximation tool given the right constraints. The CBS package provides a method to calculate the y value from a x value given an appropriately constrained CBS curve. It then uses this method to approximate latent utility functions in intertemporal choice and risky choice data. PsyArXiv : Lee, Glaze, Bradlow, Kable, (2019) <https://doi.org/10.31234/osf.io/2ugwr>.



Example code below:

# fit example ITC data with 2-piece CBS function

## each row is a choice between option 1 (Amt at Delay) vs option 2 (20 now).
Amount1 = ITCdat$Amt1
Delay1 = ITCdat$Delay1
Amount2 = 20
Delay2 = 0
Choice = ITCdat$Choice

## fit the model
out = CBS_ITC(Choice,Amount1,Delay1,Amount2,Delay2,2)

## plot the choices (x = Delay, y = relative amount : 20 / delayed amount)
plot(Delay1[Choice==1],20/Amount1[Choice==1],type = 'p',col="blue",xlim=c(0, 180), ylim=c(0, 1))
points(Delay1[Choice==0],20/Amount1[Choice==0],type = 'p',col="red")

## plot the fitted CBS
x = 0:out$normD
lines(x,CBSfunc(out$xpos,out$ypos,x),col="black")




# fit example Risky choice data with 2-piece CBS function

## each row is a choice between option 1 (Amt with prob) vs option 2 (20 for 100%).
Amount1 = RCdat$Amt1
Prob1 = RCdat$Prob1
Amount2 = 20
Prob2 = 1
Choice = RCdat$Choice

## there's no need to normalize as probability is already in [0 1]

## fit the model
out = CBS_RC(Choice,Amount1,Prob1,Amount2,Prob2,2)

## plot the choices (x = Delay, y = relative amount : 20 / risky amount)
plot(Prob1[Choice==1],20/Amount1[Choice==1],type = 'p',col="blue",xlim=c(0, 1), ylim=c(0, 1))
points(Prob1[Choice==0],20/Amount1[Choice==0],type = 'p',col="red")

## plot the fitted CBS
x = seq(0,1,.01)
lines(x,CBSfunc(out$xpos,out$ypos,x))
