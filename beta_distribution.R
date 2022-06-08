###Question 2 -Plots two beta distributions
bayesian <- function( a1,b1, a2,b2, x = seq(0,1,.01)){
	#a1,b1 describe prior
	#a2,b2 describe posterior
	
	msg = paste("Prior Blue Beta distrubtion with a = ",a1," and b = ",b1, " ")
	
	print(msg)

	plot(x, dbeta(x, shape1 = a1, shape2=b1),type="l",col="blue", main =msg,
      ylim =c(0,10))
	lines(x, dbeta(x, shape1 = a2, shape2=b2),col="red", xlab = msg)

	legend("topleft",
	c("prior","posterior"),
	fill=c("blue","red")
	)
}


#2.a.)
bayesian(.5,.5,2.5,8.5)

for (i in 1:10){

bayesian(i , i , 1/i, 1/i)

Sys.sleep(2.5)
}