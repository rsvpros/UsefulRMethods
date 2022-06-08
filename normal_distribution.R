normalpdf <- function(x =0,u =0, sigma =1){
	return(exp(-(x-u)^2/(2*sigma^2))/(sigma*sqrt(2*pi)))
}

print("Probability of a .5 in a normal dist:")
print(normalpdf(.5))

x = seq(-4,4,.1)
plot(x,normalpdf(x))