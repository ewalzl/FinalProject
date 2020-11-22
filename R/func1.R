## Function 1: Mixture Normal Distribution.
##### Write an R function to do the following:
#- Generate n observations from a mixture of two normal distributions, i.e., generate U from a Bernoulli(p) distribution and if U = 1, then draw Y from a normal distribution with mean mu1 and standard deviation sigma1, or if U = 0, draw Y from the other normal distribution with mean mu2 and standard deviation sigma2.

#- In this context, there is a probability (p) that Y is drawn from a normal distribution N(mu1,sigma1), and there is a probability 1-p that Y is drawn from the other normal distribution N(mu2.sigma2). Note that the density of Y is the sum of weighted density of two normal distributions.

#- Write a function with five arguments (p,mu1,mu,sigma1,sigma2) to sample Y from the mixture normal distribution.


#Installing needed packages
install.packages("Rlab")
library("Rlab")
install.packages("gridExtra")
library(gridExtra)

#Function 1
func1 <-  function(p,mu1,mu2,sig1,sig2){
  Y <- data.frame(
    y = double(1000)
  )
  i = 1
  while(i <= 1000){
    U = rbern(1,p) #having U be either 1 or 0  based of input probability
    if (U == 1){ #if U = 1 then the y value will be a random value from a normal distribution with mean mu1 and stdv sig1
      Y$y[i] <- rnorm(1, mean = mu1, sd = sig1)
      i = i+1
    }
    else if (U == 0){ #if U = 0 then the y value will be a random value from a normal distribution with mean mu2 and stdv sig2
      Y$y[i] <- rnorm(1, mean = mu2, sd = sig2)
      i = i+1
    }
    else {stop("Something Has Gone Wrong")}
    
  }
  return(Y)
}

Y1 <- func1(0.5,-2,3,2,1.5)
Y2 <- func1(0.2,-2,3,2,1.5)


p3 <- ggplot(Y1, aes(x=y)) + 
  geom_histogram()

p4 <- ggplot(Y2, aes(x=y)) + 
  geom_histogram()

grid.arrange(p3, p4, nrow = 1)