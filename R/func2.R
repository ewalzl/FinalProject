
## Function 2: Weather Simulation.
##### Write a function to simulate the weather forecast in Richmond:
#Assume there are two states of weather in Richmond: sunny and rainy. If a day is sunny, the probability that the next day is sunny is 0.85. If a day is rainy, the probability that the next day is rainy is 0.35. If a day is rainy, the amount of rainfall accumulation in the city is governed by an Exponential(λ=2) distribution, where the value from that distribution is the rainfall in inches. If a day is sunny, there can be no rain.

#Specifically, given an initial day’s weather conditions, simulate the following 10 days of weather and calculate the projected rainfall accumulation in inches.

#Write a function describing this process:
#- First, assume sunny conditions on the initial day. Output the number of projected sunny days in the next 10 days, as well as the projected rainfall accumulation. Repeat this 1000 times and return the average of all these simulations.
#- Now, assume rainy conditions on the initial day. Output the number of projected sunny days in the next 10 days, as well as the projected rainfall accumulation. Repeat this 1000 times and return the average of all these simulations.



install.packages("Rlab")
library("Rlab")

#Function 2: Weather simulation
func2 <-  function(WC){ 
  weather = data.frame( #setting up a data frame for the 10-day weather forcast
    condition = character(10), #whether weather is sunny or rainy
    rainfall = double(10) #amount of rainfall if rainy
  )
  i = 1
  while(i <= 10){
    weather$condition[i] = WC #the current weather condition
    if (WC == 'sunny'){ #if the weather is sunny
      weather$rainfall[i] = 0 #no rain
      i = i+1
      U = rbern(1,0.85) #probability of next day being sunny is 0.85, sunny = 1 and rainy = 0
      if (U == 1){
        WC = 'sunny'
      }
      else {WC = 'rainy'}
    }
    else { #if the weather is rainy
      weather$rainfall[i] = rexp(1,2) #rainfall is exponentially distributed with lambda = 2
      i = i+1
      U = rbern(1,0.65) #probability of next day being sunny is 0.65, sunny = 1 and rainy = 0
      if (U == 1){
        WC = 'sunny'
      }
      else {WC = 'rainy'}
    }
    
  }
  return(weather)
}
