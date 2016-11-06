
#Lung cancer model#############################################
#Created by D Trueman 2012
#Built in R 2.15
#Distributed under GPL 3.0

#Clear objects ####
rm(list=ls()) 

#Load required libraries ####
require(plyr)
require(ggplot2)
require(DESutils)

#Set system variables ####
set.seed(666)


#Load data ####
model.data <- read.table("data/model/modeldata.csv",header=T,sep=",")
#pt.data    <- read.table("data/model/patientdata.csv",header=T,sep=",")
rx.data    <- read.table("data/model/therapydata.csv",header=T,sep=",")
strat.data <- read.table("data/model/strategydata.csv",header=T,sep=",")

#Process data ####
#Model data
num.param <- nrow(model.data)
  for (z in 1:num.param) {
    assign(as.character(model.data[z,1]),model.data[z,2])
  }

#Patient data
#Therapy data
num.strats <- nrow(strat.data)

#Strategy data
#Note: number of therapies per strategy may be defined within the strategy loop

#Create dataframes for storing output ####
costs <- data.frame(NULL)
qalys <- data.frame(NULL)

#Model ####
for (i in 1:num.pts){
  
  for (s in 1:num.strats){
  
    num.tx <- ncol(strat.data[s]) - 1
    
    for (t in 1:num.tx){
    
      n.this.rx <- strat.data[s, t+1] #+1 because column 1 is the strategy 'name'
      
      if (rx.data$type[n.this.rx]=="line"){
        #gen time to death
        print("line")
      }else{
        #Use the current time to death
        print("maintenance")
      }
      
      
    #Determine response to therapy (1st or 2nd line [not BSC]) ####
    #Generate time to progression, unacceptable toxicity or patient/physician choice ####
    #Generate time to next chemotherapy starts ####
    
    
    qalys[i,s] <- runif(1)
    costs[i,s] <- runif(1)
    
    } #End patient loop
  } #End strategy loop
} #End patient loop

#Analysis of output ####
#Data manipulation
qalys <- stack(x = qalys)
colnames(qalys)[1] <- 'qalys'
costs <- stack(x = costs)
colnames(costs)[1] <- 'costs'
sim.data <- cbind(qalys, costs)
sim.data$strat <- as.numeric(sim.data$ind)
sim.data$strat <- factor(x = sim.data$strat, labels = as.character(strat.data[,1]))
sim.data <- sim.data[,!(colnames(sim.data) %in% c("ind"))] #drop the 'ind' column

#Summary of results
results.mean <- ddply(sim.data, .(strat), numcolwise(mean))
results.median <- ddply(sim.data, .(strat), numcolwise(median))
results.sterr <- ddply(sim.data, .(strat), numcolwise(CoefVar))

# Additional analysis ####

# Plots ####
lambda <- 30000
#Average NHB plots
sim.data$nhb <- sim.data$costs*lambda - sim.data$costs
  for (i in 1:num.strats) {
    strt <- (i - 1) * num.pts
    end <- i * num.pts
    temp.data <- sim.data[strt:end,]
    temp.data$cum.nhb <- cumsum(temp.data$nhb) / seq_along(temp.data$nhb)
    temp.data$pt <- seq_along(temp.data$nhb)
    sim.data$cum.nhb[strt:end] <- temp.data$cum.nhb
    sim.data$pt[strt:end] <- temp.data$pt
  }

ggplot(sim.data, aes(y = sim.data$cum.nhb, x = sim.data$pt)) + 
  geom_line(colour = "blue", size = 0.8) + 
  facet_wrap(~strat, ncol = 1) +
  scale_x_continuous('Patient') + 
  scale_y_continuous(paste('NHB (lambda=£',lambda,')', sep="")) +
  theme_bw(14) +
  opts(strip.background = theme_rect(fill = 'white',colour = 'grey')) #Examples of usage https://github.com/hadley/ggplot2/wiki/Faceting-Attributes
  
#NHB density plot
ggplot(sim.data) +
  geom_density(aes(x = sim.data$nhb,colour = sim.data$strat), size = 0.6) +
  scale_x_continuous(paste('NHB (lambda=£',lambda,')', sep="")) +
  theme_bw(14) +
  labs(colour = "Strategy")

#Conditional NHB density plot
ggplot(sim.data) +
  geom_density(aes(x = sim.data$nhb, fill = sim.data$strat), size = 0.6, position="fill") +
  scale_x_continuous(paste('NHB (lambda=£',lambda,')', sep="")) +
  scale_y_continuous('Count') +
  theme_bw(14) +
  labs(fill = "Strategy")

#CE plane scatter ('trellis' style)
ggplot(sim.data) +
  geom_point(aes(x = qalys, y = costs), colour="#0000ff50") +
  scale_x_continuous('QALYs') +
  scale_y_continuous('Cost') +
  theme_bw(14) +
  facet_wrap(~strat, ncol = 1) +
  opts(strip.background = theme_rect(fill = 'white',colour = 'grey')) #Examples of usage https://github.com/hadley/ggplot2/wiki/Faceting-Attributes

#CE plane scatter 
ggplot(sim.data) +
  geom_point(aes(x = qalys, y = costs, colour=strat)) +
  scale_x_continuous('QALYs') +
  scale_y_continuous('Cost') +
  theme_bw(14) +
  labs(colour = "Strategy")
