#'Generate stochastic age at death based on lifetables
#' 
#'Returns a stochastic age of death for an individual of a given age and
#'gender. Multiple regions are supported and a stndardised mortality
#'ratio may be applied.
#' 
#'@param age currrent age of individual
#'@param country country lifetable to be sampled from. england, england&Wales, Scotland, Wales, UK
#'@param male gender of individual (male: TRUE, female:FALSE)
#'@param prob probability, normally random number
#'@param smr standardised mortality ratio. Must be greater than 0. 1 is default.
#'@param data.location where the data are stored
#'@param prob2 another probability. Normally random. To provide non-integer ages
#'@export
#'@examples
#'my.age <- 26
#'my.country <- c("england")
#'my.smr <-1.3
#'AgeDeathLT(age = my.age, country = my.country, smr = my.smr)


AgeDeathLT<-function(age, country, male=TRUE, prob=runif(1), smr=1, data.location="./data/lifetables", prob2=runif(1)) {
  
#http://www.ons.gov.uk/ons/publications/re-reference-tables.html?edition=tcm%3A77-223324
#Interim life-tables 2008-2010
#Probs are 'lx'/100,000
  
files<-list.files(data.location)
myfile<-paste(country,".csv",sep="")
if (myfile %in% files==FALSE){
  stop('country does not match a file in data location')
  }
  #Read in data
  
  data<-as.matrix(read.table(file.path(data.location,myfile),header=T,sep=","))    
  data <- data.frame(data[order(-data[,1]),])
  age.down<-floor(age)
  
  if (age.down >= 101) {                                  #check age does not exceed maximum age of life-tables
    age.down <- 101
    warning('Age is greater than 100, truncated to 100')
  }
  
  age.diff <- age - age.down
  
  if (male == TRUE) {
    array.index <- 2  #note 1 is reserved for ages 0-101
  } else {
    array.index <- 3
  }
  
  rows.down <- (102 - age.down) 
  age.2.prob <- data[rows.down,array.index]* (prob ^ (1 / smr))
  
  #Look up age of death corresponding to age of mortality
  #life.row <- match(age.2.prob, data[,array.index])
  life.row<-FindIt(x=age.2.prob,vec=data[,array.index])
  
  #Interpolation of age of death
  age.death <- data[life.row, 1]
  age.death <- min(101, age.death + age.diff + prob2) #Assumes even chance of death over year

  return(age.death)
  
}