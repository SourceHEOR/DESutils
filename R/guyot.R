#' Create patient-level dataset from published KM curves
#' 
#' Wrapper for algorithm provided by Guyolt et al. to facilitate easier use.
#' Resturns a patient-level dataset
#' 
#' @param my.path string of file from where all the data must be accessible
#' @param n.arms integer representing teh number of arms in the study/data
#' @param surv.data vector of strings containing names of csv data files with
#'   times and probability surviving. One per arm must be supplied. Note:
#'   filepath and the strings in this vector will be combined to retrieve data. 
#' @param risk.data vector of strings containing names of csv data files with
#'   times intervals, corresponding points from surv.data, and n at risk. One per arm must be supplied. Note:
#'   filepath and the strings in this vector will be combined to retrieve data.
#' @param tot.events total number of events reported. If not reported, then tot.events=NULL.
#'   If included, should be a list with number of elements = n.arms 
#' @param out.path a string representing a filepath where temporaryd datasets will be stored.
#'   If not specified, filepath will be used
#' @references Guyot et al. Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves. BMC Medical Research Methodology 2012, 12:9
#' @export
#' @examples
#' #Original 'real' data
#' require(survival)
#' bladder
#' bsurv<-Surv(bladder$stop, bladder$event)
#' bfit<-survfit(bsurv~bladder$rx)
#' Ggkm(bfit, timeby=10, ystratalabs=c("Treatment 1","Treatment 2"), main="Original data")
#' 
#' #Reconstructed data
#' patient.data <- DigitizeKM(my.path="data/bladder",
#'                           n.arms=2,
#'                           surv.data=c('Treatment1.csv', 'Treatment2.csv'),
#'                           risk.data=c('tx1_risk.csv','tx2_risk.csv'))
#'                           
#'                           
#' bfit<-survfit(Surv(patient.data[,1], patient.data[,2])~patient.data[,3])
#' Ggkm(bfit, timeby=10, ystratalabs=c("Treatment 1","Treatment 2"), main="Reconstructed data")

DigitizeKM <- function(my.path,
                       n.arms=1,
                       surv.data,
                       risk.data,
                       tot.events = NULL,
                       out.path = NULL) {

require("MASS")
require("splines")
require("survival")

if (is.null(out.path) == TRUE) {
  out.path <- my.path
}

if (is.null(tot.events) == TRUE) {
  any.events = FALSE
} else {
  any.events = TRUE
}

out.data.path=NULL

for (q in 1:n.arms) {

#Inputs used by function ####  
data.path   <- file.path(my.path, surv.data[q])
n.risk.path <- file.path(my.path, risk.data[q])
digizeit<- read.csv(file = data.path, header=T) #Input survival times from graph reading "data initials study2 figA arm1 time1.txt" 
pub.risk<- read.csv(file = n.risk.path, header=T) #nriskfile<-"control_risk.csv" #Input reported number at risk
KMdatafile <- paste("KMdata_out_",q,".txt",sep = "") #Output file events and cens
KMdataIPDfile <- paste("KMdataIPD_out_",q,".txt", sep="") #Output file for IPD
arm.id <- q #arm indicator
    

  #Read in survival times read by digizeit
  t.S<-digizeit[,2]
  S<-digizeit[,3]
  #Read in published numbers at risk, n.risk, at time, t.risk, lower and upper
  # indexes for time interval
  
  t.risk<-pub.risk[,2]
  lower<-pub.risk[,3]
  upper<-pub.risk[,4]
  n.risk<-pub.risk[,5]
  n.int<-length(n.risk)
  n.t<- upper[n.int]
  #Initialise vectors
  arm<-rep(arm.id,n.risk[1])
  n.censor<- rep(0,(n.int-1))
  n.hat<-rep(n.risk[1]+1,n.t)
  cen<-rep(0,n.t)
  d<-rep(0,n.t)
  KM.hat<-rep(1,n.t)
  last.i<-rep(1,n.int)
  sumdL<-0
  if (n.int > 1){
    #Time intervals 1,...,(n.int-1)
    for (i in 1:(n.int-1)){
      #First approximation of no. censored on interval i
      n.censor[i]<- round(n.risk[i]*S[lower[i+1]]/S[lower[i]]- n.risk[i+1])
      #Adjust tot. no. censored until n.hat = n.risk at start of interval (i+1)
      while((n.hat[lower[i+1]]>n.risk[i+1])||((n.hat[lower[i+1]]<n.risk[i+1])&&(n.censor[i]>0))){
        if (n.censor[i]<=0){
          cen[lower[i]:upper[i]]<-0
          n.censor[i]<-0
        }
        if (n.censor[i]>0){
          cen.t<-rep(0,n.censor[i])
          for (j in 1:n.censor[i]){
            cen.t[j]<- t.S[lower[i]] +
              j*(t.S[lower[(i+1)]]-t.S[lower[i]])/(n.censor[i]+1)
          }
          #Distribute censored observations evenly over time. Find no. censored on each time interval.
          cen[lower[i]:upper[i]]<-hist(cen.t,breaks=t.S[lower[i]:lower[(i+1)]],
                                       plot=F)$counts
        }
        #Find no. events and no. at risk on each interval to agree with K-M estimates read from curves
        n.hat[lower[i]]<-n.risk[i]
        last<-last.i[i]
        for (k in lower[i]:upper[i]){
          if (i==1 & k==lower[i]){
            d[k]<-0
            KM.hat[k]<-1
          }
          else {
            d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))
            KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
          }
          n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
          if (d[k] != 0) last<-k
        }
        n.censor[i]<- n.censor[i]+(n.hat[lower[i+1]]-n.risk[i+1])
      }
      if (n.hat[lower[i+1]]<n.risk[i+1]) n.risk[i+1]<-n.hat[lower[i+1]]
      last.i[(i+1)]<-last
    }
  }
  #Time interval n.int.
  if (n.int>1){
    #Assume same censor rate as average over previous time intervals.
    n.censor[n.int]<- min(round(sum(n.censor[1:(n.int-1)])*(t.S[upper[n.int]]-
      t.S[lower[n.int]])/(t.S[upper[(n.int-1)]]-t.S[lower[1]])), n.risk[n.int])
  }
  if (n.int==1){n.censor[n.int]<-0}
  if (n.censor[n.int] <= 0){
    cen[lower[n.int]:(upper[n.int]-1)]<-0
    n.censor[n.int]<-0
  }
  if (n.censor[n.int]>0){
    cen.t<-rep(0,n.censor[n.int])
    for (j in 1:n.censor[n.int]){
      cen.t[j]<- t.S[lower[n.int]] +
        j*(t.S[upper[n.int]]-t.S[lower[n.int]])/(n.censor[n.int]+1)
    }
    cen[lower[n.int]:(upper[n.int]-1)]<-hist(cen.t,breaks=t.S[lower[n.int]:upper[n.int]],
                                             plot=F)$counts
  }
  #Find no. events and no. at risk on each interval to agree with K-M estimates read from curves
  n.hat[lower[n.int]]<-n.risk[n.int]
  last<-last.i[n.int]
  for (k in lower[n.int]:upper[n.int]){
    if(KM.hat[last] !=0){
      d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))} else {d[k]<-0}
    KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
    n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
    #No. at risk cannot be negative
    if (n.hat[k+1] < 0) {
      n.hat[k+1]<-0
      cen[k]<-n.hat[k] - d[k]
    }
    if (d[k] != 0) last<-k
  }
  #If total no. of events reported, adjust no. censored so that total no. of events agrees.
  if (any.events == TRUE){
    if (n.int>1){
      sumdL<-sum(d[1:upper[(n.int-1)]])
      #If total no. events already too big, then set events and censoring = 0 on all further time intervals
      if (sumdL >= tot.events[q]){
        d[lower[n.int]:upper[n.int]]<- rep(0,(upper[n.int]-lower[n.int]+1))
        cen[lower[n.int]:(upper[n.int]-1)]<- rep(0,(upper[n.int]-lower[n.int]))
        n.hat[(lower[n.int]+1):(upper[n.int]+1)]<- rep(n.risk[n.int],(upper[n.int]+1-lower[n.int]))
      }
    }
    #Otherwise adjust no. censored to give correct total no. events
    if ((sumdL < tot.events[q])|| (n.int==1)){
      sumd<-sum(d[1:upper[n.int]])
      while ((sumd > tot.events[q])||((sumd< tot.events[q])&&(n.censor[n.int]>0))){
        n.censor[n.int]<- n.censor[n.int] + (sumd - tot.events[q])
        if (n.censor[n.int]<=0){
          cen[lower[n.int]:(upper[n.int]-1)]<-0
          n.censor[n.int]<-0
        }
        if (n.censor[n.int]>0){
          cen.t<-rep(0,n.censor[n.int])
          for (j in 1:n.censor[n.int]){
            cen.t[j]<- t.S[lower[n.int]] +
              j*(t.S[upper[n.int]]-t.S[lower[n.int]])/(n.censor[n.int]+1)
          }
          cen[lower[n.int]:(upper[n.int]-1)]<-hist(cen.t,breaks=t.S[lower[n.int]:upper[n.int]],
                                                   plot=F)$counts
        }
        n.hat[lower[n.int]]<-n.risk[n.int]
        last<-last.i[n.int]
        for (k in lower[n.int]:upper[n.int]){
          d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))
          KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
          if (k != upper[n.int]){
            n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
            #No. at risk cannot be negative
            if (n.hat[k+1] < 0) {
              n.hat[k+1]<-0
              cen[k]<-n.hat[k] - d[k]
            }
          }
          if (d[k] != 0) last<-k
        }
        sumd<- sum(d[1:upper[n.int]])
      }
    }
  }
  
#DT # THIS NEXT LINe MAY NEED TO GO BACK IN ###########
#write.table(matrix(c(t.S,n.hat[1:n.t],d,cen),ncol=4,byrow=F), file.path(out.path, KMdatafile),sep="\t")
  ### Now form IPD ###
  #Initialise vectors
  t.IPD<-rep(t.S[n.t],n.risk[1])
  event.IPD<-rep(0,n.risk[1])
  #Write event time and event indicator (=1) for each event, as separate row in t.IPD and event.IPD
  k=1
  for (j in 1:n.t){
    if(d[j]!=0){
      t.IPD[k:(k+d[j]-1)]<- rep(t.S[j],d[j])
      event.IPD[k:(k+d[j]-1)]<- rep(1,d[j])
      k<-k+d[j]
    }
  }
  #Write censor time and event indicator (=0) for each censor, as separate row in t.IPD and event.IPD
  for (j in 1:(n.t-1)){
    if(cen[j]!=0){
      t.IPD[k:(k+cen[j]-1)]<- rep(((t.S[j]+t.S[j+1])/2),cen[j])
      event.IPD[k:(k+cen[j]-1)]<- rep(0,cen[j])
      k<-k+cen[j]
    }
  }
  #Output IPD
  IPD <- matrix(c(t.IPD,event.IPD,arm),ncol=3,byrow=F)
  write.table(IPD,file.path(out.path,KMdataIPDfile),sep="\t")
  #Find Kaplan-Meier estimates
  IPD <- as.data.frame(IPD)
  #KM.est<-survfit(Surv(IPD[,1],IPD[,2])~1,data=IPD,type="kaplan-meier",)
  #KM.est

  
#out.data.name[i] <- paste("s.data",i,sep="")
out.data.path[q] <- file.path(out.path,KMdataIPDfile)
#assign(out.data.name[i], read.table(out.data.path[i], header=TRUE))
  

if (q == 1) {
  sum.data <- read.table(out.data.path[q], header=TRUE)
} else if (q > 1) {
  sum.data <- merge(sum.data, read.table(out.data.path[q], header=TRUE), all = TRUE)
}


}

colnames(sum.data)[1] <- 'time'
colnames(sum.data)[2] <- 'fail'
colnames(sum.data)[3] <- 'arm'


#write.table(sum.data, file.path(out.path,"merged_data.txt"), sep="\t")
return(sum.data)
}