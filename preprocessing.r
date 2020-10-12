#Preprocessing supplement, good for data discovery: 

#rm(list=ls(all=TRUE))
library(knitr)
library(data.table)
library(dplyr)
library(rmarkdown)
library(xts)
library(plyr)
library(dplyr)
library(pander)
library(descr)
library(ztable)
library(knitr)
library(readxl)
library(ggplot2)
library(lubridate)
library(zoo) 
#library(likert)
library(reshape2)
library(RColorBrewer)
library(reshape2)
#library(scales)
library(grid)
library(gtable)
library(gridExtra)
library(haven)
library(cba)
library(blockcluster)
library(sampling)
library(splitstackshape)
library(imputeTS)
library(tidyr)
library(cluster)
library(fpc)
library(factoextra)
library(readr)
Avatars_Version_01 <- read_delim("C:/Users/Boura/Box Sync/CBMI-ADAM/CBMI Data Request/Tetrad/06 28 Avatars_Version 01.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
View(Avatars_Version_01)

#Create a continuous variable out of the discrete data: HEIGHT
#
set.seed(1234)
Avatars_Version_01$HEIGHT_CT<-0
for(i in 1 : nrow(Avatars_Version_01)){
  
  if(Avatars_Version_01$HEIGHT[i]=="176-185"){
    
    Avatars_Version_01$HEIGHT_CT<-runif(1,min=176, max=185)
    
  }else if(Avatars_Version_01$HEIGHT[i]=="161-168"){
  
    Avatars_Version_01$HEIGHT_CT<-runif(1,min=161, max=168)
  
  }else if(Avatars_Version_01$HEIGHT[i]=="150-160"){
    
    Avatars_Version_01$HEIGHT_CT<-runif(1,min=150, max=160)
  }else{
    
    Avatars_Version_01$HEIGHT_CT<-runif(1,min=169, max=176)
  }
}

#Create a continuous variable out of the discrete data: WEIGHT

Avatars_Version_01$WEIGHT_CT<-0
for(i in 1 : nrow(Avatars_Version_01)){
  
  if(Avatars_Version_01$WEIGHT[i]=="55-64"){
    
    Avatars_Version_01$WEIGHT_CT<-runif(1,min=55, max=64)
    
  }else if(Avatars_Version_01$WEIGHT[i]=="65-160"){
    
    Avatars_Version_01$WEIGHT_CT<-runif(1,min=65, max=160)
  
  }else{
  
    Avatars_Version_01$WEIGHT_CT<-runif(1,min=40, max=54)
  
  }
}

#Create a continuous variable out of the discrete data: AGE
#
Avatars_Version_01$AGE_CT<-0
for(i in 1 : nrow(Avatars_Version_01)){
  
  if(Avatars_Version_01$AGE[i]=="18-24"){
    
    Avatars_Version_01$AGE_CT[i]<-runif(1,min=18, max=24)
    
  }else if(Avatars_Version_01$AGE[i]=="25-44"){
    
    Avatars_Version_01$AGE_CT[i]<-runif(1,min=25, max=44)
  
  }else if(Avatars_Version_01$AGE[i]=="45-64"){
  
  Avatars_Version_01$AGE_CT[i]<-runif(1,min=45, max=64)
  
  }else{
    
    Avatars_Version_01$AGE_CT[i]<-runif(1,min=65, max=95)
  }
}
