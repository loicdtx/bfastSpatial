install_github("bfastSpatial","mengluchu")
library("bfastSpatial")
bfmarray
bfmarray

setwd("C:/Users/m_lu0002/Dropbox/mengluchu/bfastSpatial2")
list.files()
load("a1.saved")
load("fevi8.Rdata")

fevi20b20<-fevi8[1:20,1:20,] # input example array
 
output1<-bfmarray(fevi9b9,dates=a1,aggre='month',start=9) # bfast monitor

## convert time to time dimension

mtime<-monthofyears(output1[1,,]) #timearr

dimx<-dim(output1)[2]
dimy<-dim(output1)[3]
dimt<-max(mtime[!is.na(mtime)])

newarray<-array(,c(dimx,dimy,dimt)) #newarr

mag<- output1[2,,]   # variablearr (magnitude)
 
#load("C:/Users/m_lu0002/Dropbox/mengluchu/bfast/app1.R")

t3darr<-timeasdim(newarray,mtime,mag) # 3 dimensional array with magnitude 

which(!is.na(t3darr),arr.ind=TRUE)
image(t3darr[,,176]) #test: bfastmonitor magnitude

#test bfast
output2 <-bfaarray(fevi9b9,dates=a1,aggre='month',season="harmonic",max.iter=3)
dimt=23*12
dimx<-20
dimy<-20
newarray<-array(,c(dimx,dimy,dimt)) #newarr
 
mag<- array(100,c(20,20))
 
t3dbfaarr<-timeasdimbfa(newarray,app1,mag)
which(!is.na(t3dbfaarr),arr.ind=TRUE)
 