install_github("bfastSpatial","mengluchu")
library("bfastSpatial")
bfmarray
bfmarray

setwd("C:/Users/m_lu0002/Dropbox/mengluchu/bfastSpatial2")
list.files()
load("a1.saved")
load("fevi8.Rdata")

fevi20b20<-fevi8[20:50,20:50,] # input example array
 
output1<-bfmarray(fevi20b20,dates=a1,aggre='month',start=9) # bfast monitor

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

#test bfast: save only time
t1<-fevi9b9[1:2,1:2,]
output3 <-bfaarray(t1,dates=a1,aggre='month',season="harmonic",max.iter=1,level=0.05)
dimt=23*12
dimx<-20
dimy<-20

newarray<-array(,c(dimx,dimy,dimt)) #newarr
 
mag<- array(100,c(20,20))
 
t3dbfaarr<-timeasdimbfa(newarray,output3,mag)
which(!is.na(t3dbfaarr),arr.ind=TRUE)
########################################### bfast with save other values
output2 <-bfaarray2(t1,dates=a1,aggre='month',season="harmonic",max.iter=1,level=0.05)
str(output2)
allbreakpoints<-output2[1:6,,] # breakpoint
allmagnitudes<-output2[7:12,,] # magnitude
which(allbreakpoints!=0)
 mtime<-allbreakpoints #timearr

dimx<-dim(output2)[2]
dimy<-dim(output2)[3]
dimt<-max(mtime[ mtime!=0])

newarray<-array(,c(dimx,dimy,dimt)) #newarr

mag<- allmagnitudes  # variablearr (magnitude)

t3darrbfamul<-tasd.bfa.mul(newarray,mtime,mag) # 3 dimensional array with magnitude 

which(!is.na(t3darrbfamul),arr.ind=TRUE)
image(t3darrbfamul[,,140]) #test: bfastmonitor magnitude
t3darrbfamul[1,17,140]
