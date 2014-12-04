timeasdim<-function(newarr,timearr,variablearr)
{
  for (x in 1:dim(timearr)[1])
  {
    for (y in 1: dim(timearr)[2])
      newarr[x,y,timearr[x,y]]<- variablearr[x,y]
  }
  return(newarr)
  
}  

monthofyears<-function(x)
{
  return(trunc(x)*12+round((x-trunc(x))*12))
}

dayofyears<-function(x,frequency)
{
  mdays <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
  return(trunc(x)*mdays[trunc(x)]*12+round((x-trunc(x))*frequency)*mdays[trunc(x)])
}
