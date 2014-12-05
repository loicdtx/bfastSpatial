timeasdim<-function(newarr,timearr,variablearr)
{
  for (x in 1:dim(timearr)[1])
  {
    for (y in 1: dim(timearr)[2])
     
      {
      if (!is.na(timearr[x,y]))
      newarr[x,y,timearr[x,y]]<- variablearr[x,y]
    }
    }
  return(newarr)
  
}  

timeasdimbfa<-function(newarr,timearr,variablearr) # for bfast
{
  for (x in 1:dim(timearr)[1])
  {
    for (y in 1: dim(timearr)[2])
      
    {
      nb<-length(unlist(timearr[x,y]))
     # if (  nb ==1)

      #  newarr[x,y,timearr[x,y]]<- variablearr[x,y]
      if ( nb >=1)
      {
        for(i in 1:nb)
        newarr[x,y,unlist(timearr[x,y])[i]]<- variablearr[x,y]   
    
      }
    }
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
