
#prcp=Dat$Precipitation
#Nb=10

CDD_function=function(prcp,t,Nb) {
  
  prcp[length(prcp)+1]=10
  prcp[is.na(prcp)]=0
  spell <- ifelse(prcp > 1, 0, 0)
  SPELL=0
  for(i in 1:length(prcp)){
    if(prcp[i]<=t){
      SPELL=SPELL+1
      spell[i]=SPELL
    }
    else{
      SPELL=0
    }
  }
  
aa=as.data.frame(spell)

All=aa
CDD<-All  

n=length(CDD$spell)-1
for (i in 2:n) {
  if(All$spell[i+1]==0 & All$spell[i-1]>0){
    #print(All$spell[i])
  }else{
    CDD$spell[i]=0}
}


   CDD=cbind(CDD,aa$spell)
   names(CDD)[2]="Spell2"
   CDD$spell[1]=0
   CDD$Count<-ifelse(CDD$spell>=Nb,1,0)
   S=max(CDD$spell)
   return(S)
}

