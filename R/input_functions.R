#https://github.com/ISAAKiel/mortAAR

if(F){
if(!require('devtools')) install.packages('devtools')
library(devtools)
install_github('ISAAKiel/mortAAR')
}

setwd("C:\\Users\\Wolfgang\\Documents\\ISAAKiel\\Mortar")
alter=read.delim("Siedlungsbestattungen_ueberblick.txt")



#This function resolves a field (RangeOfAgeFieldName) with the format "1-12" to two fields with the numeric values 1 and 12 
function1=function(dataframe,RangeOfAgeFieldName){
 asd=dataframe
 names(asd)[which(names(asd)==RangeOfAgeFieldName)]="Range"
 asd$beg=NA
 asd$ende=NA
 
  for(i in seq_along(asd$Range)){
   asd$beg[i]=as.numeric(strsplit(as.character(asd$Range[i]),"-")[[1]][1])
   asd$ende[i]=as.numeric(strsplit(as.character(asd$Range[i]),"-")[[1]][2])
   }
   names(asd)[which(names(asd)=="Range")]=RangeOfAgeFieldName
   return(asd)
  }

asd2=function1(alter,"Ausdr2"); head(asd2)

#This function uses the adition function "+" to combine two variables setting na values to zero; this is neccesary since the sum function (which could use na.rm=T) sums up the values of a vektors input 
summe=function(x,y){
if(any(is.na(x))){
x[is.na(x)]=0
}
return(x+y)
}




#This function takes the count of funerals for a range of ages and produces a with the mean deaths per age (same as function2) / thereby it is possible to select a dependency on the Group
function3=function(dataframe,CountOfFuneralsFieldName,BeginOfAgeFieldName,EndOfAgeFieldName,Group=TRUE,GroupName="NA",methode){
 asd=dataframe
 names(asd)[which(names(asd)==CountOfFuneralsFieldName)]="cof"
 names(asd)[which(names(asd)==BeginOfAgeFieldName)]="beg"
 names(asd)[which(names(asd)==EndOfAgeFieldName)]="ende"

##
if(is.character(methode)){
if(methode=="Standard"){
meth=c(1,4)
while(sum(meth)<max(asd$ende)){
meth=c(meth,5)
}
}else if(methode=="Equal5"){
meth=rep(5,ceiling(max(asd$ende)/5))
}else{
meth=c(1,4)
while(sum(meth)<max(asd$ende)){
meth=c(meth,5)
}
}else{
if(length(methode)==1){
meth=rep(methode,ceiling(max(asd$ende)/methode))
}else{
meth=methode
}
}
 

##
if(Group==TRUE){
 names(asd)[which(names(asd)==GroupName)]="Group"

remat=matrix(data=0,ncol=length(unique(asd$Group))+2,nrow=(max(asd$ende)+1)) 
restab=as.data.frame(remat)
names(restab)=c("Alter",as.character(unique(asd$Group)),"All")
restab$Alter=seq(0,max(asd$ende),1)

 
for(k in 1:length(unique(asd$Group))){
 for(i in which(asd$Group==unique(asd$Group)[k])){
  restab[is.element(restab$Alter,seq(asd$beg[i],asd$ende[i],1)),(k+1)]=summe(x=(restab[is.element(restab$Alter,seq(asd$beg[i],asd$ende[i],1)),(k+1)]),y=(asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1)))))
  }
}
 for(i in seq_along(asd[,1])){
  restab$All[is.element(restab$Alter,seq(asd$beg[i],asd$ende[i],1))]=summe(x=(restab$All[is.element(restab$Alter,seq(asd$beg[i],asd$ende[i],1))]),y=(asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1)))))
  }
  }else{

restab=data.frame(Alter=seq(0,99,1),Bestattungen=0)

 for(i in seq_along(asd[,1])){
  restab$Bestattungen[is.element(restab$Alter,seq(asd$beg[i],asd$ende[i],1))]=summe(x=(restab$Bestattungen[is.element(restab$Alter,seq(asd$beg[i],asd$ende[i],1))]),y=(asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1)))))
  }
  }
  return(restab)
  }

asd4=function3(dataframe=asd2,CountOfFuneralsFieldName="Anzahl.von.Individuum_nr",BeginOfAgeFieldName="beg",EndOfAgeFieldName="ende",GroupName="Geschlecht_kombiniert");asd4

standard=c(1,4,5,5,5,5,5,5)
hermann=c(5,5,5,5,5,5)