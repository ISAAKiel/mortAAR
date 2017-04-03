#' Uses the adition function "+" to combine two vectors setting NA values of the first vector to zero
#'
#' Uses the adition function "+" to combine two vectors setting NA values of the first vector to zero
#'
#' @param x a vector
#' @param y vector to combine with
#'
#' @return dataframe with additional columns "from" and "to"
#'
#' @examples
#'
#' #summentest=summe(c(1,2,NA,4),c(5,6,7,8)
summe=function(x,y){
  if(any(is.na(x))){
    x[is.na(x)]=0
  }
  return(x+y)
}


#' Creates the input for the function life.table
#'
#' Creates the input for the function life.table
#'
#' @param x a dataframe
#' @param dec Column name of the count of deceased (as character)
#' @param agebeg Column name of the from field (as character)
#' @param ageend Column name of the to field (as character)
#' @param grnam Column name of the grouping field (as character)
#' @param methode name of the age class determination (as character) Options: "Standard" (default) (1,4,5,5,...), "Equal5" (5,5,...)
#'
#' @return list as input parameter for the function life.table
#'
#' @examples
#'
#' #test2=prep.life.table(x=test1,dec="NA",agebeg="from",
#' #ageend="to",grnam="Geschlecht", methode="Standard")
#'
#' @export
prep.life.table=function(x,dec="NA",agebeg,ageend,grnam="NA",methode="NA"){
  asd=x
  if(dec=="NA"){
    asd$cof=rep(1,dim(asd)[1])
  }else{
    names(asd)[which(names(asd)==dec)]="cof"
  }
  names(asd)[which(names(asd)==agebeg)]="beg"
  names(asd)[which(names(asd)==ageend)]="ende"

  ## Choosing which mnethod should be applied
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
    }
  }else{
    if(length(methode)==1){
      meth=rep(methode,ceiling(max(asd$ende)/methode))
    }else{
      meth=methode
    }
  }

  ## Using Group Argument to subset data into several groups
  if(grnam!="NA"){
    names(asd)[which(names(asd)==grnam)]="Group"

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

  for(u in seq_along(meth)){
    if(u==1){
      output1=as.data.frame(t(colSums(restab[c(1:meth[u]),],na.rm =TRUE)))
    }else{
      v=u-1
      output1=rbind(output1,as.data.frame(t(colSums(restab[(c(cumsum(meth)[v]+1):cumsum(meth)[u]),],na.rm =TRUE))))
    }
  }
  output1$Alter=cumsum(meth)-meth
  output1$a=meth

  output=list(test=output1[,c("Alter","a",colnames(output1)[2])])
  names(output[[1]])[3]="Dx"
  for(u in 2:(dim(output1)[2]-2)){
    output[[u]]=output1[,c("Alter","a",colnames(output1)[u+1])]
    names(output[[u]])[3]="Dx"
  }
  names(output)=colnames(output1)[c(-1,-length(colnames(output1)))]

  return(output)
}
