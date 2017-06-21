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
#' Creates the input for the function life.table. A single individual approach is supported as well
#' as already pooled data (e. g. from an already existing life table). In the latter case, the user
#' has to specify a numerical variable (dec) which defines the count of each age class. To get from time
#' spans of individuals or groups of individuals to discrete age classes, the time span is exploded to
#' single years; the individual values for these years are then pooled together to whichever age
#' class the user chose initially (via parameter methode). If the data set comprises a grouping variable (e.g., sex),
#' this can be specified as well (parameter grnam).
#'
#' @param x a dataframe
#' @param dec Column name of the count of deceased (as character)
#' @param agebeg Column name of the from field (as character)
#' @param ageend Column name of the to field (as character)
#' @param grnam Column name of the grouping field (as character)
#' @param methode name of the age class determination (as character) Options: "Standard" (default) (1,4,5,5,...), "Equal5" (5,5,...)
#' @param age.range is ageend included or excluded in the age range: Included means for an age range 20 to 39 that the year 39 is part of this age range. The same result will be obtained by choosing the option excluded, if the data is provided as, for example 20 to 40; excluded is the default
#'
#' @return list as input parameter for the function life.table
#'
#' @examples
#'
#' #test2=prep.life.table(x=test1,dec="NA",agebeg="from",
#' #ageend="to",grnam="Geschlecht", methode="Standard")
#'
#' @export
prep.life.table=function(x,dec="NA",agebeg,ageend,grnam="NA",methode="NA", age.range= "included"){
  asd=x
  # Ask if "dec" is set / if a count of deceased people exist
  # Otherwise for each row one deceased person is assumed
  if(dec=="NA"){
    asd$cof=rep(1,dim(asd)[1])
  }else{
    names(asd)[which(names(asd)==dec)]="cof"
  }

  # Change the names of agebeg and ageend for further processes to "beg" and "ende"
  names(asd)[which(names(asd)==agebeg)]="beg"
  names(asd)[which(names(asd)==ageend)]="ende"

  # Filter potential NA values from the begin or end column
  # asd=asd %>% filter(!is.na(beg), !is.na(ende))
  asd=asd[!is.na(asd$beg),]
  asd=asd[!is.na(asd$ende),]

  # defines if the max of the age ranges should be included or excluded
  if(age.range == "excluded"){
    asd$ende = asd$ende -1
  }

  ## Choosing which method should be used to combine different ages to classes
  if(is.character(methode)){
    if(methode=="Standard"){
      meth=c(1,4)
      while(sum(meth)<max(asd$ende,na.rm=T)){
        meth=c(meth,5)
      }
    }else if(methode=="Equal5"){
      meth=rep(5,ceiling(max(asd$ende,na.rm=T)/5))
    }else{
      # If no selection is take for method => use standard method
      meth=c(1,4)
      while(sum(meth)<max(asd$ende,na.rm=T)){
        meth=c(meth,5)
      }
    }
  }else{
    # If the "methode" is not a character and of length 1 use the value as age class size and repeat
    if(length(methode)==1){
      meth=rep(methode,ceiling(max(asd$ende,na.rm=T)/methode))
    # If the "methode" value is not of length 1 take the entry of "methode" as method
    }else{
      meth=methode
    }
  }

  ## Using Group Argument (male, female, whatever, ...) to subset data into several groups if it is set
  if(grnam!="NA"){
    # Change the names of grnam for further processes to "Group"
    names(asd)[which(names(asd)==grnam)]="Group"

    # Create a dataframe (restab) filled with zeros
    # with the column count of the grouping columns (+2)
    # and the row count of the maximum age (+1)
    remat=matrix(data=0,ncol=length(unique(asd$Group))+2,nrow=(max(asd$ende,na.rm=T)+1))
    restab=as.data.frame(remat)
    # Set the dataframes column names to age, the groups names and all
    names(restab)=c("Age",as.character(unique(asd$Group)),"All")
    # Set the age values to 0 to the maximum age +1
    restab$Age=seq(0,max(asd$ende,na.rm=T),1)

    # For each Group (k) the deaths per age class (available years i) are summed up equally seperated by ages
    for(k in 1:length(unique(asd$Group))){
      for(i in which(asd$Group==unique(asd$Group)[k])){
        restab[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1)),(k+1)]=summe(x=(restab[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1)),(k+1)]),y=(asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1)))))
      }
    }
    # Also for all Groups all deceased are summed up seperated on the according years
    for(i in seq_along(asd[,1])){
      restab$All[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1))]=summe(x=(restab$All[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1))]),y=(asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1)))))
    }

  # If no groups (male, female, whatever, ...) are specified, do the same without considering groups
  }else{

    restab=data.frame(Age=seq(0,99,1),Deceased=0)

    for(i in seq_along(asd[,1])){
      restab$Deceased[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1))]=summe(x=(restab$Deceased[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1))]),y=(asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1)))))
    }
  }

  # For each element of "meth" (5,5,5,5,5,... or 1,4,5,5,5,... or whatever) the decease numbers of the ages (first 0-4 etc.) are summarized
  for(u in seq_along(meth)){
    if(u==1){
      output1=as.data.frame(t(colSums(restab[c(1:meth[u]),],na.rm =TRUE)))
    }else{
      v=u-1
      output1=rbind(output1,as.data.frame(t(colSums(restab[(c(cumsum(meth)[v]+1):cumsum(meth)[u]),],na.rm =TRUE))))
    }
  }
  # The ages of the "meth" breaks are added as column Age and the "meth" vector is added as column "a"
  output1$Age=cumsum(meth)-meth
  output1$a=meth

  # For the output of the function the table is converted to a list containing a dataframe for each group defined in the "Group" variable
  output=list(test=output1[,c("Age","a",colnames(output1)[2])])
  names(output[[1]])[3]="Dx"
  if((dim(output1)[2]-2)>1){
  for(u in 2:(dim(output1)[2]-2)){
    output[[u]]=output1[,c("Age","a",colnames(output1)[u+1])]
    names(output[[u]])[3]="Dx"
  }
  }
  names(output)=colnames(output1)[c(-1,-length(colnames(output1)))]

  return(output)
}
