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
#' Creates the input for the function life.table. An individual based approach is supported as well
#' as already pooled data (e. g. from an already existing life table). In the latter case, the user
#' has to specify a numerical variable (\code{dec}) which defines the count for each age class.
#' If no life table exists, this function will process a dataframe including age ranges of
#' individuals or groups of individuals to discreate age classes. The age range is spread to
#' single years. \code{agebeg} has to be specified for the beginning of an age range, as well
#' as \code{ageend} for the end of an age range. These values for single years has to be integrated
#' accroding to \code{methode} into age classes. If the data set comprises a grouping variable (e.g., sex),
#' this can be specified with \code{grnam}.
#'
#' @param x single dataframe containing sex age and quantity of deceased (individuals or group of individuals).
#' @param dec numeric vector or a column name (as character) of the count of deceased.
#' @param agebeg numeric vector or a column name (as character) for the beginning of an age range.
#' @param ageend numeric vector or a column name (as character) for the end of an age range.
#' @param grnam numeric vector or a column name (as character) of the grouping field (e.g., sex),
#' optional. Default setup is: \code{NA}.
#' @param methode character string, optional.Default options is \code{Standard}, which will create age classes beginning with 1 year,
#' up to 4 years, followed by steps of 5 years (1,4,5,5,...) until the maximum age is reached. \code{Equal5} will create age classes with an even distrubution with steps of 5 years (5,5,...) until the maximum age is reached.
#' @param age.range character string, optional. Default setup is: \code{excluded}.
#' If the age ranges from "20 to 40" and "40 to 60", \code{excluded} will exclude the year 40 from "20 to 40",
#' to  prevent overlapping age classes. \code{included} is for age ranges like "20 to 39"
#' where the year 39 is meant to be counted.
#'
#' @return A list of input parameter needed for the function \code{life.table}.
#'
#' \itemize{
#'   \item \bold{x} or \bold{Age}:  age interval
#'   \item \bold{a}:  years within x
#'   \item \bold{Dx}: number of deaths within \bold{x}
#' }
#'
#' @examples
#' # to separate age ranges in you data set (requires magrittr, dplyr and tidyr)
#'  df <- dplyr::mutate(tidyr::separate(replace(magdalenenberg, magdalenenberg=="60-x", "60-69"),
#'          a, c("from", "to")),from = as.numeric(from), to = as.numeric(to))
#'
#' # apply to a data set containing age ranges
#'  prep.life.table( df, dec = "Dx", agebeg = "from", ageend = "to",
#'                     methode = "Equal5", age.range = "included")
#'
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

  # add attribute "grname" to output, if grname is available
  # necessary for nice legend title in plots
  attr(output, "grnam") <- ifelse(grnam != "NA", grnam, NA)

  return(output)
}
