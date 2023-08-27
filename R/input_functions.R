`%+0%` <- function(x, y) {
  x <- NA_to_0(x)
  y <- NA_to_0(y)
  return(x + y)
}

NA_to_0 <- function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- 0
  }
  return(x)
}

#' Creates the input for the function life.table
#'
#' Prepares the input for \code{life.table()}. An individual based approach is supported as well
#' as already pooled data (e. g. from an already existing life table). In the latter case, the user
#' has to specify a numerical variable (\bold{dec}) which defines the count for each age class.
#' If no life table exists, this function will process a dataframe including the age ranges of
#' individuals or groups of individuals to discrete the age classes. The age range is spread to
#' single years. \bold{agebeg} has to be specified for the beginning of an age range, as well
#' as \bold{ageend} for the end of an age range. If a data-set with year-known individuals is
#' used, \bold{ageend} can be omitted but then the parameter \bold{agerange} has to left on its
#' default value (\code{included}). The \bold{method} defines in which way the single years between
#' the different age classes are split. If the data set comprises a grouping variable (e.g., sex),
#' this can be specified with \bold{group}.
#'
#' @param x single dataframe containing sex age and quantity of deceased (individuals or group of individuals).
#' @param dec column name (as character) of the count of deceased, optional.
#' @param agebeg column name (as character) for the beginning of an age range.
#' @param ageend column name (as character) for the end of an age range, optional.
#' @param group column name (as character) of the grouping field (e.g., sex),
#' optional. Default setup is: \code{NA}.
#' @param method character string, optional. Default options is \code{Standard}, which will create age
#' classes beginning with 1 year, up to 4 years, followed by steps of 5 years (1,4,5,5,...) until the
#' maximum age is reached. \code{Equal5} will create age classes with an even distribution, stepped
#' by 5 years (5,5,...) until the maximum age is reached. If method is a single numeric, this number will be
#' repeated until the maximum age is reached. Thereby, it is possible to create a year-wise life table.
#' @param agerange character string, optional. Default setup is: \code{included}.
#' If the age ranges from "20 to 40" and "40 to 60", \code{excluded} will exclude the year 40 from "20 to 40",
#' to prevent overlapping age classes. \code{included} is for age ranges like "20 to 39"
#' where the year 39 is meant to be counted.
#'
#' @return A list of input parameter needed for the function \code{life.table}.
#'
#' \itemize{
#'   \item \bold{x} or \bold{Age}:  age interval.
#'   \item \bold{a}:  years within x.
#'   \item \bold{Dx}: number of deaths within \bold{x}.
#' }
#'
#' @examples
#' # Separate age ranges in your data set.
#' df <- dplyr::mutate(
#'   tidyr::separate(
#'     replace(
#'      magdalenenberg,
#'      magdalenenberg=="60-x", "60-69"
#'     ),
#'     a,
#'     c("from", "to")
#'   ),
#'   from = as.numeric(from),
#'   to = as.numeric(to)
#' )
#'
#' # Apply prep.life.table to a data set containing the age ranges.
#' magda_prep <- prep.life.table(
#'   df,
#'   dec = "Dx",
#'   agebeg = "from",
#'   ageend = "to",
#'   method = "Equal5",
#'   agerange = "included"
#' )
#'
#' # Create a life.table.
#' life.table(magda_prep)
#'
#' @export
prep.life.table=function(x, dec = NA, agebeg, ageend = NA, group = NA, method = "Standard", agerange= "included"){

  asd <- data.frame(x)

  # Ask if "dec" is set / if a count of deceased people exists.
  # Otherwise one deceased person is assumed for each row.
  if (is.na(dec)) {
    asd$cof=rep(1,dim(asd)[1])
  } else {
    names(asd)[which(names(asd)==dec)]="cof"
  }

  # Change the names of agebeg and ageend for further processes to "beg" and "ende".
  names(asd)[which(names(asd)==agebeg)] <- "beg"
  if (!is.na(ageend)) {
    names(asd)[which(names(asd)==ageend)] <- "ende"
  } else {
    asd$ende <- asd$beg
  }

  # Filters potential NA values from the begin or end column.
  # asd=asd %>% filter(!is.na(beg), !is.na(ende))
  asd=asd[!is.na(asd$beg),]
  asd=asd[!is.na(asd$ende),]

  # Defines if the max of the age ranges should be included or excluded.
  if(agerange == "excluded"){
    asd$ende = asd$ende -1
  }

  ## Choosing which method should be used to combine different ages to classes.
  if(is.character(method)){
    if(method == "Equal5") {
      meth=rep(5,ceiling(max(asd$ende,na.rm=T)/5))
    } else {
      # If no selection is taken for method => use the standard method.
      meth=c(1,4)
      while(sum(meth)<max(asd$ende,na.rm=T)){
        meth=c(meth,5)
      }
    }
  } else {
    # If the "method" is not a character and of length 1 use the value as age class size and repeat.
    if(length(method)==1){
      meth=rep(method,ceiling(max(asd$ende,na.rm=T)/method))
      # If the "method" value is not of length 1 take the entry of "method" as method.
    }else{
      meth=method
    }
  }

  ## Using a Group Argument (male, female, phase, ...) to subset data into several groups, if it is set.
  if(!is.na(group)){
    # Change the names of group for further processes to "Group".
    names(asd)[which(names(asd)==group)]="Group"

    # Create a dataframe (restab) filled with zeros,
    # with the column count of the grouping columns (+2)
    # and the row count of the maximum age (+1).
    remat=matrix(data=0,ncol=length(unique(asd$Group))+2,nrow=(max(asd$ende,na.rm=T)+1))
    restab=as.data.frame(remat)
    # Set the dataframes column names to age, the groups names and "All".
    names(restab)=c("Age",as.character(unique(asd$Group)),"All")
    # Set the age values from 0 to the maximum age +1.
    restab$Age=seq(0,max(asd$ende,na.rm=T),1)

    # For each Group (k) the deaths per age class (available years i) are summed up equally separated by ages.
    for(k in 1:length(unique(asd$Group))){
      for(i in which(asd$Group==unique(asd$Group)[k])){
        restab[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1)),(k+1)] <-
          restab[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1)),(k+1)] %+0%
          (asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1))))
      }
    }
    # Also for all Groups all deceased are summed up separated according to the years.
    for(i in seq_along(asd[,1])){
      restab$All[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1))] <-
        (restab$All[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1))]) %+0%
        (asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1))))
    }

    # If no groups (male, female, phase, ...) are specified, do the same without considering groups.
  }else{

    restab=data.frame(Age=seq(0,99,1),Deceased=0)

    for(i in seq_along(asd[,1])){
      restab$Deceased[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1))] <-
        (restab$Deceased[is.element(restab$Age,seq(asd$beg[i],asd$ende[i],1))]) %+0%
        (asd$cof[i]/(length(seq(asd$beg[i],asd$ende[i],1))))
    }
  }

  # For each element of "meth" (5,5,5,5,5,... or 1,4,5,5,5,...) the deceased numbers of the ages (first 0-4 etc.) are summarized.
  for(u in seq_along(meth)){
    if(u==1){
      output1=as.data.frame(t(colSums(restab[c(1:meth[u]),],na.rm =TRUE)))
    }else{
      v=u-1
      output1=rbind(output1,as.data.frame(t(colSums(restab[(c(cumsum(meth)[v]+1):cumsum(meth)[u]),],na.rm =TRUE))))
    }
  }
  # The ages of the "meth" breaks are added as the column Age and the "meth" vector is added as column "a".
  #output1$Age=cumsum(meth)-meth
  output1$a=meth

  # For the output of the function the table is converted into a list, containing a dataframe for each group defined in the "Group" variable.
  output=list(test=output1[,c("a",colnames(output1)[2])])
  names(output[[1]])[2]="Dx"
  if((dim(output1)[2]-2)>1){
    for(u in 2:(dim(output1)[2]-2)){
      output[[u]]=output1[,c("a",colnames(output1)[u+1])]
      names(output[[u]])[2]="Dx"
    }
  }
  names(output)=colnames(output1)[c(-1,-length(colnames(output1)))]

  output <- lapply(output, function(x){
    lower <- c(0, x[, 'a'] %>% cumsum)[1:nrow(x)]
    upper <- x[, 'a'] %>% cumsum %>% `-`(1)
    xvec <- paste0(lower, "--", upper)
    x <- cbind(x = xvec, x)
    return(x)
  })

  # Add attribute "group" to output, if group is available.
  # It is necessary for a nice legend title in the plots.
  attr(output, "group") <- group

  return(output)
}
