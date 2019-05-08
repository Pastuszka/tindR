#'@title Swipe on the columns of your data frame
#'
#'@description Select columns from a data frame in an interactive manner
#'
#'@param df the data frame to select columns from
#'@param by optional column to group by
#'
#'@export
tindR <- function(df, by=NULL){
  stopifnot(is.data.frame(df))
  dropvector <- c()
  if(!is.null(by)){
    par(mfrow=c(1,2))
    if(is.character(by)){
      by <- which(colnames(df) == by)
    }
  }
  for(i in 1:ncol(df)){
    if(i == by){
      dropvector <- c(dropvector, by)
      next
    }
    if(is.numeric(df[,i])){
      if(!is.null(by)){
        boxplot(df[,i] ~ df[,by])
        plot(df[,i], df[,by])
      }else{
        boxplot(df[,i])
        plot(df[,i])
      }
    }
    print((summary(df[,i])))
    doKeep <- readline(prompt = "Keep this column? [y/n]")
    doKeep <- tolower(doKeep)
    if(doKeep == "y"){
      dropvector <- c(dropvector, i)
    }
  }
  df[,dropvector]
}
