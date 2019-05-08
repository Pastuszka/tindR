#'@title Swipe on the columns of your data frame
#'
#'@description Select columns from a data frame in an interactive manner
#'
#'@param df the data frame to select columns from
#'
#'@export
tindR <- function(df){
  stopifnot(is.data.frame(df))
  dropvector <- c()
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])){
      boxplot(df[,i])
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
