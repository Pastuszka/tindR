#'@title Swipe on the columns of your data frame
#'
#'@description Select columns from a data frame in an interactive manner
#'
#'@params df the data frame to select columns from
#'
#'
#'@export
tindR <- function(df){
  stopifnot(is.data.frame(df))
  dropvector <- c()
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])){
      boxplot(df[,i])
    }
    print(summary(df[,i]))
    doDrop <- readline(prompt = "Keep this column? [y/n]")
    doDrop <- tolower(doDrop)
    if(doDrop == "n"){
      dropvector <- c(dropvector, i)
    }
  }
  if(length(dropvector == 0)){
    dropvector <- 1:ncol(df)
  }
  df[,dropvector]
}
