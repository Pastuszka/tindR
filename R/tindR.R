#'@title Swipe on the columns of your data frame
#'
#'@description Select columns from a data frame in an interactive manner
#'
#'@param df the data frame to select columns from
#'@param by optional column to group by
#'
#'@importFrom graphics boxplot plot
#'
#'@export
tindR <- function(df, by=NULL){
  stopifnot(is.data.frame(df))
  dropvector <- c()
  if(!is.null(by)){
    if(is.character(by)){
      by <- which(colnames(df) == by)
    }
  }
  for(i in 1:ncol(df)){
    if(i == by){
      dropvector <- c(dropvector, i)
      next
    }
    if(is.numeric(df[,i])){
      if(!is.null(by)){
        if(is.factor(df[,by])){
          boxplot(df[,i] ~ df[,by])
        }else {
          plot(df[,i] ~ df[,by])
        }
      }else{
        boxplot(df[,i])
      }
    }
    print(colnames(df)[i])
    print((summary(df[,i])))
    doKeep <- readline(prompt = "Keep this column? [y/n]")
    doKeep <- tolower(doKeep)
    if(doKeep %in% c("y", "yes", "yea", "tak", "ja", "hai", "ja, naturlich", "yup", "defo", "definitely", "of course",
                     "da", "si", "oui")){
      dropvector <- c(dropvector, i)
    }
  }
  df[,dropvector]
}
