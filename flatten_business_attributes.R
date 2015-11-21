
flatten.business.attributes <- function(business) {

  attributes <- business$attributes
  column.classes <- sapply(attributes, class)
  column.names <- colnames(attributes)

  flat.attributes.df <- data.frame(business_id = business$business_id)
  flat.col.number <- 2
  
  for(i in 1:length(column.classes)) {
    if(column.classes[[i]] == "list"|| column.classes[[i]] == "data.frame") {
      if(column.classes[[i]] == "data.frame") {
##        print(paste("processing data frame columns:",column.classes[i],column.names[i]))
        df <- attributes[,i]
        df.column.classes <- sapply(df, class)
        df.column.names <- colnames(df)
        for(j in 1:length(df.column.classes)) {
          if(df.column.classes[[j]] == "list" || df.column.classes[[j]] == "data.frame") {
          
          }
          else {
            new.col.name <- 
              paste(gsub(" ",".",column.names[i]),gsub(" ",".",df.column.names[j]),sep=".")
##            print(paste("adding data.frame column:",df.column.classes[j], new.col.name))
            flat.attributes.df <- cbind(flat.attributes.df,df[,j])
            colnames(flat.attributes.df)[flat.col.number] <- new.col.name
            flat.col.number <- flat.col.number + 1
          }
        }
      }
      else if(column.classes[[i]] == "list") {
##        print(paste("skipping:",column.classes[i],column.names[i]))
      }
    }

    else {
      new.col.name <- gsub(" ",".",column.names[i])
##      print(paste("adding:",column.classes[i],column.names[i]))
      flat.attributes.df <- cbind(flat.attributes.df,attributes[,i])
      colnames(flat.attributes.df)[flat.col.number] <- new.col.name
      flat.col.number <- flat.col.number + 1
    }
  }
  flat.attributes.df
}

