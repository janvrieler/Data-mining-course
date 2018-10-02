

impurity <- function(x) {
  #calculates the impurity of the entered node
  vector_sum <- sum(x)
  length_x <- length(x)
  vector_sum / length(x) * (1 - vector_sum / length(x) )
}

impurity(y)


bestsplit <- function(x,y) {
  #order x and y to both be sorted
  z <- data.frame(x,y)
  z <- z[order(z$x),]
  
  #transform x and y to ordered vectors
  x_sorted <- as.vector(z[,1])
  y_sorted <- as.vector(z[,2])

  
  #calculate splitpoints
  x_splitpoints <- (x_sorted[1:length(x_sorted)-1]+x_sorted[2:length(x_sorted)])/2
  
  #variables record the best split
  best_split_value <- 0
  best_split_index <- 0
  
  #calculate splits for each split point
  for (i in c(1:length(x_splitpoints))) {
    # c is the number in possible splits in index i
    c <- x_splitpoints[i]
    
    #constructs splits based in a higher/lower value than a possible split value
    split_1 <- y_sorted[x_sorted <= c]
    split_2 <- y_sorted[x_sorted >= c]
    
    #impuruty reduction calculation in three steps
    imp_split_1 <- (length(split_1)/ length(x_sorted)) * impurity(split_1)
    imp_split_2 <- (length(split_2)/ length(x_sorted)) * impurity(split_2)
    split_value <- impurity(y_sorted) - (imp_split_1 + imp_split_2)
    
    #if the value of the current split exceeds the best value found, replace it
    if (split_value > best_split_value) {
      best_split_value <- split_value
      best_split_index <- i
    }
  }
  best_split_value
  
}

getbestattribute <- function(df, classindex) {
  
  #vector witg the best split value in each attribute
  best_split_att <- c()
  
  #for loop enters each attribute into the best split function and puts the results in the best_split_att
  for (i in c(1:length(df))) {
    if (i != classindex) {
      attribute <- bestsplit(df[,i], df[,classindex])
      best_split_att <- c(best_split_att, attribute)
    }
    else {
      best_split_att <- c(best_split_att, c(0))
    }
  }
  match(max(best_split_att), best_split_att)
}

tree.grow <- function(x, y, nmin, minleaf) {
  
}