converter <- function(data){
  grey_color_data <- data[, , 1]/255
  
  # Removing the splits
  grey_color_data <- grey_color_data[apply(X = grey_color_data, MARGIN = 1, FUN = function(x){!all(x == 0)}),]
  grey_color_data <- grey_color_data[,apply(X = grey_color_data, MARGIN = 2, FUN = function(x){!all(x == 0)})]
  
  # Assumes the MNIST 28x28 pixel block
  grey_color_data_row <- split.data.frame(x = grey_color_data, f = (as.numeric(1:nrow(grey_color_data))-1) %/% 28)
  
  grey_color_data_split <- lapply(X = grey_color_data_row, FUN = function(x){
    lapply(split.data.frame(x = t(x), f = (as.numeric(1:nrow(t(x)))-1) %/% 28), FUN = t)
  })
  
  grey_color_data_list <- lapply(grey_color_data_split, FUN = function(x){
    lapply(x, FUN = function(y){
      matrix(y, nrow = 1, byrow = TRUE, 
             dimnames = list(NULL, 
                             as.vector(matrix(sapply(X = paste0("x", 1:28), 
                                                     FUN = function(x){
                                                       paste(x, 1:28, sep = "_")
                                                     }), 
                                              nrow = 1))))
    })
  })
  
  for(i in 1:length(grey_color_data_list)){
    grey_color_data_list[[i]] <- rbind.fill.matrix(lapply(grey_color_data_list[[i]], FUN = function(x){cbind(x, y = i-1)}))
  }
  
  raw_data <- rbind.fill.matrix(grey_color_data_list)

  return(raw_data)  
}




