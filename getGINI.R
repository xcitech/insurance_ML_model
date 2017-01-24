# Calculate Gini

get.GINI <- function(input            # The name of the input data set
                     ,py              # The name of the column containing the predicted values
                     ,y               # The name of the column containing the actual values
                     ,filter          # The indicee desired to score the data i.e. when train = '1' (training data)
                     ,split_ind       # The name of the column containing the filter
                     )
  {
  set.seed(1)   
  
  # Filter the data
  data <- input[which(input[,split_ind] == filter),]
  data$rand.unif <- runif(dim(data)[1])
  
  # Assign weight 1 to all observations
  data$w <- 1
  
  # Rank the data based on predictions
  data <- data[order(data[,py],data[,'rand.unif']),]
  
  test <- data
  
  #Accumulate w to calculate Gini
  for (i in 1:dim(test)[1]){
    if(i==1){test$cumm_w0[i] = 0 + test$w[i]}
    else{
      test$cumm_w0[i] <- test$cumm_w0[i-1] + test$w[i]
      
      }
    
    }

  # Calculate Gini
  a <- test[,y]*test$cumm_w0*test$w
  b <- test[,y]*test$w
  
  gini <- 1 - 2 / ( sum(test$w) - 1 )*( sum(test$w) - sum( a ) / sum( b ))
  
  print(paste("Estimated GINI on",filter,'is',round(gini,4),sep=' '))
  
}


# Example

# predictions <- data.frame(
#                    act=rgamma(40,shape=2,rate=1)
#                    ,pred=rgamma(40,shape=2.2,rate=1.1)+rnorm(40,0,5)
#                    ,split=c(rep('train',20),rep('val',20))
#                    )

# head(predictions)

# get.GINI(input=predictions, py='pred', y='act', filter='val', split_ind='split')

