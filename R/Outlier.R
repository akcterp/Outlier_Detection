#' outleir detection in dataset
#' @param values
#' @export

outlier_detection = function(data){
  outlier_single = function(x){
    if(is.numeric(x)){ #checks data type
      x_log = log(x) #taking log of the data
      q3 = quantile(x,0.75,na.rm = TRUE)
      q1 = quantile(x,0.25,na.rm = TRUE)
      #calculating the IQR for the log transformed data
      q3_log = quantile(x_log,0.75,na.rm = TRUE)
      q1_log = quantile(x_log,0.25,na.rm = TRUE)

      #calculating the boundaries
      upper_limit = q3 + IQR(x,na.rm = TRUE) * 1.5
      lower_limit = q1 - IQR(x,na.rm = TRUE) * 1.5
      upper_limit_log = q3_log + IQR(x_log,na.rm = TRUE) * 1.5
      lower_limit_log = q1_log - IQR(x_log,na.rm = TRUE) * 1.5

      #setting criteria for outlier detection
      x = na.omit(x)
      num_upper = sum(x > upper_limit)
      num_lower = sum(x < lower_limit)

      num_upper_log = sum(x_log > upper_limit_log)
      num_lower_log = sum(x_log < lower_limit_log)

      #combining the result together
      total = c(num_upper,num_lower,num_upper_log,num_lower_log)
      return(total)
    }
    #if not numeric then
    else{
      s = c('-','-','-','-')
      return(s)
    }

  }
  output = noquote(sapply(data,outlier_single))
  output = t(output)
  colnames(output) = c('Upper_Count', 'Lower_Count', 'Upper_Count_After_Log',
                       'Lower_Count_After_Log' )
  return(output)
}


fram <- read.csv("D:\\Praxis\\Term 2\\R\\framingham.csv")
outlier_detection(fram)
