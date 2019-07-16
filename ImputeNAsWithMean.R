## Create a Function That Can Impute NAs with Mean


# Creating a Print Function
PrintHello = function(name){
  hello = paste0("hello ", name)
  print(hello)
}

PrintHello("AFWERX NEXUS")

##--------------------------------------------------------------
# Creating a Return Function
GetPi = function(n=4){
  pi = round(3.141592,n)
  return(pi)
}

GetPi(5)

##--------------------------------------------------------------
# Creating a Real Function - Impute NAs With Mean
ImputeNAsWithMean = function(dataset){
  for(i in 1:ncol(dataset)){
    is_na = is.na(dataset[,i])
    if(any(is_na)){  ##will ring true if equals true, triggered by previous command which results in logical value (true or false)
      na_ids = which(is_na) 
      
      var_mean = mean(dataset[,i], na.rm = T)
      dataset[na_ids,i] = var_mean
      message = paste0("Replacing NA with mean of: ", colnames(dataset)[i])
      print(message)
    }
  }
  return(dataset)
}

CMPsubset = CMP[,c(1:4, 14:16)]
CMPsubset_imputed = ImputeNAsWithMean(CMPsubset)
View(CMPsubset_imputed)