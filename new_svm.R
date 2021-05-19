##Load_Package##
library(e1071)
library(caret)
getData <- function(){
  data("iris")
  dataDT <<- iris
  return(dataDT)
}
svm_fitness = function(string,xx,yy){
  inc = which(string == 1)
  
  if (sum(inc)==0)
    return(0)
  
  outcome <-"Species"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- svm(formula = fRpart, cross = 5,kernel = 'linear',cost = 2,
            gamma = .15,data = data)
  
  ####pred_model###
  
  pred_md = predict(DT,data)
  
  ##accuracy measure 
  
  mytab = table(pred_md,data$Species)
  
  accut = sum(diag(mytab))/sum(mytab)
  
  ##Fitness_Function
  
  fitness_function = 0.70*accut + 0.30*(sum(string == 1))^-1
  
  return(fitness_function)
   
  #return(accut)
}