
library(Matrix)
library(xgboost)
options(stringsAsFactors = F)

# Takes train and test data sets and returns
# both processed for the xgboost algorithm (and XGB_fit function)
# also returns the target label

XGB_Data_Process = function(train,test,TARGET,ID=NULL){
  
  y_label  =train[[TARGET]]
  
  train = train[, - which(names(train) %in% c(TARGET,ID))]
  test  = test[, which(names(test) %in% names(train))]
     
  ntrain = nrow(train)
  train_test = rbind(train, test)

  features = names(train)
  for (f in features) {
    if (class(train_test[[f]])=="character") {
      levels <- sort(unique(train_test[[f]]))
      train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
    }
  }
  
  train = train_test[1:ntrain,]
  test = train_test[(ntrain+1):nrow(train_test),]

 return(list(x_train= train,
             x_test = test,
             y_label = y_label)) 
}

# Takes train,test and required arguments and returns
# a list containing the xgboost object, the test predictions, the cv mean, the cv std error
# and the importance matrix
XGB_Fit =  function(test,train,y_label,xgparms,nfold,nrounds,feval=NULL,metric){
  
  train = xgb.DMatrix(as.matrix(train), label=y_label)
  test = xgb.DMatrix(as.matrix(test))
  
  run = xgb.cv(xgparms,
               train,
               early_stopping_rounds=25,
               print_every_n = 10,
               verbose= 1,
               maximize=FALSE,
               nfold=nfold, 
               nrounds=nrounds,
               feval =feval,
               metrics=metric)
  
  best_nrounds = run$best_iteration
  cv_mean = run$evaluation_log[best_nrounds,3]
  cv_std = run$evaluation_log[best_nrounds,4]
  xgb.object = xgb.train(xgparms, train, nrounds=best_nrounds)
  importance.mat = xgb.importance(names(train),xgb.object)
  
  test_preds  = predict(xgb.object,test)
  
  return(list(model.object = xgb.object,
              importance.matrix=importance.mat,
              error_mean= cv_mean,
              error_std = cv_std,
              test_preds = test_preds))
}