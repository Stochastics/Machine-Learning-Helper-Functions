# Machine-Learning-Helper-Functions


These are functions to aid in predictive modeling.

Example of how to use Loo.R

  df = cbind.data.frame(c=rnorm(50000),
  a = sample(c("a","b","c"),50000,T),
  target=rnorm(50000))
  
  df$fold_id = sample(1:4,nrow(df),T)
  
  out= LOOencoding(df,"a","runif(nrow(df),.98,1.05)")
