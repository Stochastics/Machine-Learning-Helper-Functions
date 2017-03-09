library(dplyr)
options(stringsAsFactors = F)

#function to do 'leave one out encoding' 
#this type of encoding is useful if you have a very high cardinality categorical varible
#can pre specifiy your desired number of folds
#target must be named target in dataframe
#and cv fold id fold_id
#must specify the encoded feature by string
#also must specify random noise added by a string, for example 'runif(nrow(df),0,)'

LOOencoding = function(df,feature.name,noise.str){
  stopifnot(sum(names(df) %in% c('target','fold_id'))>1)
  stopifnot(is.numeric(df$target) )
  
  for(f in unique(df$fold_id)){    
    temp =  select(df,matches(feature.name),target,fold_id)
    temp = (temp %>%
              group_by_(feature.name)%>%
              filter(fold_id !=f) %>%
              summarise(target.rate= sum(target)/n()))
    
    mean.rate = (df %>% filter(fold_id !=f) %>%
                   summarise(mean.rate = sum(target)/n()))
    
    df = left_join(df,temp,by=feature.name)
    
    df[[paste(feature.name,"loo",f,sep="_")]] = 
      eval(parse(text=noise.str))*df$target.rate

    df[[paste(feature.name,"loo",f,sep="_")]] = 
      ifelse(df$fold_id==f,mean.rate,df[[paste(feature.name,"loo",f,sep="_")]])
    
    df = df[,-(which(names(df) %in% "target.rate"))]
    
  }
  return(df)
}



#Example how to use
df = cbind.data.frame(c=rnorm(50000),a = sample(c("a","b","c"),50000,T),target=rnorm(50000))
df$fold_id = sample(1:4,nrow(df),T)
out= LOOencoding(df,"a","runif(nrow(df),.98,1.05)")
