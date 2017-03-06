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