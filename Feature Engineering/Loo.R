library(dplyr)
options(stringsAsFactors = F)

# Function to do 'leave one out encoding' 
# This type of encoding is useful if you have a very high cardinality categorical varible
# Can pre specifiy your desired number of folds
# Target must be named target in dataframe and CV fold fold_id
# Must specify the encoded feature by string
# Also must specify random noise added by a string, for example 'runif(nrow(df),0,)'

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
