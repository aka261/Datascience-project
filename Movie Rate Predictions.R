library(dslabs)
library(tidyverse)
data("movielens")


summary(movielens)

head(movielens)



movielens$year = as.numeric(movielens$year)

### checking for NA values
lapply(movielens,function(x) sum(is.na(x)))

## We come to know that year & title column has NA values
## Treating missing values with meadian 
movielens$title[is.na(movielens$title)] = median(movielens$title,na.rm = TRUE)
movielens$year[is.na(movielens$year)] = median(movielens$year,na.rm = TRUE)

glimpse(movielens)
hist(movielens$rating)
plot(movielens$genres)

movielens = movielens %>% select(-title,-genres)


library(car)

library(caTools)

split = sample.split(movielens,SplitRatio = 0.80)
split
train = subset(movielens,split=="TRUE")
test = subset(movielens,split=="FALSE")

fit_lm = lm(rating~., data = train)
sort(vif(fit_lm),decreasing = TRUE)

formula(fit_lm)

val_pred = predict(fit_lm,newdata = test)

errors_lm = test$rating-val_pred

errors_lm**2 %>% mean() %>% sqrt()



plot(fit_lm,1) ## residual vs fitted values --> non- linearity exists or not

plot(fit_lm,2) ## errors are normal or not

plot(fit_lm,3) ## varience is constant or not

plot(fit_lm,4) # ouliers in the data if cooks distance >1
