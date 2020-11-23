library(gee)
library(geepack)
library(data.table)

# Get and clean data 
xerop <- fread("./xerop.data")
description <- fread("./xerop_description.txt",skip = 2, header = FALSE, fill = TRUE)
labels <- description$V2
labels <- labels[labels!='']
labels <- gsub(" ","_",labels)
colnames(xerop) <- labels 


# add response at lag 1 
xerop[,respiratory_infection_1 := shift(respiratory_infection,n=1,type="lag", fill = NA), by = "ID"]
## transition probabilities 
tab1 <- table(xerop$respiratory_infection,xerop$respiratory_infection_1)
tab1
round(prop.table(tab1,margin = 1),2)

# association between xeropthalmia and respiratory function 
tab2 <- table(xerop$respiratory_infection,xerop$xerophthalmia)
round(prop.table(tab2,margin = 1),2)

# association between xeropthalmia and respiratory function stratified by previous response i.e. response at lag 1 
temp <- split(xerop,xerop$respiratory_infection_1)
tab3 <- lapply(temp, function(z){table(z$respiratory_infection,z$xerophthalmia)})
lapply(tab3, function(z){round(prop.table(z,margin = 1),2)})

model_lag_1 <- gee(respiratory_infection~ xerophthalmia * respiratory_infection_1, corstr = "independence",family = binomial("logit"), id = ID, data = xerop)
round(summary(model_lag_1)$coeff,2)

model_lag_1b <- gee(respiratory_infection~ xerophthalmia + respiratory_infection_1, corstr = "independence",family = binomial("logit"), id = ID, data = xerop)
round(summary(model_lag_1b)$coeff,2)

