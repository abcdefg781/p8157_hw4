library(data.table)
library(gee)
library(geepack)
library(lme4)
library(doBy)
library(tidyr)
library(mice)
library(purrr)
library(mitml)
library(CRTgeeDR)

obesity <- fread("./muscatine.dat")
colnames(obesity) <- c("id","gender","cohort","current_age","occasion","status")
obesity$gender <- as.factor(obesity$gender)
obesity$id <- as.factor(obesity$id)
# center age 
obesity$current_age <- obesity$current_age - min(obesity$current_age) 

table(obesity$status,useNA = "always")
table(obesity$occasion,obesity$status,useNA = "always")

# complete case analysis 
count <- obesity[,j = list(n=sum(!is.na(status))), by = "id"]
table(count$n)
count <- count[n==3]
obesity1 <- obesity[id %in% count$id]
table(obesity1$status,useNA = "always")
table(obesity1$occasion,obesity1$status,useNA = "always")
gee1 <- geeglm(status ~ gender + (current_age + I(current_age^2)) , id = id, data = obesity1, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee1)

# available case analysis 
obesity2 <- obesity
table(obesity2$status,useNA = "always")
table(obesity2$occasion,obesity2$status,useNA = "always")
gee2 <- geeglm(status ~ gender + (current_age + I(current_age^2)) , id = id, data = obesity2, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee2)

# LOCF
obesity3 <- lapply(unique(obesity$id), function(z){tidyr::fill(obesity[id == z], status)})
obesity3 <- rbindlist(obesity3)
table(obesity3$occasion,obesity3$status,useNA = "always")
gee3 <- geeglm(status ~ gender + (current_age + I(current_age^2)) , id = id, data = obesity3, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee3)

# MI 
obesity4 <- obesity
pred <- make.predictorMatrix(obesity4)
pred
pred["status", "id"] <- -2
pred
pred <- pred["status",,drop = FALSE]
pred
obesity4$id <- as.integer(obesity4$id)
imp <- mice(obesity4, method = "2l.bin", pred = pred, seed = 12102, maxit = 1, m = 5, print = FALSE, blocks = list(c("status")))
table(mice::complete(imp)$status, useNA = "always")

### GEE
implist <- mids2mitml.list(imp)
gee4 <- with(implist, geeglm(status ~ gender + (current_age + I(current_age^2)), id=id,family = binomial, corstr = "unstructured"))
testEstimates(gee4)

### Mixed Effects 
lme1 <- mice::complete(imp, "all") %>%
    purrr::map(lme4::glmer,
               formula = status ~ gender + (current_age + I(current_age^2)) + (1 | id),
               family = binomial) %>%
    pool() %>%
    summary()

# Weighted (IPW)
obesity5 <- obesity
obesity5$MISSING <- is.na(obesity5$status)*1
obesity5$gender <- as.integer(obesity5$gender) -1
obesity5[,status_1 := shift(status,type = "lag"), by = "id"]
gee5 <- geeDREstimation(status ~ gender + (current_age + I(current_age^2)), id="id",family = "binomial", corstr = "unstructured", data = obesity5,
                        model.weights=I(MISSING==0)~ gender * status_1, nameTRT = "gender")
summary(gee5)


