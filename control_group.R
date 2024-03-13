# the objective is to create a control group
# the method will be propensity score matching
# logit model will be estimated using average revenue for the pre-COVID 19 years 
# r package called matching will be used
# guideline for implementation at
# https://cran.r-project.org/web/packages/Matching/index.html
# to create the potential treatment group 
# subset the identity data for all CMIE firms
# to get NIC 10 and 11 firms
# and subset out the non-pli firms (all categories)
# getting classification data for all companies
class_all <- read.table("class_all.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 332327)
# formatting the date in class_all
class_all[,3] <- format(as.Date(as.character(class_all[,3]), "%Y%m%d"), "%Y")
# taking only annual report based classification
class_all <- class_all[class_all$mr_info_full_name=="Annual Report",]
# inserting column of nic two digit code
nic_code_two_digit <- as.numeric(substr(as.character(class_all[,5]), 1, 2))
class_all <- cbind(class_all,nic_code_two_digit)
# subset the data for nic 10 and 11
# to get the potential control group
class_10.11 <- subset(class_all, nic_code_two_digit == "10" | nic_code_two_digit == "11", select = "coprd_company_code")
class_10.11 <- unique(class_10.11$coprd_company_code)
# saving the list of all beneficiaries with a cmie footprint
benificiary <- read.csv("./PLISFPI.csv")
beneficiary_cmie <- subset(benificiary, CMIE.Footprint == "Yes", select = c("Name.of.Company","Category","Prowess.Code"))
# write.csv(beneficiary_cmie,"./beneficiary_cmie.csv")
# excluding scheme beneficiaries from potential control group
d.row <- which(class_10.11 %in% beneficiary_cmie$Prowess.Code)
class_10.11 <- class_10.11[-d.row]
write.csv(class_10.11, file = "./10.11.csv")
# getting the data for the treatment group
# consolidated accounts
treat_ca <- read.table("treat_ca.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 310)
# cleaning the data
treat_ca[,3] <- format(as.Date(as.character(treat_ca[,3]), "%Y%m%d"), "%Y")
for(i in 7:16){
  treat_ca[,i] <- as.numeric(treat_ca[,i])
}
treat_ca <- treat_ca[,-c(4,5,14)]
# standalone accounts
treat_sa <- read.table("treat_sa.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 543)
# cleaning the data
treat_sa[,3] <- format(as.Date(as.character(treat_sa[,3]), "%Y%m%d"), "%Y")
for(i in 7:16){
  treat_sa[,i] <- as.numeric(treat_sa[,i])
}
treat_sa <- treat_sa[,-c(4,5,14)]
# merging sa and ca data
# wherever ca values are available they are taken
# preparations
# for ca
# creating id 
for (i in 1:nrow(treat_ca)){
treat_ca$id[i] <- paste(treat_ca[i,1], treat_ca[i,3], sep = "_")
}
# naming the columns similarly to merge the data
colnames(treat_ca) <- c("prowess_code", "company_name", "year", "sales", "export_incentive", "salary", "long_term_borrowings", "curr_portion_lt_borrowings", "capital","gross_fixed_ast_addn", "gross_intangible_assets_tot_addn", "cf_net", "export_goods","id")
# for sa
# creating id
for (i in 1:nrow(treat_sa)){
  treat_sa$id[i] <- paste(treat_sa[i,1], treat_sa[i,3], sep = "_")
}
colnames(treat_sa) <- colnames(treat_ca)
d.row <- which(treat_sa[,ncol(treat_sa)] %in% treat_ca[,ncol(treat_ca)])
treat <- merge(x = treat_sa[-d.row,], y = treat_ca, all.y = TRUE, all.x = TRUE)
# getting data for control group
# consolidated accounts
control_ca <- read.table("control_ca.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 2224)
# cleaning the data
control_ca[,3] <- format(as.Date(as.character(control_ca[,3]), "%Y%m%d"), "%Y")
for(i in 7:16){
  control_ca[,i] <- as.numeric(control_ca[,i])
}
control_ca <- control_ca[,-c(4,5,14)]
# standalone accounts
control_sa <- read.table("control_sa.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 11734)
# cleaning the data
control_sa[,3] <- format(as.Date(as.character(control_sa[,3]), "%Y%m%d"), "%Y")
for(i in 7:16){
  control_sa[,i] <- as.numeric(control_sa[,i])
}
control_sa <- control_sa[,-c(4,5,14)]
# merging sa and ca data
# wherever ca values are available they are taken
# preparations
# for ca
# creating id 
for (i in 1:nrow(control_ca)){
  control_ca$id[i] <- paste(control_ca[i,1], control_ca[i,3], sep = "_")
}
# naming the columns similarly to merge the data
colnames(control_ca) <- c("prowess_code", "company_name", "year", "sales", "export_incentive", "salary", "long_term_borrowings", "curr_portion_lt_borrowings", "capital","gross_fixed_ast_addn", "gross_intangible_assets_tot_addn", "cf_net", "export_goods","id")
# for sa
# creating id
for (i in 1:nrow(control_sa)){
  control_sa$id[i] <- paste(control_sa[i,1], control_sa[i,3], sep = "_")
}
colnames(control_sa) <- colnames(control_ca)
d.row <- which(control_sa[,ncol(control_sa)] %in% control_ca[,ncol(control_ca)])
control <- merge(x = control_sa[-d.row,], y = control_ca, all.y = TRUE, all.x = TRUE)
# strategy for selecting out of the potential control group
# for each category the Average sales will be calculated for FY16-20
# the effects of COVID pandemic in FY 21, so it is excluded
# for each category we select controls so that their AR is within 2 sds of the category AR
# the control group is weighted by category to keep it balanced
# weights mimic the treatment group
# adding category information to control group
treat <- merge(x = beneficiary_cmie, y = treat, by.x = "Prowess.Code", by.y = "prowess_code")
treat <- treat[,-2]
treat$sales <- as.numeric(treat$sales)
library(dplyr)
# data set with mean sales for the year FY16-FY20
treat_ar <- subset(treat, year == "2016" | year == "2017"| year == "2018"| year == "2019"| year == "2020") %>% group_by(Prowess.Code) %>%
  summarise(ar = mean(sales, na.rm = TRUE))
treat_ar <- merge(x = treat_ar, y = beneficiary_cmie)
# creating a data set for the cutoff values for control group selection
treat_ar_sum <- treat_ar %>%
  group_by(Category) %>%
  summarise(cutoff_ll = mean(ar)-2*sd(ar), cutoff_ul = mean(ar)+2*sd(ar))
# the table shows that the mean classification 
# will not give meaningful results
# creating data set with mean sales for FY16-20
# for the potential control group
control$sales <- as.numeric(control$sales)
control_ar <- subset(control, year == "2016" | year == "2017" |year == "2018"| year == "2019"| year == "2020") %>%
  group_by(prowess_code) %>%
  summarise(ar = mean(sales, na.rm = TRUE))
# getting rid of data with NaN
control_ar <- subset(control_ar, is.nan(ar) != TRUE)
control_ar <- mutate(control_ar, treat = rep(0, nrow(control_ar)))
treat_ar <- mutate(treat_ar, treat = rep(1, nrow(treat_ar)))
treat_ar <- treat_ar[,-c(3,4)]
colnames(treat_ar)[1] <- "prowess_code"
ar <- rbind(treat_ar, control_ar)
# estimating logit for probability of treatment based on past revenues
glm1 <- glm(treat ~ ar, data = ar)
# using the logit model for matching
library(Matching)
rr1 <- Match(Tr = ar$treat, X = glm1$fitted, ties = FALSE)
# checking how good the matches are
MatchBalance(treat ~ ar, match.out = rr1, nboots = 1000, data = ar)
# appears to be a good match
# will get the final treatment and control groups at one place
did <- rbind(ar[rr1$index.treated,], ar[rr1$index.control,])
# getting the control group
control_did <- merge(x = control, y = subset(did, treat == 0), all.y = TRUE, all.x = FALSE)
# merging the data for treatment and control groups
# naming the columns similarly
colnames(treat)[1] <- "prowess_code"
# creating category column in control data set
control_did <- mutate(control_did, Category = rep(NA, nrow(control_did)))
treat <- mutate(treat, treat = rep(1, nrow(treat)))
# removing duplicate rows in control group
library(tidyverse)
control_did <- control_did[!duplicated(control_did$id),]
# removing columns in control that are not in treatment
control_did <- control_did[,-15]
# comparing the order of columns in control and treatment
treat <- treat[,match(colnames(control_did), colnames(treat))]
# merging treat and control
data_did <- rbind(treat, control_did)
# create a variable called post
# takes value 1 if FY 22,23
# takes 0 otherwise
for (i in 1:nrow(data_did)){
  if (data_did$year[i] == "2022" | data_did$year[i] == "2023"){
    data_did$post[i] <- 1
  }
  else 
    data_did$post[i] <- 0
}
# removing id variable as not required
data_did <- data_did[,-14]
# calculating investment
for (i in 1:nrow(data_did)){
  if(is.na(data_did$gross_intangible_assets_tot_addn[i]) == TRUE){
    data_did$inv[i] <- data_did$gross_fixed_ast_addn[i]
  }
  else 
    data_did$inv[i] <- data_did$gross_fixed_ast_addn[i] - data_did$gross_intangible_assets_tot_addn[i]
}
i.by.k <- data_did$inv/data_did$capital
s.by.k <- data_did$sales/data_did$capital
data_did$log.inv <- log(data_did$inv)
data_did$log.sales <- log(data_did$sales)
data_did$log.salary <- log(data_did$salary)
# running panel data did regressions
library(AER)
library(plm)
library(stargazer)
data_did <- pdata.frame(data_did, index = c("prowess_code", "year"))
# for investment
m1 <- plm(inv ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m1, vcov = vcovHC, type = "HC1")
m2 <- plm(inv ~ I(post*treat) + lag(sales), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m2, vcov = vcovHC, type = "HC1")
m3 <- plm(i.by.k ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m3, vcov = vcovHC, type = "HC1")
m4 <- plm(i.by.k ~ I(post*treat) + lag(s.by.k), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m4, vcov = vcovHC, type = "HC1")
m5 <- plm(log.inv ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m5, vcov = vcovHC, type = "HC1")
m6 <- plm(log.inv ~ I(post*treat) + lag(log.sales), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m6, vcov = vcovHC, type = "HC1")
rob_se_inv1 <- list(sqrt(diag(vcovHC(m1, type = "HC1"))),
                   sqrt(diag(vcovHC(m2, type = "HC1"))),
                   sqrt(diag(vcovHC(m3, type = "HC1"))))
                   
rob_se_inv2 <- list(sqrt(diag(vcovHC(m4, type = "HC1"))),
                    sqrt(diag(vcovHC(m5, type = "HC1"))),
                    sqrt(diag(vcovHC(m6, type = "HC1"))))                   
stargazer(m1, 
          m2, 
          m3, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_inv1,
          title = "Linear Panel Regression Models of Investment",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)"))
stargazer(m4, 
          m5, 
          m6, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_inv2,
          title = "Linear Panel Regression Models of Investment",
          model.numbers = FALSE,
          column.labels = c("(4)", "(5)", "(6)"))

# for salary
m7 <- plm(salary ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m7, vcov = vcovHC, type = "HC1")

m8 <- plm(salary ~ I(post*treat) + sales, 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m8, vcov = vcovHC, type = "HC1")
rob_se_salary1 <- list(sqrt(diag(vcovHC(m7, type = "HC1"))),
                      sqrt(diag(vcovHC(m8, type = "HC1"))))
stargazer(m7, 
          m8,
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_salary1,
          title = "Linear Panel Regression Models of Salary",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))

m9 <- plm(log(salary) ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m9, vcov = vcovHC, type = "HC1")

m10 <- plm(log(salary) ~ I(post*treat) + log(sales), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m10, vcov = vcovHC, type = "HC1")

rob_se_salary2 <- list(sqrt(diag(vcovHC(m9, type = "HC1"))),
                      sqrt(diag(vcovHC(m10, type = "HC1"))))
stargazer(m9,
          m10,
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_salary2,
          title = "Linear Panel Regression Models of Salary",
          model.numbers = FALSE,
          column.labels = c("(3)","(4)"))
# for cf

m11 <- plm(cf_net ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m11, vcov = vcovHC, type = "HC1")
m12 <- plm(cf_net ~ I(post*treat)+sales, 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m12, vcov = vcovHC, type = "HC1")

m13 <- plm(cf_net ~ I(post*treat)+log.sales, 
           data = data_did,
           index = c("prowess_code", "year"),
           method = "twoways",
           model = "within")
summary(m12, vcov = vcovHC, type = "HC1")

rob_se_cf <- list(sqrt(diag(vcovHC(m11, type = "HC1"))),
                  sqrt(diag(vcovHC(m12, type = "HC1"))),
                  sqrt(diag(vcovHC(m13, type = "HC1"))))

stargazer(m11, 
          m12,
          m13,
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_cf,
          title = "Linear Panel Regression Models of Cash Flows",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)","(3)"))

m14 <- plm(sales ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m14, vcov = vcovHC, type = "HC1")

m15 <- plm(log.sales ~ I(post*treat), 
           data = data_did,
           index = c("prowess_code", "year"),
           method = "twoways",
           model = "within")
summary(m15, vcov = vcovHC, type = "HC1")
rob_se_sales <- list(sqrt(diag(vcovHC(m14, type = "HC1"))),
                  sqrt(diag(vcovHC(m15, type = "HC1"))))
stargazer(m14, 
          m15, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_sales,
          title = "Linear Panel Regression Models of Sales",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))
# attempting to create a better matching set
# create matching set only for the trimmed treatment group
# trimming the top 10% from the treatment group
treat_ar <- subset(treat_ar,treat_ar$ar <= quantile(treat_ar$ar,0.98))
ar <- rbind(treat_ar, control_ar)
# estimating logit for probability of treatment based on past revenues
glm1 <- glm(treat ~ ar, data = ar)
# using the logit model for matching
library(Matching)
rr1 <- Match(Tr = ar$treat, X = glm1$fitted, ties = FALSE)
# checking how good the matches are
MatchBalance(treat ~ ar, match.out = rr1, nboots = 1000, data = ar)
# will get the final treatment and control groups at one place
did <- rbind(ar[rr1$index.treated,], ar[rr1$index.control,])
# getting the control group
control_did <- merge(x = control, y = subset(did, treat == 0), all.y = TRUE, all.x = FALSE)
# merging the data for treatment and control groups
# creating category column in control data set
control_did <- mutate(control_did, Category = rep(NA, nrow(control_did)))
# removing duplicate rows in control group
library(tidyverse)
control_did <- control_did[!duplicated(control_did$id),]
# removing columns in control that are not in treatment
control_did <- control_did[,-15]
# comparing the order of columns in control and treatment
treat <- treat[,match(colnames(control_did), colnames(treat))]
# merging treat and control
data_did <- rbind(treat, control_did)
# create a variable called post
# takes value 1 if FY 22,23
# takes 0 otherwise
for (i in 1:nrow(data_did)){
  if (data_did$year[i] == "2022" | data_did$year[i] == "2023"){
    data_did$post[i] <- 1
  }
  else 
    data_did$post[i] <- 0
}
# removing id variable as not required
data_did <- data_did[,-14]
# calculating investment
for (i in 1:nrow(data_did)){
  if(is.na(data_did$gross_intangible_assets_tot_addn[i]) == TRUE){
    data_did$inv[i] <- data_did$gross_fixed_ast_addn[i]
  }
  else 
    data_did$inv[i] <- data_did$gross_fixed_ast_addn[i] - data_did$gross_intangible_assets_tot_addn[i]
}
i.by.k <- data_did$inv/data_did$capital
s.by.k <- data_did$sales/data_did$capital
data_did$log.inv <- log(data_did$inv)
data_did$log.sales <- log(data_did$sales)
data_did$log.salary <- log(data_did$salary)
# running panel data did regressions
library(AER)
library(plm)
library(stargazer)
data_did <- pdata.frame(data_did, index = c("prowess_code", "year"))
# for investment
m1 <- plm(inv ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m1, vcov = vcovHC, type = "HC1")
m2 <- plm(inv ~ I(post*treat) + lag(sales), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m2, vcov = vcovHC, type = "HC1")
m3 <- plm(i.by.k ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m3, vcov = vcovHC, type = "HC1")
m4 <- plm(i.by.k ~ I(post*treat) + lag(s.by.k), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m4, vcov = vcovHC, type = "HC1")
m5 <- plm(log.inv ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m5, vcov = vcovHC, type = "HC1")
m6 <- plm(log.inv ~ I(post*treat) + lag(log.sales), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m6, vcov = vcovHC, type = "HC1")
rob_se_inv1 <- list(sqrt(diag(vcovHC(m1, type = "HC1"))),
                    sqrt(diag(vcovHC(m2, type = "HC1"))),
                    sqrt(diag(vcovHC(m3, type = "HC1"))))

rob_se_inv2 <- list(sqrt(diag(vcovHC(m4, type = "HC1"))),
                    sqrt(diag(vcovHC(m5, type = "HC1"))),
                    sqrt(diag(vcovHC(m6, type = "HC1"))))                   
stargazer(m1, 
          m2, 
          m3, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_inv1,
          title = "Linear Panel Regression Models of Investment",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)"))
stargazer(m4, 
          m5, 
          m6, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_inv2,
          title = "Linear Panel Regression Models of Investment",
          model.numbers = FALSE,
          column.labels = c("(4)", "(5)", "(6)"))

# for salary
m7 <- plm(salary ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m7, vcov = vcovHC, type = "HC1")

m8 <- plm(salary ~ I(post*treat) + sales, 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m8, vcov = vcovHC, type = "HC1")
rob_se_salary1 <- list(sqrt(diag(vcovHC(m7, type = "HC1"))),
                       sqrt(diag(vcovHC(m8, type = "HC1"))))
stargazer(m7, 
          m8,
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_salary1,
          title = "Linear Panel Regression Models of Salary",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))

m9 <- plm(log(salary) ~ I(post*treat), 
          data = data_did,
          index = c("prowess_code", "year"),
          method = "twoways",
          model = "within")
summary(m9, vcov = vcovHC, type = "HC1")

m10 <- plm(log(salary) ~ I(post*treat) + log(sales), 
           data = data_did,
           index = c("prowess_code", "year"),
           method = "twoways",
           model = "within")
summary(m10, vcov = vcovHC, type = "HC1")

rob_se_salary2 <- list(sqrt(diag(vcovHC(m9, type = "HC1"))),
                       sqrt(diag(vcovHC(m10, type = "HC1"))))
stargazer(m9,
          m10,
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_salary2,
          title = "Linear Panel Regression Models of Salary",
          model.numbers = FALSE,
          column.labels = c("(3)","(4)"))
# for cf

m11 <- plm(cf_net ~ I(post*treat), 
           data = data_did,
           index = c("prowess_code", "year"),
           method = "twoways",
           model = "within")
summary(m11, vcov = vcovHC, type = "HC1")
m12 <- plm(cf_net ~ I(post*treat)+sales, 
           data = data_did,
           index = c("prowess_code", "year"),
           method = "twoways",
           model = "within")
summary(m12, vcov = vcovHC, type = "HC1")

m13 <- plm(cf_net ~ I(post*treat)+log.sales, 
           data = data_did,
           index = c("prowess_code", "year"),
           method = "twoways",
           model = "within")
summary(m12, vcov = vcovHC, type = "HC1")

rob_se_cf <- list(sqrt(diag(vcovHC(m11, type = "HC1"))),
                  sqrt(diag(vcovHC(m12, type = "HC1"))),
                  sqrt(diag(vcovHC(m13, type = "HC1"))))

stargazer(m11, 
          m12,
          m13,
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_cf,
          title = "Linear Panel Regression Models of Cash Flows",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)","(3)"))

m14 <- plm(sales ~ I(post*treat), 
           data = data_did,
           index = c("prowess_code", "year"),
           method = "twoways",
           model = "within")
summary(m14, vcov = vcovHC, type = "HC1")

m15 <- plm(log.sales ~ I(post*treat), 
           data = data_did,
           index = c("prowess_code", "year"),
           method = "twoways",
           model = "within")
summary(m15, vcov = vcovHC, type = "HC1")
rob_se_sales <- list(sqrt(diag(vcovHC(m14, type = "HC1"))),
                     sqrt(diag(vcovHC(m15, type = "HC1"))))
stargazer(m14, 
          m15, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se_sales,
          title = "Linear Panel Regression Models of Sales",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))