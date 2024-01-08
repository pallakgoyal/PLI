# will run a panel data regression 
# dependent variable is investment_it/capital_it-1
# explanatory variables
# cashflow_it/capital_it-1
# sales_it-1/capital_it-1
# debt_it/capital_it-1
# uncertainty_t-1
# pli_t
# first will load data from ca and sa 
# loading data from ca
ca <- read.table("./investment_cat1_ca.txt", header = T, sep = "|", na.strings = "", comment.char = "", quote = "\"", fill = F, nrows = 238)
# setting the right col type
ca[,1] <- as.character(ca[,1])
ca[,3] <- format(as.Date(as.character(ca[,3]), "%Y%m%d"), "%Y")
for (i in 6:12){
  ca[,i] <- as.numeric(ca[,i])
}
# imputing missing data
# imputing zeros for missing values of current portion of long term borrowings
for (i in 1:nrow(ca)){
  if(is.na(ca[i,8]) == TRUE){
    ca[i,8] <- 0
  }
}
# imputing zero for missing value of gross addition to gross fixed assets
for (i in 1:nrow(ca)){
  if(is.na(ca[i,10]) == TRUE){
    ca[i,10] <- 0
  }
}
# imputing zero for missing values of total additional gross intangible assets
for (i in 1:nrow(ca)){
  if(is.na(ca[i,11]) == TRUE){
    ca[i,11] <- 0
  }
}
# imputing other missing values with entity level averages
for (i in 6:ncol(ca)){
  ca[,i] <- ave(ca[[i]], ca$ca_finance1_cocode, FUN = function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
}
# inserting column for investment
for (i in 1:nrow(ca)){
  ca$investment[i] <- ca[i,10]-ca[i,11]
}
# inserting column for debt
for (i in 1:nrow(ca)){
  ca$debt[i] <- ca[i,7] + ca[i,8]
}
# getting rid of useless columns
ca <- ca[,-c(4,5,7,8,10,11)]
# loading data from sa
sa <- read.table("./investment_cat1_sa.txt", header = T, sep = "|", na.strings = "", comment.char = "", quote = "\"", fill = F, nrows = 387)
# setting the right col type
sa[,1] <- as.character(sa[,1])
sa[,3] <- format(as.Date(as.character(sa[,3]), "%Y%m%d"), "%Y")
for (i in 6:12){
  sa[,i] <- as.numeric(sa[,i])
}
# imputing missing data
# imputing zeros for missing values of current portion of long term borrowings
for (i in 1:nrow(sa)){
  if(is.na(sa[i,8]) == TRUE){
    sa[i,8] <- 0
  }
}
# imputing zero for missing value of gross addition to gross fixed assets
for (i in 1:nrow(sa)){
  if(is.na(sa[i,9]) == TRUE){
    sa[i,10] <- 0
  }
}
# imputing zero for missing values of total additional gross intangible assets
for (i in 1:nrow(sa)){
  if(is.na(sa[i,10]) == TRUE){
    sa[i,10] <- 0
  }
}
# imputing other missing values with entity level averages
for (i in 6:ncol(sa)){
  sa[,i] <- ave(sa[[i]], sa$sa_finance1_cocode, FUN = function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
}
# inserting column for investment
for (i in 1:nrow(sa)){
  sa$investment[i] <- sa[i,9]-sa[i,10]
}
# inserting column for debt
for (i in 1:nrow(sa)){
  sa$debt[i] <- sa[i,7] + sa[i,8]
}
# getting ride of useless column in sa
sa <- sa[,-c(4,5,7,8,9,10)]
# will merge sa and ca data
# will keep ca data wherever availale and fill the rest with sa data
# creating new id for ca 
for (i in 1:nrow(ca)){
  ca$id[i] <- paste(ca[i,1], ca[i,3], sep = "_")
}
# creating new id for sa
for (i in 1:nrow(sa)){
  sa$id[i] <- paste(sa[i,1], sa[i,3], sep = "_")
}
# using id to find where ca entries are appearing in sa
d.row <- which(sa$id %in% ca$id)
# naming the columns of sa and ca similarly
colnames(ca) <- c("prowess_code","company_name","year","sales","capital","cashflow","investment","debt","id")
colnames(sa) <- colnames(ca)
# merging the data
bs <- rbind(sa[-d.row,-9],ca[,-9])
# generating lagged value of capital and sales 
library(dplyr)
bs <- bs %>%
  group_by(prowess_code) %>%
  mutate(capital_1 = dplyr::lag(x = capital, n = 1, order_by = year), sales_1 = dplyr::lag(x = sales, n = 1, order_by = year), investment_1 = dplyr::lag(x = investment, n = 1, order_by = year), capital_2 = dplyr::lag(x = capital, n = 2, order_by = year))
# rearranging the data to get the group observations together arranged by year
bs <- bs %>%
  arrange(prowess_code, year)
# generating dependent and independent variables
# investment_it/capital_it-1 <- i.by.k_1
# cashflow_it/capital_it-1 <- cf.by.k_1
# sales_it-1/capital_it-1 <- s.by.k_1
# debt_it/capital_it-1 <- d.by.k_1
# investment_it-1/capital_it-2 <- i_1.by.k_2
bs <- bs %>%
  group_by(prowess_code) %>%
  mutate(i.by.k_1 = investment/capital_1, cf.by.k_1 = cashflow/capital_1, s_1.by.k_1 = sales_1/capital_1, d.by.k_1 = debt/capital_1, i_1.by.k_2 = investment_1/capital_2, ds.by.k = (sales-sales_1)/capital)
# loading uncertainty index data
eui <- read.csv("./EUI_India.csv")
# creating annual uncertainty database
eui <- eui %>% 
  group_by(Year) %>%
  summarise(uncertainty = mean(India.News.Based.Policy.Uncertainty.Index))
colnames(eui)[1] <- "year"
# merging uncertainty data to bs
bs <- merge(x = bs, y = eui, by = "year", all.x = TRUE)
# rearranging the data for ease of analysis
bs <- bs %>%
  arrange(prowess_code, year)
# generating lagged variable of uncertainty
bs <- bs %>%
  group_by(prowess_code) %>%
  mutate(uncertainty_1 = dplyr::lag(x = uncertainty, order_by = year))
# generating PLI dummy
PLI <- NULL
for(i in 1:nrow(bs)){
  if (bs$year[i] == "2022" || bs$year[i] == "2023"){
    PLI[i] <- 1
  }
  else
    PLI[i] <- 0
}
bs <- cbind(bs,PLI)
colnames(bs)[21] <- "pli"
# adding repo rate data
repo_rate <- read.csv("./repo_rate.csv")
repo_rate[,1] <- format(base::as.Date(as.character(repo_rate[,1]), "%d-%m-%Y"), "%Y")
# "-" represents no change in value 
# so adding the same value as previous value in place of "-"
for (i in 1:nrow(repo_rate)){
  if (repo_rate[i,2] == "-"){
    repo_rate[i,2] <- repo_rate[i-1,2]
  }
}
repo_rate[,2] <- as.numeric(repo_rate[,2])
repo_rate <- repo_rate %>%
  group_by(date) %>%
  summarise(repo_rate = mean(repo_rate, na.rm = TRUE))
# addding repo_rate as a proxy for cost of capital to bs
colnames(repo_rate)[1] <- "year"
bs <- merge(x = bs, y= repo_rate, by = "year", all.x = TRUE)
# rearranging the data for ease of analysis
bs <- bs %>%
  arrange(prowess_code, year)
# creating a variable 
# change in repo_rate
bs <- bs %>%
  group_by(prowess_code) %>%
  mutate(d.repo_rate = repo_rate - dplyr::lag(x = repo_rate, order_by = year))
# running the panel data regression
library(AER)
library(plm)
library(stargazer)
# declaring the data as panel data
bs <- pdata.frame(bs, index = c("prowess_code","year"))
plm_model1 <- plm(i.by.k_1 ~ cf.by.k_1 + s_1.by.k_1 + d.by.k_1 + uncertainty_1 + repo_rate + pli, 
                  data = bs,
                  index = c("prowess_code", "year"),
                  model = "within")
summary(plm_model1, vcov = vcovHC(plm_model1))
pwartest(plm_model1)
plm_model2 <- plm(i.by.k_1 ~ cf.by.k_1 + ds.by.k + d.by.k_1 + uncertainty_1 + d.repo_rate + pli, 
                  data = bs,
                  index = c("prowess_code", "year"),
                  model = "within")
summary(plm_model2, vcov = vcovHC(plm_model2))
pwartest(plm_model2)
rob_se <- list(sqrt(diag(vcovHC(pli_model1, type = "HC1"))),
               sqrt(diag(vcovHC(pli_model2, type = "HC1"))))
stargazer(plm_model1, 
          plm_model2, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se,
          title = "Linear Panel Regression Models of effect of PLI scheme on Category 1 beneficiary investment",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))
# reference studies use gmm
# and also i_1.by.k_2 as an explanatory variable
# but small sample size here means that gmm will give singular variance covariance matrix
dpgmm_model1 <- pgmm(i.by.k_1 ~ i_1.by.k_2 + cf.by.k_1 + s_1.by.k_1 + d.by.k_1 + uncertainty_1 + repo_rate + pli | lag(i_1.by.k_2,2:5),
                   data = bs,
                   effect = "individual",
                   model = "twosteps",
                   transformation = "ld")
# we are getting a singular variance covariance matrix because the data set is small.
# the results are still going to be consistent
# AR(1) should be rejected as we expect serial autocorrelation 
# AR(2) should not be rejected as we expect no serial autocorrelation here
# basically the null AR(n) is that there is no serial autocorrelation
summary(dpgmm_model1, robust = TRUE)
dpgmm_model2 <- pgmm(i.by.k_1 ~ i_1.by.k_2 + cf.by.k_1 + ds.by.k + d.by.k_1 + uncertainty_1 + d.repo_rate + pli | lag(i_1.by.k_2, 2:5),
                     data = bs,
                     effect = "individual",
                     model = "twosteps",
                     transformation = "ld")
summary(dpgmm_model2, robust = TRUE)
rob_se1 <- list(sqrt(diag(vcovHC(dpgmm_model1, type = "HC1"))),
               sqrt(diag(vcovHC(dpgmm_model2, type = "HC1"))))
stargazer(dpgmm_model1, 
          dpgmm_model2, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se1,
          title = "GMM Panel Regression Models of effect of PLI scheme on Category 1 beneficiary investment",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))