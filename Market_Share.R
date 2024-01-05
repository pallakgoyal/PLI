# The purpose of this file is to get the market share for the beneficiaries of the firms in categories 1 and 3 of the PLIFPI. The basis is going to be total sales of goods field in the consolidated annual financial statements. 

# Loading the identity details of the beneficiaries.
identity <- read.table("./identity.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 74)

# loading the history of classification of the beneficiaries.
class <- read.table("./class.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 2713)
# asking r to read the dates as such
class[,3] <- format(as.Date(as.character(class[,3]), "%Y%m%d"), "%Y")
# adding a column of nic codes at 2 digit level
nic_code_two_digit <- as.numeric(substr(as.character(class[,7]), 1, 2))
class <- cbind(class,nic_code_two_digit)
# taking only annual reports based classification
class <- class[class$mr_info_full_name=="Annual Report",]
# considering only the years 2017-2023
class <- class[class$coprd_date=="2017"|class$coprd_date=="2018"|class$coprd_date=="2019"|class$coprd_date=="2020"|class$coprd_date=="2021"|class$coprd_date=="2022"|class$coprd_date=="2023",]
# load the sales data.
sales <- read.table("./sales.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 466)
# getting the sales data in a desirable format
sales[,6] <- as.numeric(sales[,6])
sales[,3]<- format(as.Date(as.character(sales[,3]), "%Y%m%d"), "%Y")
# creating same column names to help merge data
colnames(class)[1] <- "prowess_code"
colnames(sales)[1] <- "prowess_code"
colnames(class)[3] <- "year"
colnames(sales)[3] <- "year"
colnames(sales)[2] <- "company_name"
# merging data 
sales_by_class <- merge(x = sales, y = class, by = c("prowess_code","year","company_name"), all.x = TRUE)
# getting rid of the less likely to be used columns
sales_by_class <- sales_by_class[,c(1,2,3,6,12)]
# loading sales data for all prowess companies in the relevant years
sales_all <- read.table("./sales_all.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 218552)
# getting the sales_all data in desirable format
sales_all[,3] <- format(as.Date(as.character(sales_all[,3]), "%Y%m%d"), "%Y")
sales_all[,6] <- as.numeric(sales_all[,6])
# getting classification data for all companies
class_all <- read.table("class_all.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 332327)
# formatting the date in class_all
class_all[,3] <- format(as.Date(as.character(class_all[,3]), "%Y%m%d"), "%Y")
# taking only annual report based classification
class_all <- class_all[class_all$mr_info_full_name=="Annual Report",]
# inserting column of nic two digit code
nic_code_two_digit <- as.numeric(substr(as.character(class_all[,5]), 1, 2))
class_all <- cbind(class_all,nic_code_two_digit)
# preparing for merging the data class_all and sales_all
colnames(class_all)[1] <- "prowess_code"
colnames(sales_all)[1] <- "prowess_code"
colnames(class_all)[3] <- "year"
colnames(sales_all)[3] <- "year"
colnames(sales_all)[2] <- "company_name"
# merging class_all and sales_all
sales_by_class_all <- merge(x = sales_all, y = class_all, by = c("prowess_code","year","company_name"),all.x = TRUE)
# keeping only the relevant columns in the data
sales_by_class_all <- sales_by_class_all[,c(1,2,3,6,10)]
# appending a column of "<>" for market_share to sales_by_class
market_share <- rep("<>",nrow(sales_by_class))
sales_by_class <- cbind(sales_by_class,market_share)
# calculating market share for each cell and writing it to the data
for (i in 1:nrow(sales_by_class)){
  if ((is.na(sales_by_class[i,4]) == TRUE)|is.na(sales_by_class[i,5]) == TRUE){
    sales_by_class[i,6] <- NA
  }
  else
    sales_by_class[i,6] <- {sales_by_class[i,4]/sum(subset(sales_by_class_all, (year == sales_by_class[i,2]) & (nic_code_two_digit == sales_by_class[i,5]), sa_sale_of_goods),na.rm = TRUE)}*100
}
# write.csv(sales_by_class,"./market_share.csv")
# year 2023 has smaller base and missing observation so dropping it for comparable visualization
sales_by_class <- sales_by_class[sales_by_class$year!="2023",]
# also dropping 2016 as it is not to be analysed
sales_by_class <- sales_by_class[sales_by_class$year!="2016",]
# reading market share as a number
sales_by_class[,6] <- as.numeric(sales_by_class[,6])
# reading prowess_code as text
sales_by_class[,1] <- as.character(sales_by_class[,1])
# classifying the companies based on average turnover for the period under consideration.
# the average turnover considered is the pre-COVID period that is 2017-19.
# creating a data set with average turnover
library(tidyverse)
sales_avg <- sales[sales$year == "2017"|sales$year == "2018"|sales$year == "2019",]
sales_avg <- sales_avg %>% group_by(prowess_code) %>% summarise(avg_turnover = mean(sa_sale_of_goods,na.rm = TRUE))
# adding firm_size based on MSME Act 
for(i in 1:73){
  if (sales_avg$avg_turnover[i] <= 50){
    sales_avg$firm_size[i] <- "Micro"
  }
  else{
    if(sales_avg$avg_turnover[i] <= 500){
      sales_avg$firm_size[i] <- "Small"
    }
    else{
      if(sales_avg$avg_turnover[i] <= 2500){
        sales_avg$firm_size[i] <- "Medium"
      }
        else{
          sales_avg$firm_size[i] <- "Large"
      }
    }
  }
}
# merging data set of firm_size so that the heat maps can be drawn by firm size category
sales_by_class <- merge(x = sales_by_class, y = sales_avg, by = "prowess_code", all.x = TRUE)
# plotting heat map for all firms
# for this the data needs to be transformed
market_share <- reshape(sales_by_class, idvar = c("company_name","prowess_code"), timevar = "year", drop = c("sa_sale_of_goods","nic_code_two_digit","avg_turnover","firm_size"),direction = "wide")
colnames(market_share) <- c("prowess_code","company_name","2017","2018","2019","2020","2021","2022")
rownames(market_share) <- market_share[,1] 
x <- as.matrix(market_share[,-c(1,2)])
heatmap(x, Colv = NA, Rowv = NA, scale="row",xlab = "Year", ylab = "Prowess Code")
# plotting a heat map for firms classified as large
market_share_large <- reshape(subset(sales_by_class, firm_size == "Large"), idvar = c("company_name","prowess_code"), timevar = "year", drop = c("sa_sale_of_goods","nic_code_two_digit","avg_turnover","firm_size"),direction = "wide")
colnames(market_share_large) <- c("prowess_code","company_name","2017","2018","2019","2020","2021","2022")
rownames(market_share_large) <- market_share_large[,1] 
y <- as.matrix(market_share_large[,-c(1,2)])
heatmap(y, Colv = NA, Rowv = NA, scale="row",xlab = "Year", ylab = "Prowess Code")
# plotting heat map for firms classified as medium
market_share_medium <- reshape(subset(sales_by_class, firm_size == "Medium"), idvar = c("company_name","prowess_code"), timevar = "year", drop = c("sa_sale_of_goods","nic_code_two_digit","avg_turnover","firm_size"),direction = "wide")
colnames(market_share_medium) <- c("prowess_code","company_name","2017","2018","2019","2020","2021","2022")
rownames(market_share_medium) <- market_share_medium[,1] 
z <- as.matrix(market_share_medium[,-c(1,2)])
heatmap(z, Colv = NA, Rowv = NA, scale="row",xlab = "Year", ylab = "Prowess Code")
# plotting heat map for firms classified as small
market_share_small <- reshape(subset(sales_by_class, firm_size == "Small"), idvar = c("company_name","prowess_code"), timevar = "year", drop = c("sa_sale_of_goods","nic_code_two_digit","avg_turnover","firm_size"),direction = "wide")
colnames(market_share_small) <- c("prowess_code","company_name","2017","2018","2019","2020","2021","2022")
rownames(market_share_small) <- market_share_small[,1] 
w <- as.matrix(market_share_small[,-c(1,2)])
heatmap(w, Colv = NA, Rowv = NA, scale="row",xlab = "Year", ylab = "Prowess Code")
# to plot heat map by the category of the beneficiary the data is transformed as follows
scheme <- read.csv("./PLISFPI.csv")
scheme <- scheme[is.na(scheme$Prowess.Code) == FALSE,]
colnames(scheme)[11] <- "prowess_code"
colnames(identity)[1] <- "prowess_code"
identity <- merge(x = identity, y = scheme, by = "prowess_code", all.x = TRUE)
# removing some less useful columns
identity <- identity[,-c(7,8,22,23,24)]
identity <- identity[,-19]
# merging category details with sales data
sales_by_class <- merge(x = identity, y = sales_by_class, by = c("prowess_code","company_name"), all.x = TRUE)
# heat map for category 1 
market_share_cat1 <- reshape(subset(sales_by_class,Category == "Category 1"),idvar = c("company_name","prowess_code"), timevar = "year", drop = c("co_industry_gp_code","co_industry_name","nic_prod_code","nic_name","Category","RTE..RTC","F..V","Marine","Mozzerrella.Cheese","Innovative","Organic","B.M","CIN","NIC.Code","Year.of.Incorporation","CMIE.Footprint","sa_sale_of_goods","nic_code_two_digit","avg_turnover","firm_size"),direction = "wide")
colnames(market_share_cat1) <- c("prowess_code","company_name","2021","2019","2020","2017","2018","2022")
rownames(market_share_cat1) <- market_share_cat1[,1]
market_share_cat1 <- market_share_cat1[,c("prowess_code","company_name","2017","2018","2019","2020","2021","2022")]
v <- as.matrix(market_share_cat1[,-c(1,2)])
heatmap(v, Colv = NA, Rowv = NA, scale="row",xlab = "Year", ylab = "Prowess Code")
# heat map for category 3
market_share_cat3 <- reshape(subset(sales_by_class,Category == "Category 3"),idvar = c("company_name","prowess_code"), timevar = "year", drop = c("co_industry_gp_code","co_industry_name","nic_prod_code","nic_name","Category","RTE..RTC","F..V","Marine","Mozzerrella.Cheese","Innovative","Organic","B.M","CIN","NIC.Code","Year.of.Incorporation","CMIE.Footprint","sa_sale_of_goods","nic_code_two_digit","avg_turnover","firm_size"),direction = "wide")
colnames(market_share_cat3) <- c("prowess_code","company_name","2021","2019","2020","2017","2018","2022")
rownames(market_share_cat3) <- market_share_cat3[,1]
market_share_cat3 <- market_share_cat1[,c("prowess_code","company_name","2017","2018","2019","2020","2021","2022")]
u <- as.matrix(market_share_cat3[,-c(1,2)])
heatmap(u, Colv = NA, Rowv = NA, scale = "row",xlab = "Year", ylab = "Prowess Code")
# testing hypothesis of the average market share being the same in 2021 and 2022
t.test(x = market_share[,7], y = market_share[,8])
# performing the same test for only large beneficiaries
t.test(x = market_share_large[,7], y = market_share_large[,8])
# performing the same test for only category 1 beneficiaries
t.test(x = market_share_cat1[,7], y = market_share_cat1[,8])
# performing the test for only category 3 beneficiaries
t.test(x = market_share_cat3[,7], y = market_share_cat3[,8])
# drawing a mobility matrix for beneficiaries that have nic 10 classification
# will do this only for those that were classified as 10 for both the years 2017 and 2022
# creating a subset of market share with year 2017 and nic 10
nic10_2017 <- subset(sales_by_class, year == "2017" & nic_code_two_digit == 10)
nic10_2017 <- nic10_2017[,-c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,23,24)]
# creating a subset of market share with year 2022 and nic 10 
nic10_2022 <- subset(sales_by_class, year == "2022" & nic_code_two_digit == 10)
nic10_2022 <- nic10_2022[,-c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,23,24)]
# creating a subset of market share with nic 10 and years 2017 and 2022
nic10 <- merge(x = nic10_2017, y = nic10_2022, by = c("prowess_code","company_name"))
nic10 <- nic10[,-c(3,5)]
colnames(nic10)[3] <- "market_share.2017"
colnames(nic10)[4] <- "market_share.2022"
# classifying firms in 2017
for(i in 1:36){
  if(nic10$market_share.2017[i]>=quantile(nic10$market_share.2017,probs = 0.7)){
    nic10$position.2017[i] <- "top30"
  }
  else
    if(nic10$market_share.2017[i]<=quantile(nic10$market_share.2017,probs = 0.3)){
      nic10$position.2017[i] <- "bottom30"
    }
  else
    nic10$position.2017[i] <- "middle40"
}
# classifying firms in 2022
for(i in 1:36){
  if(nic10$market_share.2022[i]>=quantile(nic10$market_share.2022,probs = 0.7)){
    nic10$position.2022[i] <- "top30"
  }
  else
    if(nic10$market_share.2022[i]<=quantile(nic10$market_share.2022,probs = 0.3)){
      nic10$position.2022[i] <- "bottom30"
    }
  else
    nic10$position.2022[i] <- "middle40"
}
# creating mobility matrix
# empty matrix first
row_names <- c("top30","middle40","bottom30")
col_names <- c("top30","middle40","bottom30")
a <- matrix(data = rep(0,9), nrow = 3, ncol = 3, byrow = TRUE, dimnames = list(row_names,col_names))
# filling it with the number of firms
for (i in 1:36){
  if(nic10$position.2022[i] == "top30" && nic10$position.2017[i] == "top30"){
    a[1,1] <- a[1,1]+1
  }
  if(nic10$position.2022[i] == "middle40" && nic10$position.2017[i] == "top30"){
    a[1,2] <- a[1,2]+1
  }
  if(nic10$position.2022[i] == "bottom30" && nic10$position.2017[i] == "top30"){
    a[1,3] <- a[1,3]+1
  }
  if(nic10$position.2022[i] == "top30" && nic10$position.2017[i] == "middle40"){
    a[2,1] <- a[2,1]+1
  }
  if(nic10$position.2022[i] == "middle40" && nic10$position.2017[i] == "middle40"){
    a[2,2] <- a[2,2]+1
  }
  if(nic10$position.2022[i] == "middle40" && nic10$position.2017[i] == "bottom30"){
    a[2,3] <- a[2,3]+1
  }
  if(nic10$position.2022[i] == "bottom30" && nic10$position.2017[i] == "top30"){
    a[3,1] <- a[3,1]+1
  }
  if(nic10$position.2022[i] == "bottom30" && nic10$position.2017[i] == "middle40"){
    a[3,2] <- a[3,2]+1
  }
  if(nic10$position.2022[i] == "bottom30" && nic10$position.2017[i] == "bottom30"){
    a[3,3] <- a[3,3]+1
  }
}
heatmap(a,Rowv = NA, Colv = "Rowv", xlab = "2022", ylab = "2017",cexRow = 1,cexCol = 1, symm = TRUE)
#repeating the exercise for NIC11 beneficiaries
# drawing a mobility matrix for beneficiaries that have nic 11 classification
# will do this only for those that were classified as 11 for both the years 2017 and 2022
# creating a subset of market share with year 2017 and nic 11
nic11_2017 <- subset(sales_by_class, year == "2017" & nic_code_two_digit == 11)
nic11_2017 <- nic11_2017[,-c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,23,24)]
# creating a subset of market share with year 2022 and nic 11 
nic11_2022 <- subset(sales_by_class, year == "2022" & nic_code_two_digit == 11)
nic11_2022 <- nic11_2022[,-c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,23,24)]
# creating a subset of market share with nic 11 and years 2017 and 2022
nic11 <- merge(x = nic11_2017, y = nic11_2022, by = c("prowess_code","company_name"))
nic11 <- nic11[,-c(3,5)]
colnames(nic11)[3] <- "market_share.2017"
colnames(nic11)[4] <- "market_share.2022"
# classifying firms in 2017
for(i in 1:12){
  if(nic11$market_share.2017[i]>=quantile(nic11$market_share.2017,probs = 0.7)){
    nic11$position.2017[i] <- "top30"
  }
  else
    if(nic11$market_share.2017[i]<=quantile(nic11$market_share.2017,probs = 0.3)){
      nic11$position.2017[i] <- "bottom30"
    }
  else
    nic11$position.2017[i] <- "middle40"
}
# classifying firms in 2022
for(i in 1:12){
  if(nic11$market_share.2022[i]>=quantile(nic11$market_share.2022,probs = 0.7)){
    nic11$position.2022[i] <- "top30"
  }
  else
    if(nic11$market_share.2022[i]<=quantile(nic11$market_share.2022,probs = 0.3)){
      nic11$position.2022[i] <- "bottom30"
    }
  else
    nic11$position.2022[i] <- "middle40"
}
# creating mobility matrix
# empty matrix first
row_names <- c("top30","middle40","bottom30")
col_names <- c("top30","middle40","bottom30")
b <- matrix(data = rep(0,9), nrow = 3, ncol = 3, byrow = TRUE, dimnames = list(row_names,col_names))
# filling it with the number of firms
for (i in 1:12){
  if(nic11$position.2022[i] == "top30" && nic11$position.2017[i] == "top30"){
    b[1,1] <- b[1,1]+1
  }
  if(nic11$position.2022[i] == "middle40" && nic11$position.2017[i] == "top30"){
    b[1,2] <- b[1,2]+1
  }
  if(nic11$position.2022[i] == "bottom30" && nic11$position.2017[i] == "top30"){
    b[1,3] <- b[1,3]+1
  }
  if(nic11$position.2022[i] == "top30" && nic11$position.2017[i] == "middle40"){
    b[2,1] <- b[2,1]+1
  }
  if(nic11$position.2022[i] == "middle40" && nic11$position.2017[i] == "middle40"){
    b[2,2] <- b[2,2]+1
  }
  if(nic11$position.2022[i] == "middle40" && nic11$position.2017[i] == "bottom30"){
    b[2,3] <- b[2,3]+1
  }
  if(nic11$position.2022[i] == "bottom30" && nic11$position.2017[i] == "top30"){
    b[3,1] <- b[3,1]+1
  }
  if(nic11$position.2022[i] == "bottom30" && nic11$position.2017[i] == "middle40"){
    b[3,2] <- b[3,2]+1
  }
  if(nic11$position.2022[i] == "bottom30" && nic11$position.2017[i] == "bottom30"){
    b[3,3] <- b[3,3]+1
  }
}
heatmap(b,Rowv = NA, Colv = "Rowv", xlab = "2022", ylab = "2017",cexRow = 1,cexCol = 1, symm = TRUE)