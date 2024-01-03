#The purpose of this file is to get the market share for the beneficiaries of the firms in categories 1 and 3 of the PLIFPI. The basis is going to be total sales of goods field in the standalone annual financial statements. 

#Loading the identity details of the beneficiaries.
identity <- read.table("./identity.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 74)

#loading the history of classification of the beneficiaries.
class <- read.table("./class.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 2713)
#asking r to read the dates as such
class[,3] <- format(as.Date(as.character(class[,3]), "%Y%m%d"), "%Y")
#adding a column of nic codes at 2 digit level
nic_code_two_digit <- as.numeric(substr(as.character(class[,7]), 1, 2))
class <- cbind(class,nic_code_two_digit)
#taking only annual reports based classification
class <- class[class$mr_info_full_name=="Annual Report",]
#considering only the years 2017-2023
class <- class[class$coprd_date=="2017"|class$coprd_date=="2018"|class$coprd_date=="2019"|class$coprd_date=="2020"|class$coprd_date=="2021"|class$coprd_date=="2022"|class$coprd_date=="2023",]
#load the sales data.
sales <- read.table("./sales.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 466)
#getting the sales data in a desirable format
sales[,6] <- as.numeric(sales[,6])
sales[,3]<- format(as.Date(as.character(sales[,3]), "%Y%m%d"), "%Y")
#creating same column names to help merge data
colnames(class)[1] <- "prowess_code"
colnames(sales)[1] <- "prowess_code"
colnames(class)[3] <- "year"
colnames(sales)[3] <- "year"
colnames(sales)[2] <- "company_name"
#merging data 
sales_by_class <- merge(x = sales, y = class, by = c("prowess_code","year","company_name"), all.x = TRUE)
#getting rid of the less likely to be used columns
sales_by_class <- sales_by_class[,c(1,2,3,6,12)]
#loading sales data for all prowess companies in the relevant years
sales_all <- read.table("./sales_all.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 218552)
#getting the sales_all data in desirable format
sales_all[,3] <- format(as.Date(as.character(sales_all[,3]), "%Y%m%d"), "%Y")
sales_all[,6] <- as.numeric(sales_all[,6])
#getting classification data for all companies
class_all <- read.table("class_all.txt", header = T,sep = "|", na.strings="", comment.char = "", quote = "\"", fill = F,nrows = 332327)
#formatting the date in class_all
class_all[,3] <- format(as.Date(as.character(class_all[,3]), "%Y%m%d"), "%Y")
#taking only annual report based classification
class_all <- class_all[class_all$mr_info_full_name=="Annual Report",]
#inserting column of nic two digit code
nic_code_two_digit <- as.numeric(substr(as.character(class_all[,5]), 1, 2))
class_all <- cbind(class_all,nic_code_two_digit)
#preparing for merging the data class_all and sales_all
colnames(class_all)[1] <- "prowess_code"
colnames(sales_all)[1] <- "prowess_code"
colnames(class_all)[3] <- "year"
colnames(sales_all)[3] <- "year"
colnames(sales_all)[2] <- "company_name"
#merging class_all and sales_all
sales_by_class_all <- merge(x = sales_all, y = class_all, by = c("prowess_code","year","company_name"),all.x = TRUE)
#keeping only the relevant columns in the data
sales_by_class_all <- sales_by_class_all[,c(1,2,3,6,10)]
#appending a column of "<>" for market_share to sales_by_class
market_share <- rep("<>",nrow(sales_by_class))
sales_by_class <- cbind(sales_by_class,market_share)
#calculating market share for each cell and writing it to the data
for (i in 1:nrow(sales_by_class)){
  if ((is.na(sales_by_class[i,4]) == TRUE)|is.na(sales_by_class[i,5]) == TRUE){
    sales_by_class[i,6] <- NA
  }
  else
    sales_by_class[i,6] <- {sales_by_class[i,4]/sum(subset(sales_by_class_all, (year == sales_by_class[i,2]) & (nic_code_two_digit == sales_by_class[i,5]), sa_sale_of_goods),na.rm = TRUE)}*100
}
write.csv(sales_by_class,"./market_share.csv")
#year 2023 has smaller base and missing observation so dropping it for comparable visualization
sales_by_class <- sales_by_class[sales_by_class$year!="2023",]
#also dropping 2016 as it is not to be analysed
sales_by_class <- sales_by_class[sales_by_class$year!="2016",]
#reading market share as a number
sales_by_class[,6] <- as.numeric(sales_by_class[,6])
#reading prowess_code as text
sales_by_class[,1] <- as.character(sales_by_class[,1])
#plotting a heatmap
library(ggplot2)
plt <- ggplot(sales_by_class,aes(y = prowess_code,x = year, fill = market_share))
plt <- plt + geom_tile()
# further customizing the heatmap by
# applying colors and title
plt <- plt + theme_minimal()
# setting gradient color as red and white
plt <- plt + scale_fill_gradient(low="white", high="red")
# setting the title and subtitles using
# title and subtitle
plt <- plt + labs(title = "Heatmap")
plt <- plt + labs(subtitle = "For market share of category 1 and 3 beneficiaries")
# setting x and y labels using labs
plt <- plt + labs(x ="Year", y ="Beneficiary")
# plotting the Heatmap
plt
#currently the heatmap is not very informative as it includes very big and small companies together.
#in the identity database, we add data related to the category of the beneficiary and the classification based on turnover