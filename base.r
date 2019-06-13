############ 資料處理-得到trans.df(trans.df為刪除會員卡號、性別、年齡，並且omit na後的dataframe) ###########
# 讀取10萬筆資料
transactions <- read.csv("C:\\Users\\tiffany\\Documents\\network practice\\customer_data.csv", fileEncoding = "big5", nrows=100000)
trans.df <- transactions
options(scipen=999)
summary(trans.df)

# 刪除會員卡號、性別、年齡
trans.df <- trans.df[, -c(1, 15, 16)]
# na.omit
trans.df <- na.omit(trans.df) # 99981 obs. of 13 variables
length(unique(trans.df$交易id)) # 32134
#################################




######### 未刪減過的資料 #########

# 產品的association rules
library(arules)
binarymat <- ifelse(table(trans.df$交易id, trans.df$單品名稱) > 0,1,0)
trans.rules <- apriori(binarymat, parameter = list(supp=0.002,conf=0.3,target='rules'))
inspect(head(sort(trans.rules, by='lift'),n=20))


# 品號的association rules
binarymat.pin <- ifelse(table(trans.df$交易id, trans.df$品號.品名稱) > 0,1,0)
trans.pin.rules <- apriori(binarymat.pin, parameter=list(supp=0.01,conf=0.3,target='rules'))
inspect(head(sort(trans.pin.rules, by='lift'),n=20))
###################################




######### 刪掉銷售單價=0的資料(得到trans.pruned) #########
trans.pruned <- trans.df[-which(trans.df$銷售單價==0),] # trans.pruned:84471 obs, 13 variables
length(unique(trans.pruned$交易id)) # 29200

# 產品的association rules
library(arules)
binarymat.pruned <- ifelse(table(trans.pruned$交易id, trans.pruned$單品名稱) > 0,1,0)
trans.rules.pruned <- apriori(binarymat.pruned, parameter = list(supp=0.001,conf=0.3,target='rules'))
inspect(head(sort(trans.rules.pruned, by='lift'),n=20))


# 品號.品名稱的association rules
binarymat.pin.pruned <- ifelse(table(trans.pruned$交易id, trans.pruned$品號.品名稱) > 0,1,0)
trans.pin.rules.pruned <- apriori(binarymat.pin.pruned, parameter=list(supp=0.01,conf=0.3,target='rules'))
inspect(head(sort(trans.pin.rules, by='lift'),n=20))
#########################################################

