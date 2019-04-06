library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gridExtra)
library(tidyr)


SEP <- "\t"
SEP2 <- ","

R_ANA_GENERAL <<- "/home/jackey/Downloads/G53FIV/code/data/ByRegion/"

R_DATA_GENERAL <<- "/home/jackey/Downloads/G53FIV/code/data/ByRegion/"

##############################################################################################################
# LOAD DATA
##############################################################################################################

message("loading data...")

# full median-gross-annual-workplace-based-earnings-by-region
# replace all empty cells with NA
medianEarning <- read.table(paste(R_DATA_GENERAL, "median-gross-annual-workplace-based-earnings-by-region.csv", sep=""), quote="\"", header=T, comment.char="", sep=SEP2, na.strings = c("", "NA"), fill=TRUE)
head(medianEarning)


# full median-house-price-by-region
# replace all empty cells with NA
medianHousePrice <- read.table(paste(R_DATA_GENERAL, "median-house-price-by-region.csv", sep=""), quote="\"", header=T, comment.char="", sep=SEP2, na.strings = c("", "NA"), fill=TRUE)
head(medianHousePrice)


# full ratio-of-median-house-price-to-median-gross-annual-workplace-based-earnings-by-region
# replace all empty cells with NA
affordRatio <- read.table(paste(R_DATA_GENERAL, "ratio-of-median-house-price-to-median-gross-annual-workplace-based-earnings-by-region.csv", sep=""), quote="\"", header=T, comment.char="", sep=SEP2, na.strings = c("", "NA"), fill=TRUE)
head(affordRatio)



##############################################################################################################
# DATA MANIPULATION
##############################################################################################################

message("filtering data...")

# essential earnings data for analysis
# only use use data from 2000 to 2018
medianEarningEss <- subset(medianEarning, select=-c(Code, X1997,X1998,X1999,X))

# essential ration data for analysis
# only use use data from 2000 to 2018
affordRatioEss <- subset(affordRatio, select=-c(Code, X1997,X1998,X1999,X))

# essential house price data for analysis
# only use use data from 2000 to 2018
before2000 <- grepl(pattern = "Year.ending.Sep.199|Code", colnames(medianHousePrice))
medianHousePriceEss <- medianHousePrice[!before2000]

# remove all empty cells
medianEarningEss = medianEarningEss %>% na.omit()
medianHousePriceEss = medianHousePriceEss %>% na.omit()
affordRatioEss = affordRatioEss %>% na.omit()


# rearrange data -- put year date in one column

message("rearrenging data...")
medianEarningEss <- as.data.frame(medianEarningEss) %>% tidyr::gather(key = Year, value = Value, -Name)
affordRatioEss <- as.data.frame(affordRatioEss) %>% tidyr::gather(key = Year, value = Value, -Name)
medianHousePriceEss <- as.data.frame(medianHousePriceEss) %>% tidyr::gather(key = Year, value = Value, -Name)




##############################################################################################################
# BASIC ANALYSIS AND VISUALIZATION
##############################################################################################################

message("RQ1: How the mean house price across the regions changedfrom  2000  onward  and  Which  region  has  cheapesthouse.")


groupColumns = c("month")
dataColumns = c("price")

res = ddply(propSaleEss2, groupColumns, summarize, meanprice=mean(price, 2), sumcount=sum(count))
head(res)
p1 <- ggplot(data = res, aes(x = month, y = meanprice)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1))
p2 <- ggplot(data = res, aes(x = month, y = sumcount)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1))

grid.arrange(p1, p2, ncol=2)


message("RQ2: How the ratio of house price to workplace-based earn-ings changed from 2000 onward and which region hasthe most affordable houses.")

groupColumns = c("year","city","type","age")
dataColumns = c("price")
res = ddply(propSaleEss, groupColumns, function(x) colMeans(x[dataColumns]))
head(res)

res2 <- subset(res, type == "D" | type == "F")
ggplot(data = res2, aes(x = year, y = price, fill=type)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1))


ggplot(data = res, aes(x = year, y = price, fill=age)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1))


message("RQ3: How the house price changes across different regioncorelated to each other. (what is the corelation coef-ficiency of house price between different regions.)")

groupColumns = c("year", "type", "county")
dataColumns = c("price")
propSaleEss4 <- subset(propSaleEss, county == "Greater London" | county == "Nottinghamshire" | county == "Northumberland")
res = ddply(propSaleEss4, groupColumns, summarize, meanprice=mean(price, 2), sumcount=sum(count))
head(res)
pdf("countyStats_bytypeage.pdf", onefile = TRUE, width = 14)
ggplot(data = res, aes(x = year, y = meanprice, fill=county)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1)) + facet_grid(county ~ type) 
dev.off()







