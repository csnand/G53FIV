library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gridExtra)


SEP <- "\t"
SEP2 <- ","

R_ANA_GENERAL <<- "/home/jackey/Downloads/G53FIV/area-analysis/"

R_DATA_GENERAL <<- "/home/jackey/Downloads/G53FIV/area-analysis/"

##############################################################################################################
# LOAD DATA
##############################################################################################################

message("loading data...")

# full property sale data
propSale <- read.table(paste(R_DATA_GENERAL, "property-sale-data-1995-2017-withheader_001sample.csv", sep=""), quote="\"", header=T, comment.char="", sep=SEP2)
head(propSale)


# read the county data
county <- read.table(paste(R_ANA_GENERAL, "counties.tsv", sep=""), quote="\"", header=T, comment.char="", sep=SEP)
head(county)

##############################################################################################################
# DATA MANIPULATION
##############################################################################################################

message("filtering data...")

# essential property sale data for analysis
# only use standard priced paid entry + additional records
propSaleEss <- subset(propSale, ppdcategory == "A" && recordstatus == "A")


propSaleEss <- propSaleEss[,c("price","date","postcode","type","age","tenure","city")]

propSaleEss$yearmonth <- substr(propSaleEss$date, 1, 7)
propSaleEss$year <- substr(propSaleEss$yearmonth, 1, 4)
propSaleEss$area <- sub(" .*", "", propSaleEss$postcode)
propSaleEss <- subset(propSaleEss, type != "O")
propSaleEss <- subset(propSaleEss, year >= 2006 & year <= 2010)
propSaleEss <- subset(propSaleEss, tenure == "F" | tenure == "L")

propSaleEss <- propSaleEss[,c("yearmonth","price","city","area","type","age","tenure")]


# group by to get essential aggregated data
groupColumns = c("yearmonth","city","area","type","age","tenure")
dataColumns = c("price")

propSaleEssGroup = ddply(propSaleEss, groupColumns, summarize, meanprice=mean(price, 2), count=sum(!is.na(price)))

propSaleEssGroup$year <- substr(propSaleEssGroup$yearmonth, 1, 4)
propSaleEssGroup$month <- substr(propSaleEssGroup$yearmonth, 6, 7)


colnames(propSaleEssGroup)[which(names(propSaleEssGroup) == "meanprice")] <- "price"

write.csv(propSaleEssGroup, file = "property_sale_2006_2010_essential_grouped_demo.csv", row.names=FALSE, na="")

propSaleEss <- propSaleEssGroup



# merge with other data

message("merging data...")

propSaleEss2 <- merge(propSaleEss, county, by.x="area", by.y="postcode.prefix")
head(propSaleEss2)
propSaleEss <- propSaleEss2

##############################################################################################################
# BASIC ANALYSIS AND VISUALIZATION
##############################################################################################################

message("RQ1: When do property sales generally happen? Which month is the best month to sell?")

groupColumns = c("month")
dataColumns = c("price")

res = ddply(propSaleEss2, groupColumns, summarize, meanprice=mean(price, 2), sumcount=sum(count))
head(res)
p1 <- ggplot(data = res, aes(x = month, y = meanprice)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1))
p2 <- ggplot(data = res, aes(x = month, y = sumcount)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1))

grid.arrange(p1, p2, ncol=2)


message("RQ2: How do property sale price change according to different property types and age?")

groupColumns = c("year","city","type","age")
dataColumns = c("price")
res = ddply(propSaleEss, groupColumns, function(x) colMeans(x[dataColumns]))
head(res)

res2 <- subset(res, type == "D" | type == "F")
ggplot(data = res2, aes(x = year, y = price, fill=type)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1))


ggplot(data = res, aes(x = year, y = price, fill=age)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1))


message("RQ3: How do property sale price compare across different counties?")

groupColumns = c("year", "type", "county")
dataColumns = c("price")
propSaleEss4 <- subset(propSaleEss, county == "Greater London" | county == "Nottinghamshire" | county == "Northumberland")
res = ddply(propSaleEss4, groupColumns, summarize, meanprice=mean(price, 2), sumcount=sum(count))
head(res)
pdf("countyStats_bytypeage.pdf", onefile = TRUE, width = 14)
ggplot(data = res, aes(x = year, y = meanprice, fill=county)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1)) + facet_grid(county ~ type) 
dev.off()







