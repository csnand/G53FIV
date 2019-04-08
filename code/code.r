library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)
library(ggcorrplot)
# library(tidyr)
# library(plyr)



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
medianEarning <- read.table(paste(R_DATA_GENERAL, "median-gross-annual-workplace-based-earnings-by-region.csv", sep=""), quote="\"", header=T, comment.char="", sep=SEP2, na.strings = c("", "NA"))
head(medianEarning)


# full median-house-price-by-region
# replace all empty cells with NA
medianHousePrice <- read.table(paste(R_DATA_GENERAL, "median-house-price-by-region.csv", sep=""), quote="\"", header=T, comment.char="", sep=SEP2, na.strings = c("", "NA"))
head(medianHousePrice)


# full ratio-of-median-house-price-to-median-gross-annual-workplace-based-earnings-by-region
# replace all empty cells with NA
affordRatio <- read.table(paste(R_DATA_GENERAL, "ratio-of-median-house-price-to-median-gross-annual-workplace-based-earnings-by-region.csv", sep=""), quote="\"", header=T, comment.char="", sep=SEP2, na.strings = c("", "NA"))
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

# convert character to numeric
medianEarningEss$Value <- as.numeric(gsub("," ,"", medianEarningEss$Value))
medianHousePriceEss$Value <- as.numeric(gsub("," ,"", medianHousePriceEss$Value))

# convert string to numeric representation
affordRatioEss$Year <- as.numeric(gsub("X", "", affordRatioEss$Year))
medianEarningEss$Year <- as.numeric(gsub("X", "", medianEarningEss$Year))
medianHousePriceEss$Year <- as.numeric(gsub("Year.ending.Sep.", "", medianHousePriceEss$Year))


# filter dta related to economy crisis 
housePriceCrisis <- medianHousePriceEss %>% filter(Year == 2008 | Year == 2009)
affordRatioCrisis <- affordRatioEss %>% filter(Year == 2008 | Year == 2009)
earningCrisis <- medianEarningEss %>% filter(Year == 2008 | Year == 2009)



##############################################################################################################
# BASIC ANALYSIS AND VISUALIZATION
##############################################################################################################

message("RQ1: How the mean house price across the regions changedfrom  2000  onward  and  Which  region  has  cheapest house.")
ggplot(medianHousePriceEss, aes(x = factor(Year), y = Value)) + geom_point() + xlab("Year")
ggsave("Q1Geom_point_no_colour.png", width = 16, height = 9, units = "in", dpi = 100)

ggplot(medianHousePriceEss, aes(x = factor(Year), y = Value)) + geom_point(aes(color = Name)) + xlab("Year")
ggsave("Q1Geom_point.png", width = 16, height = 9, units = "in", dpi = 100)


ggplot(medianHousePriceEss, aes(x = factor(Year), y = Value, group=Name)) + geom_line() + xlab("Year")
ggsave("Q1Geom_line_no_colour.png", width = 16, height = 9, units = "in", dpi = 100)

ggplot(medianHousePriceEss, aes(x = factor(Year), y = Value, group=Name, colour=Name)) + geom_line() + xlab("Year")
ggsave("Q1Geom_line.png", width = 16, height = 9, units = "in", dpi = 100)

ggplot(data = medianHousePriceEss, aes(x = factor(Year), y = Value, fill=Name)) + geom_bar(stat="identity",position=position_dodge(0.9)) +theme(axis.text.x = element_text(size=4, angle = 90, hjust = 1)) + facet_grid(rows = medianHousePriceEss$Name) + xlab("Year")
ggsave("Q1Geom_gridbar.png", width = 16, height = 6, units = "in", dpi = 320)


message("RQ2: How the ratio of house price to workplace-based earn-ings changed from 2000 onward and which region hasthe most affordable houses.")

ggplot(affordRatioEss, aes(x = factor(Year), y = Value)) + geom_point() + xlab("Year")
ggsave("Q2Geom_point_no_colour.png", width = 16, height = 9, units = "in", dpi = 100)

ggplot(affordRatioEss, aes(x = factor(Year), y = Value)) + geom_point(aes(color = Name)) + xlab("Year")
ggsave("Q2Geom_point.png", width = 16, height = 9, units = "in", dpi = 100)

ggplot(affordRatioEss, aes(x = factor(Year), y = Value, group=Name)) + geom_line() + xlab("Year")
ggsave("Q2Geom_line_no_colour.png", width = 16, height = 9, units = "in", dpi = 100)

ggplot(affordRatioEss, aes(x = factor(Year), y = Value, group=Name, colour=Name)) + geom_line() + xlab("Year")
ggsave("Q2Geom_line.png", width = 16, height = 9, units = "in", dpi = 100)

ggplot(data = affordRatioEss, aes(x = factor(Year), y = Value, fill=Name)) + geom_bar(stat="identity",position=position_dodge(0.9)) + theme(axis.text.x = element_text(size=4, angle = 90, hjust = 1)) + facet_grid(rows = affordRatioEss$Name) + xlab("Year")
ggsave("Q2Geom_gridbar.png", width = 16, height = 6, units = "in", dpi = 320)


message("RQ3: How the affordability and house price affected by the economy crisis in 2008.")
g <- ggplot(data = earningCrisis, aes(x = factor(Year), y = Value, fill=Name)) + geom_bar(stat="identity",position=position_dodge(0.9)) + theme(axis.text.x = element_text(size=4, angle = 90, hjust = 1)) + facet_grid(rows = earningCrisis$Name) + xlab("Year")
g1 <- ggplot(data = housePriceCrisis, aes(x = factor(Year), y = Value, fill=Name)) + geom_bar(stat="identity",position=position_dodge(0.9)) + theme(axis.text.x = element_text(size=4, angle = 90, hjust = 1)) + facet_grid(rows = housePriceCrisis$Name) + xlab("Year")
g2 <- ggplot(data = affordRatioCrisis, aes(x = factor(Year), y = Value, fill=Name)) + geom_bar(stat="identity",position=position_dodge(0.9)) + theme(axis.text.x = element_text(size=4, angle = 90, hjust = 1)) + facet_grid(rows = affordRatioCrisis$Name) + xlab("Year")
grid.arrange(g, g1, g2, nrow=3)
ggsave("Q3Geom_gridbar.png", plot = grid.arrange(g, g1, g2, nrow=3), width = 16, height = 10, units = "in", dpi = 320)



# calculate corelation coefficient
housePriceForCor <- data.frame(matrix(ncol = 12, nrow = 19))
name <- medianHousePrice$Name %>% na.omit()
colnames(housePriceForCor) <- c(as.character(name))


for (col in 1:ncol(housePriceForCor)) {
    housePriceForCor[col] <- (medianHousePriceEss %>% filter(Name == colnames(housePriceForCor)[col]))$Value
}

houseCor <- cor(housePriceForCor)
ggcorrplot(houseCor, hc.order = TRUE, outline.col = "white", ggtheme = ggplot2::theme_gray, colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE)
ggsave("corHeatMap.png", width = 16, height = 9, units = "in", dpi = 100)
