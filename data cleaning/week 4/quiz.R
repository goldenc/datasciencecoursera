if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/communitySurvey.csv")
cmtySrvy <- read.csv("./data/communitySurvey.csv")

names(cmtySrvy)

strsplit(names(cmtySrvy), "wgtp")[123]

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "./data/gdp.csv")
gdp <- read.csv("./data/gdp.csv")


gdpFiltered <- gdp[!is.na(as.numeric(levels(gdp$Gross.domestic.product.2012))[gdp$Gross.domestic.product.2012]), ]

averages <- as.numeric(gsub(",", "", gdpFiltered$X.3))
mean(averages[!is.na(averages)])

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", destfile = "./data/country.csv")
country <- read.csv("./data/country.csv")

filtered <- country[nchar(as.character(country$Income.Group)) > 0, ]
merged <- merge(x = gdpFiltered, y = filtered, by.x = "X", by.y = "CountryCode")

length(merged[grep("^Fiscal year end: June", merged$Special.Notes), "Special.Notes"])

library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

year