if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/communitySurvey.csv")
cmtySrvy <- read.csv("./data/communitySurvey.csv")

filtered <- cmtySrvy[cmtySrvy$RT == "H" & !is.na(cmtySrvy$ACR) & cmtySrvy$ACR == 3 & !is.na(cmtySrvy$AGS) & cmtySrvy$AGS == 6, ]

cmtySrvy$RT == "H" & (!is.na(cmtySrvy$ACR) & cmtySrvy$ACR == 3) & (!is.na(cmtySrvy$AGS) & cmtySrvy$AGS == 6)

cmtySrvy[which(predicate), ]

library("jpeg")


download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", destfile = "./data/jeff.jpg")
img <- readJPEG("./data/jeff.jpg")

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "./data/gdp.csv")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", destfile = "./data/country.csv")

gdp <- read.csv("./data/gdp.csv")
country <- read.csv("./data/country.csv")

head(country)


gdpFiltered <- gdp[!is.na(as.numeric(levels(gdp$Gross.domestic.product.2012))[gdp$Gross.domestic.product.2012]), ]
merged <- merge(x = gdpFiltered, y = filtered, by.x = "X", by.y = "CountryCode")

merged$gdpVal <- as.numeric(as.character(merged$Gross.domestic.product.2012))
ordered <- merged[order(merged$gdpVal, decreasing = TRUE), ]

incomeList <- merged[merged$Income.Group == "High income: OECD", ]
mean(incomeList$gdpVal)

incomeList <- merged[merged$Income.Group == "High income: nonOECD", ]
mean(incomeList$gdpVal)

table(cut2(merged$gdpVal, g=5), merged$Income.Group)