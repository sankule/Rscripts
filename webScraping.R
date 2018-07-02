library(dplyr)
library(curl)
library(rvest)
library(ggplot2)
library(stringr)

# years to scrape
yearsToScrape <- 2017:2018
yearsToScrape

monthsName <- tolower(month.name)
monthsName

# Loop with month Month name and number
weatherData <- data.frame()
for(i in yearsToScrape){
  for(j in 1:12){
    # URL 
    url <- paste0("https://www.accuweather.com/en/in/bengaluru/2801148/", monthsName[j],"-weather/2801148?monyr=", as.character(j),"/1/",as.character(i),"&view=table")
    # XPath remains the same
    # Mining table and appending each DF to weatherData
    minedDataset <- url %>% read_html() %>% html_nodes(xpath='//*[@id="panel-main"]/div[2]/div/div/table') %>% html_table()
    
    # extracting DF from LIST
    minedDataset <- as.data.frame(minedDataset[[1]])
    
    # Imputing Date/Hi/Low/Precipitation
    minedDataset$realDate <- lubridate::mdy(paste0(stringr::str_split(minedDataset$Date, pattern = " ", simplify = TRUE)[,-1], "/",as.character(i)))
    minedDataset$HighTemp <- as.numeric(stringr::str_sub(stringr::str_split(minedDataset$`Hi/Lo`, pattern = "/", simplify = TRUE)[,1], start = 1, end = 2))
    minedDataset$LowTemp <- as.numeric(stringr::str_sub(stringr::str_split(minedDataset$`Hi/Lo`, pattern = "/", simplify = TRUE)[,2], start = 1, end = 2))
    minedDataset$precipitation_in_mm <- as.numeric(stringr::str_split(minedDataset$Precip, pattern = " ", simplify = TRUE)[,1])
    
    # rowbinding month table to parent DF
    weatherData <- rbind(weatherData, minedDataset)
    
    # Break at the last month
    if(paste0(as.character(i),"-",as.character(j)) == paste0(as.character(year(Sys.Date())), "-", as.character(month(Sys.Date())-1))){
      break
    }
  }
}

weatherData

# precipitation factor summary
summary.factor(weatherData$Precip)
# NA check 
colSums(is.na(weatherData))

# removing forecast / average temp column / snow column / precip/ Date
weatherData$Forecast <- NULL
weatherData$`Avg. HI / LO` <- NULL
weatherData$Date <- NULL
weatherData$Snow <- NULL
weatherData$Precip <- NULL
weatherData$`Hi/Lo` <- NULL

# checking structure
str(weatherData)

# Month Wise Aggregation
weatherSummary <- as.data.frame(weatherData %>% mutate(MONTH=floor_date(realDate, "month"), YEAR = lubridate::year(realDate)) %>% 
                                  group_by(MONTH, YEAR) %>% dplyr::summarise(totPrecip = sum(precipitation_in_mm),
                                                                             avgPrecip = mean(precipitation_in_mm),
                                                                             avgHighTemp = mean(HighTemp),
                                                                             avgLowTemp = mean(LowTemp)) %>% dplyr::arrange(YEAR))
weatherSummary


# Plots
# Total Precipitation
plot <- ggplot(weatherSummary, aes(MONTH, totPrecip)) +  geom_point(color = "red", size = 2) + geom_line(linetype = "dashed") 
plot
min <- as.Date("2017-1-1")
max <- as.Date("2018-7-1")
plot <- plot + scale_x_date(limits = c(min, max), date_labels = "%b-%y", date_breaks = "1 month") + scale_y_continuous(breaks = seq(0, 700, 20))
plot + xlab("Month") + ylab("Total Precipitation (mm)") + ggtitle("Monthly Total Precipitation(mm) in Bangalore")

# Average Low Temperature
plot <- ggplot(weatherSummary, aes(MONTH, avgLowTemp)) +  geom_point(color = "red", size = 2) + geom_line(linetype = "dashed") 
plot
min <- as.Date("2017-1-1")
max <- as.Date("2018-7-1")
plot <- plot + scale_x_date(limits = c(min, max), date_labels = "%b-%y", date_breaks = "1 month") + scale_y_continuous(breaks = seq(0,50,1))
plot + xlab("Month") + ylab("Temperature in C") + ggtitle("Average Monthly Low Temperature in Bangalore")

# Average High Temperature
plot <- ggplot(weatherSummary, aes(MONTH, avgHighTemp)) +  geom_point(color = "red", size = 2) + geom_line(linetype = "dashed") 
plot
min <- as.Date("2017-1-1")
max <- as.Date("2018-7-1")
plot <- plot + scale_x_date(limits = c(min, max), date_labels = "%b-%y", date_breaks = "1 month") + scale_y_continuous(breaks = seq(0,50,1))
plot + xlab("Month") + ylab("Temperature in C") + ggtitle("Average Monthly High Temperature in Bangalore")
