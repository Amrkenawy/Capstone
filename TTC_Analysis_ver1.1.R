# Load readxl & stringr libraries

library(readxl)
library(stringr)
library(ggplot2)


# Create one dataframe for the entire worksheet

df <- data.frame()

for (b in c(14,15,16,17,18)){
    

    x <- sprintf("D:/DataAnalytics/CKME_136/TTC_Bus_Delay/Capstone/Bus_20%s.xlsx",b)

    
    y <- excel_sheets(x)
    
    
    # Create one dataframe for the entire worksheet
    
    for (a in y){
        
        df1 <- read_excel(x,sheet = a)
        
        df <- rbind(df,df1)
        
    }
    
}


write.csv(df,"D:/DataAnalytics/CKME_136/TTC_Bus_Delay/Capstone/combined.csv")


library(readxl)
library(stringr)
library(ggplot2)


df <- read.csv("D:/DataAnalytics/CKME_136/TTC_Bus_Delay/Capstone/combined.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")

head(df)

df$Incident <- as.factor(df$Incident)

df$day <- as.factor(df$day)

class(df$Incident)

class(df)

summary(df)

names(df) <- c("date","route","time","day","location","incident","delay","gap","direction","vehicle")


df2 <- df[which(!is.na(df$delay)),]


ggplot(df2,aes(y=delay,x=incident))+geom_boxplot()

df3 <- df2[which(df2$delay < 60),]

ggplot(df3,aes(y=delay,x=incident))+geom_boxplot()


aggregate(delay ~ incident, data = df3, mean)

aggregate(delay ~ incident, data = df3, var)

ggplot(df3, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~incident)

delay.lm <- lm(delay ~ incident, data = df3)
summary(delay.lm)


delay.lm2 <- lm(delay ~ incident-1, data = df3)
summary(delay.lm2)

require(coefplot)
coefplot(delay.lm)


coefplot(delay.lm2)

delay.lm3 <- lm(delay ~ incident * day, data = df3)
summary(delay.lm3)

coefplot(delay.lm3)
multiplot(delay.lm2,delay.lm3)

month2 <- str_sub(df3$date,start = 6, end = 7)

mon.name <- function(x){
    
    switch(x,
           "01"="Jan",
           "02"="Feb",
           "03"="Mar",
           "04"="Apr",
           "05"="May",
           "06"="Jun",
           "07"="Jul",
           "08"="Aug",
           "09"="Sep",
           "10"="Oct",
           "11"="Nov",
           "12"="Dec"
           )
    
}

month3 <- sapply(month2, mon.name)

df4 <- data.frame(df3,month2)

ggplot(df4, aes(x= delay,fill=month2))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~month2)


year2 <- str_sub(df4$date,start = 1, end = 4)

df5 <- data.frame(df4,year2)

ggplot(df5, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~year2)

season <- function(x){
    
    switch(x,
           "01"="Winter",
           "02"="Winter",
           "03"="Winter",
           "04"="Spring",
           "05"="Spring",
           "06"="Spring",
           "07"="Summer",
           "08"="Summer",
           "09"="Summer",
           "10"="Fall",
           "11"="Fall",
           "12"="Winter"
    )
    
}
season2 <- sapply(month2, season)
df6 <- data.frame(df5,season2)

ggplot(df6, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~season2)

ggplot(df6, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~season2+year2)


df5$location <- as.factor(df5$location)

levels(df5$location)

location2 <- aggregate(incident ~ location, df5 ,length)
location3 <- location2[order(-location2$incident),]
summary(location3)

quantile(location3$incident, probs = .995)

length(location3$incident[location3$incident > 1000])
