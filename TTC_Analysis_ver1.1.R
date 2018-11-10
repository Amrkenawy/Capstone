# Load readxl & stringr libraries

library(readxl)
library(stringr)
library(ggplot2)
require(coefplot)


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


# Store Combined file in one CSV file

# write.csv(df,"D:/DataAnalytics/CKME_136/TTC_Bus_Delay/Capstone/combined.csv")

# Load the combined file

delay <- read.csv("D:/DataAnalytics/CKME_136/TTC_Bus_Delay/Capstone/combined.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")

head(delay)

delay$Incident <- as.factor(delay$Incident)

delay$Day <- as.factor(delay$Day)

class(delay$Incident)

class(delay$Day)

summary(delay$Incident)

# Data Cleaning

# Renaming the columns

names(delay) <- c("SrNo","date","route","time","day","location","incident","delay","gap","direction","vehicle")


# removing "NAs"

delay2 <- delay[which(!is.na(delay$delay)),]


ggplot(delay2,aes(y=delay,x=incident))+geom_boxplot()

# removing outliers

delay3 <- delay2[which(delay2$delay < 60 & delay2$delay > 0 ),]

head(delay3)

class(delay3$date)

delay3$date <- as.Date(delay3$date, format = "%m/%d/%Y")

ggplot(delay3,aes(y=delay,x=incident))+geom_boxplot()


aggregate(delay ~ incident, data = delay3, mean)

aggregate(delay ~ incident, data = delay3, var)

ggplot(delay3, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~incident)

ggplot(delay3, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~incident)+theme(legend.position = "top")

# Analysis of Variance "ANOVA" using linear regression coefficient

delay3.lm <- lm(delay ~ incident, data = delay3)
summary(delay3.lm)


delay3.lm2 <- lm(delay ~ incident-1, data = delay3)
summary(delay3.lm2)

# Plot linear regression coefficient

coefplot(delay3.lm)


coefplot(delay3.lm2)

# Consider the interaction with the day of incident

delay3.lm3 <- lm(delay ~ incident * day, data = delay3)
summary(delay3.lm3)

coefplot(delay3.lm3)

# Studying monthly distribution of the delay

month.col <- str_sub(delay3$date,start = 6, end = 7)

month.name <- function(x){
    
    switch(x,
           "01"="01Jan",
           "02"="02Feb",
           "03"="03Mar",
           "04"="04Apr",
           "05"="05May",
           "06"="06Jun",
           "07"="07Jul",
           "08"="08Aug",
           "09"="09Sep",
           "10"="10Oct",
           "11"="11Nov",
           "12"="12Dec"
           )
    
}

month.names <- sapply(month.col, month.name)

delay4 <- data.frame(delay3,month.names)

ggplot(delay4, aes(x= delay,fill=month.names))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~month.names)


yearly <- str_sub(delay4$date,start = 1, end = 4)

delay5 <- data.frame(delay4,yearly)

ggplot(delay5, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~yearly)

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
           "09"="Fall",
           "10"="Fall",
           "11"="Fall",
           "12"="Winter"
    )
    
}
season.names <- sapply(month.col, season)

delay6 <- data.frame(delay5,season.names)

ggplot(delay6, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~season.names)+theme(legend.position = "top")

ggplot(delay6, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~yearly+season.names)


# Analyze Incidents per location

delay6$location <- as.factor(delay6$location)

levels(delay6$location)

location2 <- aggregate(incident ~ location, delay6 ,length)
location3 <- location2[order(-location2$incident),]
summary(location3)

quantile(location3$incident, probs = .995)

length(location3$incident[location3$incident > 1000])

#x1 <- c(50 < df3$route)

#df95 <- df3[50 < df3$route & df3$route < 70,]


#s <- ggplot(df95, aes(x = route, fill = incident, label = incident))
#s+ geom_bar(position = "fill",width = .5)+coord_flip()

#t <- ggplot(df95, aes(x = route, fill = incident))
#t+ geom_bar(position = "fill",width = .5)+coord_flip()+theme(legend.position = "top")

# +scale_fill_manual(values = c("Diversion","Emergency","General","Investigation","Late","Mechanical","Utilized Off Route"))

# Create a prediction model for the delay through linear regression


    
index <- sample(length(delay6$route), .7 *length(delay6$route))

train <- delay6[index,]
test <- delay6[-index,]



#delay_Multi <- lm(delay ~ location+day+incident+direction+vehicle, data = train)

delay_single <- lm(delay ~ incident-1, data = train)

summary(delay_single)



delay_pred <- predict(delay_single, newdata = test, interval = "prediction")

pred <- data.frame(delay_pred)


error_pred <- pred$fit - test$delay

error <- data.frame(error_pred)
head(error)

hist(error_pred)

ggplot(error,aes(x=error_pred))+geom_histogram(binwidth = 1,alpha=3/4)

summary(error_pred)

mse_single <- sqrt(sum((error_pred)^2)/nrow(test))

mse_single

relative_error <- 1- ((test$delay - abs(error_pred))/test$delay)
relative_error
relative_error_less25 <- table(relative_error < .25)["TRUE"]/nrow(test)
relative_error_less25_pct <- paste("relative_error_less 25% is ",100*relative_error_less25)
relative_error_less25_pct




Mechanical_mean <- mean(delay6$delay[delay6$incident == "Mechanical"])

Mechanical_mean

Mechanical_sd <- sd(delay6$delay[delay6$incident == "Mechanical"])

Mechanical_var <- var(delay6$delay[delay6$incident == "Mechanical"])



Population_mean <- mean(delay6$delay)

Population_mean

Population_sd <- sd(delay6$delay)

Population_sd

Population_var <- var(delay6$delay)

Population_var

n <- length(delay6$delay[delay6$incident == "Mechanical"])

n

z <- (Mechanical_mean - Population_mean)/(Population_sd/sqrt(n))

z

z1 <- (Mechanical_mean - Population_mean)/(Mechanical_sd/sqrt(n))

p_value <- pnorm(z)

p_value


# side by side histograms of whole delays and delays due to Mechanical reasons

ggplot(delay6, aes(x=delay))+geom_histogram(binwidth = 1,alpha=3/4)


delay6_mechanical <- delay6[delay6$incident == "Mechanical",]

ggplot(delay6_mechanical, aes(x=delay))+geom_histogram(binwidth = 1,alpha=3/4)



#Pearson correlation test between Delay and Gap


cor.test(delay6$delay,delay6$gap, method = "pearson")



delay_Multi <- lm(delay ~ incident-1+gap, data = train)

summary(delay_Multi)



delay_pred <- predict(delay_Multi, newdata = test, interval = "prediction")

pred_Multi <- data.frame(delay_pred)

error_pred_multi <- pred_Multi$fit - test$delay

error_multi <- data.frame(error_pred_multi)
head(error_multi)

hist(error_pred_multi)

ggplot(error_multi,aes(x=error_pred_multi))+geom_histogram(binwidth = 1,alpha=3/4)

summary(error_pred_multi)



