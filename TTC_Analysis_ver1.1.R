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


delay <- read.csv("D:/DataAnalytics/CKME_136/TTC_Bus_Delay/Capstone/combined.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")

head(df)

df$Incident <- as.factor(df$Incident)

df$Day <- as.factor(df$Day)

class(df$Incident)

class(df$day)

summary(df$incident)

names(df) <- c("date","route","time","day","location","incident","delay","gap","direction","vehicle")


df2 <- df[which(!is.na(df$delay)),]


ggplot(df2,aes(y=delay,x=incident))+geom_boxplot()

df3 <- df2[which(df2$delay < 60),]

ggplot(df3,aes(y=delay,x=incident))+geom_boxplot()


aggregate(delay ~ incident, data = df3, mean)

aggregate(delay ~ incident, data = df3, var)

ggplot(df3, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~incident)

ggplot(df3, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~incident)+theme(legend.position = "top")

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
           "09"="Fall",
           "10"="Fall",
           "11"="Fall",
           "12"="Winter"
    )
    
}
season2 <- sapply(month2, season)
df6 <- data.frame(df5,season2)

ggplot(df6, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~season2)+theme(legend.position = "top")

ggplot(df6, aes(x= delay,fill=incident))+geom_histogram(binwidth = 1,alpha=3/4)+facet_wrap(~year2+season2)


df5$location <- as.factor(df5$location)

levels(df5$location)

location2 <- aggregate(incident ~ location, df5 ,length)
location3 <- location2[order(-location2$incident),]
summary(location3)

quantile(location3$incident, probs = .995)

length(location3$incident[location3$incident > 1000])

x1 <- c(50 < df3$route)

df95 <- df3[50 < df3$route & df3$route < 70,]


s <- ggplot(df95, aes(x = route, fill = incident, label = incident))
s+ geom_bar(position = "fill",width = .5)+coord_flip()

t <- ggplot(df95, aes(x = route, fill = incident))
t+ geom_bar(position = "fill",width = .5)+coord_flip()+theme(legend.position = "top")

# +scale_fill_manual(values = c("Diversion","Emergency","General","Investigation","Late","Mechanical","Utilized Off Route"))



sum(!is.na(delay$route))

df6$day <- as.character(df6$day)

delay.df <- df6[df6$delay < 60,]

class(delay.df$day)
    
index <- sample(length(delay.df$route), .7 *length(delay.df$route))

train <- delay.df[index,]
test <- delay.df[-index,]



delay_Multi <- lm(delay ~ location+day+incident+direction+vehicle, data = train)

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




Mechanical_mean <- mean(df3$delay[df3$incident == "Mechanical"])

Mechanical_mean

Mechanical_sd <- sd(df3$delay[df3$incident == "Mechanical"])

Mechanical_var <- var(df3$delay[df3$incident == "Mechanical"])



Population_mean <- mean(df3$delay)

Population_mean

Population_sd <- sd(df3$delay)

Population_sd

Population_var <- var(df3$delay)

Population_var

n <- length(df3$delay[df3$incident == "Mechanical"])

n

z <- (Mechanical_mean - Population_mean)/(Population_sd/sqrt(n))

z

z1 <- (Mechanical_mean - Population_mean)/(Mechanical_sd/sqrt(n))

p_value <- pnorm(z)

p_value


# side by side histograms of whole delays and delays due to Mechanical reasons

ggplot(df3, aes(x=delay))+geom_histogram(binwidth = 1,alpha=3/4)


df3_mechanical <- df3[df3$incident == "Mechanical",]

ggplot(df3_mechanical, aes(x=delay))+geom_histogram(binwidth = 1,alpha=3/4)









Late_mean <- mean(df3$delay[df3$incident == "Investigation"])


Delay_mean <- mean(df3$delay)

Delay_sd <- sd(df3$delay)

n1 <- length(df3$delay[df3$incident == "Investigation"])



#Pearson correlation test between Delay and Gap


cor.test(df3$delay,df3$gap, method = "pearson")


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
