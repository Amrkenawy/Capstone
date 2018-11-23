# Load readxl & stringr libraries

library(readxl)
library(stringr)
library(ggplot2)
require(coefplot)
library(tree)
library(rpart)


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

class(delay$Direction)


# Clean Direction field

New_Direction <- delay$Direction
class(New_Direction)

direct_fun <- function(x){
    
    switch(x,
           "W/N"="WNB","w/b"="WB","b/w"="BW","w.b"="WB","w"="WB","UP"="NB","u"="NB",
           "SS"="SB","s/d"="SB","s/b"="SB","s"="SB","Ou"="NB","W"="WB","N"="NB",
           "S"="SB","E"="EB","n"="NB","s"="SB","e"="EB","WB"="WB","NB"="NB","SB"="SB",
           "EB"="EB","BW"="BW","E/W"="BW","N/E"="NE","BW's"="BW","BWS"="BW","B/W's"="BW",
           "b/ws"="BW","b/b"="BW","'N"="NB", "N & S"="BW","1w/b"="WB"

    )
}

direct_fun2 <- function(x){
    
    switch(x,"N"="NB",
           ",n/b"="NB",
           ". E"="EB",
           "/B"="BW",
           "/nb"="NB",
           "?W"="WB",
           "`b/w"="BW",
           "`BW"="BW",
           "`E/B"="EB",
           "`EB"="EB",
           "`N"="NB",
           "`n/b"="NB",
           "`nb"="NB",
           "`S"="SB",
           "`S/B"="SB",
           "`SB"="SB",
           "`W"="WB",
           "`W/B"="WB",
           "`WB"="WB",
           "0EB"="EB",
           "0s"="SB",
           "10n/b"="NB",
           "143 West a"="WB",
           "15nb"="NB",
           "1w/b"="WB",
           "20S/B"="SB",
           "23447sb"="SB",
           "53940wb"="WB",
           "55102eb"="EB",
           "56653wb"="WB",
           "5s/b"="SB",
           "65567wb"="WB",
           "66161WB"="WB",
           "68021eb"="EB",
           "68735N"="NB",
           "70295W"="WB",
           "72276eb"="EB",
           "7up"="NB",
           "B"="BW",
           "B / W"="BW",
           "B /w"="BW",
           "B W"="BW",
           "B ways"="BW",
           "B."="BW",
           "B./w"="BW",
           "B.W"="BW",
           "b/"="BW",
           "B/ W"="BW",
           "b/.w"="BW",
           "b//w"="BW",
           "b/b"="BW",
           "B/D"="BW",
           "b/e"="BW",
           "B/E's"="BW",
           "B/S"="BW",
           "B/T"="BW",
           "b/v"="BW",
           "b/w"="BW",
           "B/W 6"="BW",
           "b/w."="BW",
           "B/W/"="BW",
           "B/W`"="BW",
           "b/w1"="BW",
           "B/W6"="BW",
           "B/W8"="BW",
           "B/ways"="BW",
           "B/Wb"="BW",
           "B/WE"="BW",
           "B/WN"="BW",
           "b/ws"="BW",
           "B/W's"="BW",
           "B/W's`"="BW",
           "B/Ww"="BW",
           "B/Wys"="BW",
           "b/y"="BW",
           "b;/w"="BW",
           "B?W"="BW",
           "b/w"="BW",
           "B/W's"="BW",
           "b30."="BW",
           "bbw"="BW",
           "BD"="BW",
           "be"="BW",
           "bh/w"="BW",
           "BN"="BW",
           "BN/W"="BW",
           "BN=====NB"="BW",
           "BNB"="BW",
           "Both"="BW",
           "Both Directions"="BW",
           "Both way"="BW",
           "Both ways"="BW",
           "Bothway"="BW",
           "bothways"="BW",
           "bqw"="BW",
           "BS"="BW",
           "BT"="BW",
           "bv"="BW",
           "BV/W"="BW",
           "BW"="BW",
           "B-W"="BW",
           "BW."="BW",
           "BW@"="BW",
           "bw`"="BW",
           "bw020"="BW",
           "BW14"="BW",
           "B-Way"="BW",
           "B-ways"="BW",
           "BWE"="BW",
           "BWS"="BW",
           "BW's"="BW",
           "bwy"="BW",
           "DB"="BW",
           "dw"="BW",
           "E"="EB",
           "e & w"="BW",
           "E /W"="BW",
           "e b"="EB",
           "ee"="EB",
           "E#"="EB",
           "E."="EB",
           "E./B"="EB",
           "E./W"="BW",
           "e.b"="EB",
           "e/"="EB",
           "e/ b"="EB",
           "E//B"="EB",
           "e/;b"="EB",
           "e/b"="EB",
           "E/B 1st Bus #1525, 3 run at 6.00 PM"="EB",
           "E/B."="EB",
           "e/b`"="EB",
           "e/b0"="EB",
           "e/b1"="EB",
           "e/b3"="EB",
           "E/bB"="EB",
           "e/bn/b"="EB",
           "e/e"="EB",
           "E/E/B"="EB",
           "E/NB"="BW",
           "E?B"="EB",
           "e/b"="EB",
           "e`"="EB",
           "E13"="EB",
           "e18"="EB",
           "E4"="EB",
           "Easr"="EB",
           "East"="EB",
           "EB"="EB",
           "EB & WB"="EB",
           "EB 4"="EB",
           "EB."="EB",
           "EB/NB"="EB",
           "eb/wb"="EB",
           "EB/"="EB",
           "EB`"="EB",
           "EB20"="EB",
           "eb3"="EB",
           "eb5"="EB",
           "Eb72"="EB",
           "ebnb"="EB",
           "Ebq"="EB",
           "EBWB"="EB",
           "ed"="EB",
           "ee"="EB",
           "eeb"="EB",
           "EG"="EB",
           "EN"="EB",
           "ENB"="EB",
           "eq"="EB",
           "er/b"="EB",
           "ES"="EB",
           "Esat"="EB",
           "EVB"="EB",
           "EW"="EB",
           "ew/b"="EB",
           "N"="NB",
           "N'"="NB",
           "N 10"="NB",
           "n b"="NB",
           "N."="NB",
           "N./B"="NB",
           "N.B"="NB",
           "n/"="NB",
           "N/ B"="NB",
           "n/.b"="NB",
           "N//b"="NB",
           "n/b"="NB",
           "n/b'"="NB",
           "n/'b"="NB",
           "n/b/B"="NB",
           "n/b/W"="NB",
           "N/B`"="NB",
           "n/b+"="NB",
           "N/B0"="NB",
           "N/B150"="NB",
           "N/Bb"="NB",
           "n/bn"="NB",
           "N/Bw"="NB",
           "n/m"="NB",
           "n/N"="NB",
           "N/R"="NB",
           "n/s"="BW",
           "n/s/"="BW",
           "n/v"="NB",
           "n/vb"="NB",
           "N?B"="NB",
           "n\b"="NB",
           "N`"="NB",
           "N+"="NB",
           "N13"="NB",
           "N320"="NB",
           "N5"="NB",
           "n8"="NB",
           "NB 11"="NB",
           "nb."="NB",
           "NB/SB"="NB",
           "NB/WB"="NB",
           "NB`"="NB",
           "NB3"="NB",
           "NB4"="NB",
           "nb7"="NB",
           "nbbw"="NB",
           "NBN"="NB",
           "NBSB"="NB",
           "NBW"="NB",
           "NM"="NB",
           "NN"="NB",
           "Nnb"="NB",
           "North"="NB",
           "NS"="NB",
           "ns/b"="NB",
           "NW"="NB",
           "N-W"="NB",
           "s"="SB",
           "S & N"="SB",
           "S 10"="SB",
           "s b"="SB",
           "S N"="SB",
           "S."="SB",
           "S./b"="SB",
           "s.b"="SB",
           "s.b."="SB",
           "s/"="SB",
           "S/.B"="SB",
           "s//b"="SB",
           "s/b"="SB",
           "s/b 4"="SB",
           "s/b e/b"="SB",
           "S/b from Lawrencea"="SB",
           "s/b to w/b"="SB",
           "s/b?"="SB",
           "s/b`"="SB",
           "s/b+"="SB",
           "s/bb/w"="SB",
           "S/Be/B"="SB",
           "s/bh"="SB",
           "s/bn"="SB",
           "s/bn/b"="SB",
           "s/bv"="SB",
           "s/d"="SB",
           "s/e"="SB",
           "S/N"="SB",
           "S/NB"="SB",
           "s/s"="SB",
           "s/v"="SB",
           "s/w"="SB",
           "S?B"="SB",
           "s/b"="SB",
           "S/N"="SB",
           "S`"="SB",
           "S`Jane and Lawrence"="SB",
           "s=n/b"="SB",
           "S0"="SB",
           "S1"="SB",
           "s10"="SB",
           "S29"="SB",
           "S9"="SB",
           "SB"="SB",
           "SB & NB"="SB",
           "SB only"="SB",
           "sb."="SB",
           "SB/EB"="SB",
           "SB`"="SB",
           "SB12"="SB",
           "SB3"="SB",
           "SB4"="SB",
           "SBB"="SB",
           "SBEB"="SB",
           "sbnb"="SB",
           "sbSB"="SB",
           "sbwb"="SB",
           "SD/B"="SB",
           "SDB"="SB",
           "se"="SB",
           "SE/B"="SB",
           "SEB"="SB",
           "SS"="SB",
           "ss/b"="SB",
           "SSB"="SB",
           "w"="WB",
           "W (s)"="WB",
           "w b"="WB",
           "w."="WB",
           "w./b"="WB",
           "w.b"="WB",
           "W/"="WB",
           "w//b"="WB",
           "w/b"="WB",
           "W/B 5"="WB",
           "W/B."="WB",
           "W/B/"="WB",
           "W/B`"="WB",
           "w/b17"="WB",
           "w/be/b"="WB",
           "w/bs/b"="WB",
           "W/E"="WB",
           "w/n"="WB",
           "W/S"="WB",
           "w/v"="WB",
           "W/W"="WB",
           "W?B"="WB",
           "w/"="WB",
           "w/b"="WB",
           "W/N"="WB",
           "w`"="WB",
           "W+"="WB",
           "W=EB"="WB",
           "w1"="WB",
           "W10"="WB",
           "w2"="WB",
           "W5"="WB",
           "W9"="WB",
           "war"="WB",
           "WB"="WB",
           "wb2"="WB",
           "WB."="WB",
           "wb/"="WB",
           "WB/EB"="WB",
           "wb/w"="WB",
           "Wb/B"="WB",
           "WB`"="WB",
           "wb3"="WB",
           "WB5"="WB",
           "wbnb"="WB",
           "WB\'s"="WB",
           "WBW"="WB",
           "WE"="WB",
           "we/b"="WB",
           "web"="WB",
           "West"="WB",
           "WestS"="WB",
           "Wet"="WB",
           "WM"="WB",
           "wn"="WB",
           "WN/B"="WB",
           "WQ"="WB",
           "WS"="WB",
           "WSB"="WB",
           "wv"="WB",
           "wW"="WB",
           "WWB"="WB",
           "W/N"="WNB","w/b"="WB","b/w"="BW","w.b"="WB","w"="WB","UP"="NB","u"="NB",
           "SS"="SB","s/d"="SB","s/b"="SB","s"="SB","Ou"="NB","W"="WB","N"="NB",
           "S"="SB","E"="EB","n"="NB","s"="SB","e"="EB","WB"="WB","NB"="NB","SB"="SB",
           "EB"="EB","BW"="BW","E/W"="BW","N/E"="NE","BW's"="BW","BWS"="BW","B/W's"="BW",
           "b/ws"="BW","b/b"="BW","'N"="NB", "N & S"="BW","1w/b"="WB"
           )
}
New_Direction <- sapply(New_Direction,direct_fun2)

New_Direction <- as.character(New_Direction)
class(New_Direction)

sum(New_Direction == "NULL")

compare <- data.frame[New_Direction,New_Direction2]

delay$Day <- as.factor(delay$Day)

class(delay$Incident)
class(delay$direction)
class(delay$Day)

summary(delay$Incident)

delay$Direction <- sapply(delay$Direction,direct_fun)

# Data Cleaning

# Renaming the columns

names(delay) <- c("SrNo","date","route","time","day","location","incident","delay","gap","direction","vehicle")

delay <- delay[delay$direction != "NULL",]

# removing "NAs"

delay2 <- delay[which(!is.na(delay$delay)),]


ggplot(delay2,aes(y=delay,x=incident))+geom_boxplot()

# removing outliers

delay3 <- delay2[which(delay2$delay < 60 & delay2$delay > 0 ),]

head(delay3)

class(delay3$date)

#delay3$date <- as.Date(delay3$date, format = "%m/%d/%Y")

ggplot(delay3,aes(y=delay,x=incident))+geom_boxplot()


aggregate(delay ~ incident, data = delay3, mean)

aggregate(delay ~ incident, data = delay3, var)

ggplot(delay3, aes(x= delay,fill=incident))+geom_histogram(binwidth = 3,alpha=3/4)+facet_wrap(~incident)

ggplot(delay3, aes(x= delay,fill=incident))+geom_histogram(binwidth = 3,alpha=3/4)+facet_wrap(~incident)+theme(legend.position = "top")

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
class(month.col)

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
class(month.names)
delay4 <- data.frame(delay3,month.names)

ggplot(delay4, aes(x= delay,fill=month.names))+geom_histogram(binwidth = 3,alpha=3/4)+facet_wrap(~month.names)


yearly <- str_sub(delay4$date,start = 1, end = 4)

delay5 <- data.frame(delay4,yearly)

ggplot(delay5, aes(x= delay,fill=incident))+geom_histogram(binwidth = 3,alpha=3/4)+facet_wrap(~yearly)

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

ggplot(delay6, aes(x= delay,fill=incident))+geom_histogram(binwidth = 3,alpha=3/4)+facet_wrap(~season.names)+theme(legend.position = "top")

ggplot(delay6, aes(x= delay,fill=incident))+geom_histogram(binwidth = 3,alpha=3/4)+facet_wrap(~yearly+season.names)


# Analyze Incidents per location

delay6$location <- as.factor(delay6$location)

levels(delay6$location)

delay6$direction <- as.character(delay6$direction)
delay6$direction <- as.factor(delay6$direction)

levels(delay6$direction)

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

ggplot(error,aes(x=error_pred))+geom_histogram(binwidth = 3,alpha=3/4)

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
cor.test(delay6$incident,delay6$direction, method = "pearson")


delay_Multi <- lm(delay ~ incident-1+direction, data = train)

summary(delay_Multi)



delay_pred <- predict(delay_Multi, newdata = test, interval = "prediction")

pred_Multi <- data.frame(delay_pred)

error_pred_multi <- pred_Multi$fit - test$delay

error_multi <- data.frame(error_pred_multi)
head(error_multi)

hist(error_pred_multi)

ggplot(error_multi,aes(x=error_pred_multi))+geom_histogram(binwidth = 1,alpha=3/4)

summary(error_pred_multi)




delay_Multi <- lm(delay ~ incident-1+direction, data = train)

summary(delay_Multi)




delay_pred <- predict(delay_Multi, newdata = test, interval = "prediction")

pred_Multi <- data.frame(delay_pred)

error_pred_multi <- pred_Multi$fit - test$delay

error_multi <- data.frame(error_pred_multi)
head(error_multi)

hist(error_pred_multi)

ggplot(error_multi,aes(x=error_pred_multi))+geom_histogram(binwidth = 1,alpha=3/4)

summary(error_pred_multi)

#write.csv(delay6,"D:/DataAnalytics/CKME_136/TTC_Bus_Delay/Capstone/combined2.csv")

mse_multi <- sqrt(sum((error_multi)^2)/nrow(test))

mse_multi


tree_model <- tree(delay ~ incident, data = test)
plot(tree_model)
text(tree_model)

class(delay6$direction)
levels(delay6$direction)

delay_pred_tree <- predict(tree_model, newdata = test, interval = "prediction")

pred_tree <- data.frame(delay_pred_tree)

error_pred_tree <- pred_tree - test$delay

error_tree <- data.frame(error_pred_tree)
head(error_tree)

hist(error_pred_multi)

mse_tree <- sqrt(sum((error_tree)^2)/nrow(test))

mse_tree


tree_model_multi <- tree(delay ~ incident+direction, data = test)
plot(tree_model_multi)
text(tree_model_multi)

class(delay6$direction)
levels(delay6$direction)

delay_pred_tree_multi <- predict(tree_model_multi, newdata = test, interval = "prediction")

pred_tree_multi <- data.frame(delay_pred_tree_multi)

error_pred_tree_multi <- pred_tree_multi - test$delay

error_tree_multi <- data.frame(error_pred_tree_multi)
head(error_tree_multi)

hist(error_pred_multi)


mse_tree_multi <- sqrt(sum((error_tree_multi)^2)/nrow(test))

mse_tree_multi

