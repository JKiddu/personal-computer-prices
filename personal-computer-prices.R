# First regression in R
# Authors: Amit, Gunnika, Karan, Ravinder, Sparsh
# Date: June 19, 2019
# -----------------------------------------------------------------------
# Install relevant libraries
install.packages("xlsx")
install.packages("olsrr")
install.packages("psych")
# -----------------------------------------------------------------------
# Load in relevant libraries
library(xlsx)
library(olsrr)
library(psych)
# -----------------------------------------------------------------------
# Load in the data
com <- read.csv("C:/Users/Dell/Desktop/computers.csv")
# -----------------------------------------------------------------------
# Clean the data
# Cast the data objects into a data frame
com <- data.frame(com)
# -----------------------------------------------------------------------
# Add New Columns
com$Year<-0



com$Month <- 0
com$Quarter <- 0

# -----------------------------------------------------------------------
# seasonality (month, year)

count = 1
for ( i in com$trend){
  if(i > 24 & i < 36){
    com$Year[count] <- 1995
  }
  else if(i > 12 & i < 25 ){
    com$Year[count] <- 1994
  }
  else
    com$Year[count] <- 1993
  count= count + 1
}


count = 1
for (i in com$trend){
  if(i == 1 | i == 13 | i == 25){
    com$Month[count] <- 1
    com$Quarter[count] <- 1
  }
  else if(i == 2 | i == 14 | i ==26){
    com$Month[count] <- 2
    com$Quarter[count] <- 1
  }
  else if(i == 3 | i == 15 | i ==27){
    com$Month[count] <- 3
    com$Quarter[count] <- 1
  }
  else if(i == 4 | i == 16 | i ==28){
    com$Month[count] <- 4
    com$Quarter[count] <- 2
  }
  else if(i == 5 | i == 17 | i ==29){
    com$Month[count] <- 5
    com$Quarter[count] <- 2
  }
  else if(i == 6 | i == 18 | i == 30){
    com$Month[count] <- 6
    com$Quarter[count] <- 2
  }
  else if(i == 7 | i == 19 | i == 31){
    com$Month[count] <- 7
    com$Quarter[count] <- 3
  }
  else if(i == 8 | i == 20 | i == 32){
    com$Month[count] <- 8
    com$Quarter[count] <- 3
  }
  else if(i == 9 | i == 21 | i == 33){
    com$Month[count] <- 9
    com$Quarter[count] <- 3
  }
  else if(i == 10 | i == 22 | i == 34){
    com$Month[count] <- 10
    com$Quarter[count] <- 4
  }
  else if(i == 11 | i == 23 | i == 35){
    com$Month[count] <- 11
    com$Quarter[count] <- 4
  }
  else{
    com$Month[count] <- 12
    com$Quarter[count] <- 4
  }
  
  count = count + 1
}

# ------------------------------------------------------------------------
# Confirm the column names
colnames(com)
colnames(com) <- c("Id", "Price", "Speed in Mhz", "HDD size(MB)", "RAM size(MB)", "Screen size(IN)", "CDROM", "Multimedia kit", "Preminum","No. of ads", "Trend", "Year", "Month", "Quarter")

levels(com$CDROM)  # for a categorical variable it will give factor
levels(com$`Multimedia kit`)
levels(com$Preminum)

com$CDROM <- as.factor(com$CDROM) #confirm categorical
com$Preminum <- as.factor(com$Preminum) #confirm categorical
com$`Multimedia kit` <- as.factor(com$`Multimedia kit`) #confirm categorical

com$Price <- as.numeric(com$Price)#confirm numeric
com$`Speed in Mhz` <- as.numeric(com$`Speed in Mhz`)#confirm numeric
com$`HDD size(MB)` <- as.numeric(com$`HDD size(MB)`)#confirm numeric
com$`RAM size(MB)` <- as.numeric(com$`RAM size(MB)`)#confirm numeric
com$`Screen size(IN)` <- as.numeric(com$`Screen size(IN)`)#confirm numeric
com$`No. of ads` <- as.numeric(com$`No. of ads`)#confirm numeric
com$Trend <- as.numeric(com$Trend)#confirm numeric
com$Year <- as.numeric(com$Year)#confirm numeric
com$Month <- as.numeric(com$Month)#confirm numeric
com$Quarter <- as.numeric(com$Quarter)#confirm numeric


# -----------------------------------------------------------------------
# Changing categorical data to numerical values
#com$CDROM <- as.numeric(com$CDROM)
#com$`Multimedia kit` <- as.numeric(com$`Multimedia kit`)
#com$Preminum <- as.numeric(com$Preminum)

# -----------------------------------------------------------------------
# Attach data
attach(com)
# -----------------------------------------------------------------------
# Review the data
View(com)
summary(com) # Summary
str(com)#structure of the data and allows us to see the data type
dim(com)#dimension of data
class(com)#numeric,matrix etc
describe(com)# Descriptive Analysis

# Create Boxplot
boxplot(Price~`Screen size(IN)`, main="Price vs Screen Size boxplot", xlab="Size of Screen in Inches", ylab="Price",col=(c("blue","red","green")))



# Generate histogram
hist(Price , xlab="Price", ylab="No.of items", breaks = 10, main = 'Price Distribution Histogram', col="lightblue3", border="white") # cex changes font size

# Create scatterplots
plot(`Screen size(IN)` , Price, main = "Price vs Screen Size Scatterplot", pch=19)

# -----------------------------------------------------------------------
# Simple Linear Regression 
fit <-lm(Price ~ `Screen size(IN)`) # This is the regression function
summary(fit) # Show results
com$price_screen_predict<-predict(fit)
# Various functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters
influence(fit) 

# -----------------------------------------------------------------------
# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2))
plot(fit)

# -----------------------------------------------------------------------
# Multiple Regression  
fit <-lm(com$Price ~ `Screen size(IN)` + `HDD size(MB)` + `RAM size(MB)` + `Screen size(IN)` + com$CDROM + `Multimedia kit` + Preminum + `No. of ads` + Year + Quarter)

summary(fit) # Show results
com$multi_predict<-predict(fit)
# Various functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters
influence(fit)

# -----------------------------------------------------------------------
# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2))
plot(fit)

# -----------------------------------------------------------------------
# Variable Selection
model <- lm(com$Price ~ `Screen size(IN)` + `HDD size(MB)` + `RAM size(MB)` + `Screen size(IN)` + com$CDROM + `Multimedia kit` +Trend + Preminum + `No. of ads` + Year + Month + Quarter,data = com)

# FSS
k <- ols_step_forward_aic(model, details = TRUE)
plot(k)

# BSS
ki <- ols_step_backward_aic(model, details = TRUE)
plot(ki)

e# All subset
pe<-step(lm(com$Price ~ `Screen size(IN)` + `HDD size(MB)` + `RAM size(MB)` + `Screen size(IN)` + com$CDROM + `Multimedia kit` + Preminum + `No. of ads` + Year + Month + Quarter,
        data=com),direction="both")

plot(pe)
speed<-lm(com$Price~com$`Speed in Mhz`)
summary(speed)
com$price_speed_predict<-predict(speed)
hd<-lm(com$Price~com$`HDD size(MB)`)
summary(hd)
com$price_hd_predict<-predict(hd)
ram<-lm(com$Price~com$`RAM size(MB)`)
summary(ram)
ram1<-predict(ram,interval="confidence", level=.95)

com$price_ram_predict<-predict(ram)
cdrom<-lm(com$Price~com$CDROM)
summary(cdrom)
com$price_cdrom_predict<-predict(cdrom)
multi<-lm(com$Price~com$`Multimedia kit`)
summary(multi)
com$price_multi_predict<-predict(multi)
premium<-lm(com$Price~com$Preminum)
summary(premium)
com$price_premium_predict<-predict(premium)
ads<-lm(com$Price~com$`No. of ads`)
summary(ads)
com$price_ads_predict<-predict(ads)
year<-lm(com$Price~com$Year)
summary(year)
com$price_year_predict<-predict(year)
quarter<-lm(com$Price~com$Quarter)
summary(quarter)
com$price_quarter_predict<-predict(quarter)
month<-lm(com$Price~com$Month)
summary(month)
cor(com$Price,com$Month)
# Save or export your plots to your directory
# Print out the results from the regression to a file

boxplot(com$Price~com$Year,main=" price vs year",xlab=" year", ylab=" price of computers",col=(c("blue","red","green")))
