# Report has been compiled using R markdown
# Reasoning and results have been discusssed in comments

library(readr)
library(car)

train.data = read_csv("C:/Users/Pulkit/Desktop/ISEN/613 Engg Data Analysis/Midterm/Training.csv")
#711 observations of 11 variables


  
test.data = read_csv("C:/Users/Pulkit/Desktop/ISEN/613 Engg Data Analysis/Midterm/Test.csv")
#20 observations of 11 variables

#Part a
lm.fit1 = lm(casual~., data = train.data)

#part b
summary(lm.fit1)
# Results
# season may be affecting (close)
# Year is affecting
# holiday & weekday slightly affects
# weathersit is a affecting
# windspeed is influencing
# even though the F statistic is high (45.34) and gives a 
# low p value implying predictors are influencing the response, 
# the R2 is not so good (.3931), 
# implying there may be predictors which are irrelvant
# there is room for development


#Part c: Diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit1)
# slight trend is present---use nonlinear transformations of predictors
# funnel yes---try taking log of response to stablize variance
# outlier no (no value is above 3)
# high leverage yes (pt 579). Needs further analysis. Effect after transformations etc.
vif(lm.fit1)
#values above 5---remove one of temp or atemp

#Part d
train.data.new = train.data[,-8]
#remove atemp from training data

pairs(train.data.new)
#use plot zoom
#low casual in first season
#low casual in initial year
#low casual in dec, jan, feb
#low casual in holidays
#low values in weekdays, high in sat & sun
#very low casual in weather condition 3: light snow, rain
#low casual in high wind speeds

#colinearity b/w temp and atemp


#box plot is required to study confidence intervals
graphics.off()
par("mar")
par(mar=c(5,4,1,1))
par(mfrow = c(3,3))

boxplot(casual~season, data = train.data.new, 
        ylab = "casual", xlab = "season")
boxplot(casual~year, data = train.data.new, 
        ylab = "casual", xlab = "year")
boxplot(casual~month, data = train.data.new, 
        ylab = "casual", xlab = "month")
boxplot(casual~holiday, data = train.data.new, 
        ylab = "casual", xlab = "holiday")
boxplot(casual~weekday, data = train.data.new, 
        ylab = "casual", xlab = "weekday")
boxplot(casual~weathersit, data = train.data.new, 
        ylab = "casual", xlab = "weathersit")
boxplot(casual~temp, data = train.data.new, 
        ylab = "casual", xlab = "temp")
boxplot(casual~hum, data = train.data.new, 
        ylab = "casual", xlab = "humidity")
boxplot(casual~windspeed, data = train.data.new, 
        ylab = "casual", xlab = "windspeed")

# first season give lower value of casual
# 1,2 and 12 month gives lower value
# sat and sunday give lower values
# weathersit == 3 gives lower values
# low temp give lower values
# high windspeed gives low values




lm.fit2 = lm(casual~., data = train.data.new)
summary(lm.fit2)
# compare with lm.fit1
# humidity is not influencing
# temp is influencing
# season & month doesn't seem to be great influencers

# remove humidity
lm.fit3 = lm(casual~., data = train.data.new[,-c(8)])
summary(lm.fit3)

# corrobate results for season and month
# information provided by season & month may be a result of environmental conditions
# might be a good idea to use predictors such as temp, windspeed for general application

# try removing month and humidity
lm.fit4 = lm(casual~., data = train.data.new[,-c(8,3)])
summary(lm.fit4)

# try removing month, humidity & season
lm.fit5 = lm(casual~., data = train.data.new[,-c(8,3,1)])
summary(lm.fit5)
# values of R square and RSE are nearly similar
# implying they are not influencing the model
# conclusion remove month, humidity & season

par(mfrow = c(2,2))
plot(lm.fit5)
# looks like trend is slightly reduced
# non constant variance is present
# absolute diff of leverage (very low when compared to initial values)
# no outliers, all values are less than 3
vif(lm.fit5)
# no colinearity



pairs(~temp+windspeed+weathersit, data = train.data.new)
#no conceivable relation between these three
pairs(~holiday+weekday, data = train.data.new)
#no conceivable relation between these two

#try removing weathersit
lm.fit6 = lm(casual~., data = train.data.new[,-c(8,3,1,6)])
summary(lm.fit6)
#error increases to 551, R2 decreases

#try removing weathersit & temp
lm.fit7 = lm(casual~., data = train.data.new[,-c(8,3,1,6,7)])
summary(lm.fit7)
#error increases to 654, R2 decreases

lm.fit8 = lm(casual~., data = train.data.new[,-c(8,3,1,6,9)])
summary(lm.fit8)
#error increases to 554, R2 decreases

# conclusion
# variables used in lm.fit5 are necessary predictors
# lm.fit5 = lm(casual~., data = train.data.new[,-c(8,3,1)])


#try taking log of response
train.data.new1a = train.data.new[,-c(8,3,1)]
train.data.new1a[,7] = log(train.data.new1a[,7])
lm.fit9 = lm(casual~., data = train.data.new1a)
summary(lm.fit9)
par(mfrow = c(2,2))
plot(lm.fit9)
# problem of heteroscedasticity is highly reduced

# R square becomes 0.55, error gets logged as well!!!


# try combinations of all variables
train.data.new2 = train.data.new[,-c(8,3,1)]

lm.fit10 = lm(casual~year*holiday+year*weekday+year*weathersit
              +year*temp+year*windspeed + 
              holiday*weekday+holiday*weathersit+
              holiday*temp+ holiday*windspeed + 
              weekday*weathersit+weekday*temp+weekday*windspeed+ 
              weathersit*temp+weathersit*windspeed + 
              temp*windspeed  
              , data = train.data.new2)
summary(lm.fit10)
# only three pairs appear to be important


lm.fit11 = lm(casual~temp+year:temp+holiday:temp,
              data = train.data.new2)
summary(lm.fit11)
# R square decreases, error increases

# use all variables of lm.fit5 with the interactions
lm.fit12 = lm(casual~year+holiday+weekday+weathersit+windspeed+
                temp+
                year:temp + holiday:temp,
              data = train.data.new2)
summary(lm.fit12)
# results are improved, R square ups to 0.39, error is reduced

lm.fit13 = lm(casual~holiday+weekday+weathersit+windspeed+
                temp+
                year:temp + holiday:temp,
              data = train.data.new2)
summary(lm.fit13)
# giving good results, but counterintutive to idea of !year

# are year or temp related?
graphics.off()
boxplot(temp~year, data = train.data.new2, 
        xlab = "year", ylab = "temperature")
#no

plot(train.data.new2$year)
plot(train.data.new2$holiday)
plot(train.data.new2$weekday)
plot(train.data.new2$weathersit)
plot(train.data.new2$temp)
plot(train.data.new2$windspeed)



###transformations

lm.fit20 = lm(casual~.+log(windspeed), data = train.data.new2)
summary(lm.fit20)
#not good

lm.fit21 = lm(casual~.+sqrt(windspeed), data = train.data.new2)
summary(lm.fit21)
#not good

# high windspeed and low temp can reduce the casual
# try increasing the impact by taking square

lm.fit22 = lm(casual~.+I(windspeed^2), data = train.data.new2)
summary(lm.fit22)
#not good


#range of temp is .059 to 0.86. Never negative, safe to square
lm.fit26 = lm(casual~.+I(temp^2), data = train.data.new2)
summary(lm.fit26)
#not good





## something unique, from weekdays boxplot
## sat and sunday give lower values of casual
## try changing the code for sat and sun to 0, rest as 1
## and using log of response

train.data.new2a = train.data.new2
train.data.new2a[,3] = (train.data.new2a[,3])/6
train.data.new2a[,3] = (train.data.new2a[,3])%%1!=0
train.data.new2a[,7] = log(train.data.new2a[,7])

lm.fit23 = lm(casual~., data = train.data.new2a)
summary(lm.fit23)
# R square jumps to 0.70

par(mfrow = c(2,2))
plot(lm.fit23)
# very good results
# model looks good in diagnostic plots

# try transformations of windspeed
lm.fit24 = lm(casual~.+I(windspeed^2), data = train.data.new2a)
summary(lm.fit24)
# no improvements

# try transformations of temp
lm.fit25 = lm(casual~.+I(temp^2), data = train.data.new2a)
summary(lm.fit25)
# R square increases to 0.79
# model with log y, weekday, temp2

lm.pred1i = predict(lm.fit25, train.data.new2a)
train.rse25 = sqrt(sum((exp(lm.pred1i)-exp(train.data.new2a$casual))^2)/709) 
# lm.fit25 train rse is 319, drasctically lower than 500
# probablity of overfitting is low, not a lot of transformations

# try comparing samy predictors without taking log of response
train.data.new3a = train.data.new2
train.data.new3a[,3] = (train.data.new3a[,3])/6
train.data.new3a[,3] = (train.data.new3a[,3])%%1!=0

lm.fit30 = lm(casual~.+I(temp^2), data = train.data.new3a)
summary(lm.fit30)
# R square is .705, RSE 372.8 model with weekday, temp2
# Not as good as with log of response

#interactions with log, weekday and temp2

lm.fit40 = lm(casual~.+I(temp^2)+temp:holiday, data = train.data.new2a)
summary(lm.fit40)
#no improvements

lm.fit41 = lm(casual~.+I(temp^2)+temp:windspeed, data = train.data.new2a)
summary(lm.fit41)
# slight improvement, logged err = 0.461

lm.fit42 = lm(casual~.+I(temp^2)+temp:weathersit, data = train.data.new2a)
summary(lm.fit42)
# slightly more improvement, logged err = 0.4596


lm.pred42i = predict(lm.fit42, train.data.new2a)
train.rse42i = sqrt(sum((exp(lm.pred42i)-exp(train.data.new2a$casual))^2)/709) 
# train rse 309

lm.fit43 = lm(casual~.+I(temp^2)+weathersit:windspeed, data = train.data.new2a)
summary(lm.fit43)
# R square beomes 0.80 but it tells to removes weathersit
# logged err = .4506
lm.fit44 = lm(casual~.-weathersit+I(temp^2)+weathersit:windspeed, data = train.data.new2a)
summary(lm.fit44)
# R square is .80, logged err = 0.4503

lm.fit45 = lm(casual~.+I(temp^2)+
                weathersit:windspeed + weathersit:temp
                , data = train.data.new2a)
summary(lm.fit45)
# R square is .80, logged err = 0.4502


lm.fit46 = lm(casual~.+I(temp^2)+ temp:windspeed+
                weathersit:windspeed + weathersit:temp
              , data = train.data.new2a)
summary(lm.fit46)
# R square is .80, logged err = 0.449
# Leaves a few variables, though it performs better

## test



# the final two models choosen are lm.fit25 and lm.fit42
# both models have logged values of response, temp^2 and recoded weekday 
# But in lm.fit42 interaction term is also present
# lm.fit25 = lm(casual~.+I(temp^2), data = train.data.new2a)
# lm.fit42 = lm(casual~.+I(temp^2)+temp:weathersit, data = train.data.new2a)
# The reason being, in lm.fit25, we see a clear effect of temp^2
# The effect of interactions is not so clear
# But after considering all interactions, only lm.fit42 is giving a summary 
# in which all interactions are contributing


test.data.1 = test.data[,-c(1,3,8,9)]
test.data.1[,3] = (test.data.1[,3])/6
test.data.1[,3] = (test.data.1[,3])%%1 !=0
test.data.1[,7] = log(test.data.1[,7])



#test using lm.fit25
lm.pred1 = predict(lm.fit25, test.data.1)
lm.pred1 = exp(lm.pred1)
lm.pred1
test.rse1 = sqrt(sum((lm.pred1-exp(test.data.1$casual))^2)/18)
test.rse1
# RSE is 381  model with log y, weekday & temp2
test.mse1 = (18/20)*(test.rse1)^2
test.mse1

#test using lm.fit42
lm.pred2 = predict(lm.fit42, test.data.1)
exp(lm.pred2)
test.rse2 = sqrt(sum((exp(lm.pred2)-exp(test.data.1$casual))^2)/18)
test.rse2
# RSE is 357  model with log y, weekday & temp2, temp*weathersit
test.mse2 = (18/20)*(test.rse2)^2
test.mse2

# lm.fit42 (with interaction) performs better
# Thus corrobating the effect of interaction of temp & weathersit
