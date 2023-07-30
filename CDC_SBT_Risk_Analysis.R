#CRAWFORD DEVELOPMENT CO. AND SOUTHEAST BANK OF TEXAS Case Study
# R Script by Madison Costanza

# Import relevant R libraries:
library(ggplot2)
library(magrittr)
library(tidyverse)
library(gridExtra)
library(fitdistrplus)
library(FinCal)

#Loading commercial project data 
expected <- c(66382.35, 60852.39, 65872.30, 64541.97, 73871.30, 52689.86, 63470.57, 55828.80, 63972.39, 61999.35, 78151.43, 75928.84, 73195.79, 
              70009.95, 58471.00, 66242.29, 79315.70, 60000.95, 60839.91, 65844.29, 70511.40, 49527.53, 67793.12, 72333.48, 55992.65, 75956.10, 
              61969.04, 55875.70, 74283.09, 85382.88, 69717.37, 56725.72)
indicators <- c(-1, 0, 0, 1, 1, -1, -1, 1, 0, 1, 1, 1, -1, -1, 0, 1, 0, -1, 1, -1, -1, -1, 0, -1, 1, -1, 0, 0, 1, 0, 1, -1)
actual <- c(51302.37, 63452.49, 67932.41, 79689.09, 70408.31, 46006.15, 61575.80, 65413.68, 57756.02, 72319.87, 84962.93, 82105.66,64361.14, 
            64554.01, 56729.86, 76268.45, 84896.93, 43384.19, 68533.78, 51601.49, 62379.33, 36775.17, 70128.14, 61806.21, 68218.22, 63401.88, 
            60720.96, 65842.55, 78509.94, 83909.33, 87853.35, 38732.77)

officesale <- data.frame(expected, indicators, actual)

## EDA
officesale %>% group_by(indicators) %>% summarise(count = sum(actual > 0)) 
officesale_1 <- filter(officesale, indicators == 1)
officesale_2 <- filter(officesale, indicators == 0)
officesale_3 <- filter(officesale, indicators == -1)


#view trend of expected sales
hist(expected) #seems like normal distribution

#try to fit with different distributions
expec_model1<- fitdist(officesale$expected, "norm")
expec_model2<- fitdist(officesale$expected,"weibull")
expec_model3<- fitdist(officesale$expected,"lnorm")
gofstat(list(expec_model1,expec_model2,expec_model3))
#Lognormal distribution has the lowest KS value and lowest AD value. Therefore, the distribution of expected value is close to Lognormal distribution.

plot(expec_model3)

expec_model3$estimate #meanlog: 11.0902055, sdlog: 0.1263053

#view trend of actual sales
hist(actual)

#check the relationship between actual sales and expected sales
cor(officesale$expected, officesale$actual) # corrleation = 0.67
cor(officesale_1$expected, officesale_1$actual) # correlation = 0.72
cor(officesale_2$expected, officesale_2$actual) # correlation = 0.89
cor(officesale_3$expected, officesale_3$actual) # correlation = 0.89

ggplot(data = officesale) + 
  geom_point(mapping = aes(x = expected, y = actual)) + 
  geom_smooth(mapping = aes(x = expected, y = actual)) + 
  xlab("Expected sales") + ylab("Actual sales") #close to linear

# So we should build a linear reg model with 2 predictors: actual sales ~ indicators + expected sales

office.lm <- lm(actual ~ expected + factor(indicators))
summary(office.lm) 

#try to identify leverage points through hat statistics
hat.plot <- function(x){
  p <- length(coefficients(x))
  n <- length(fitted(x))
  plot(hatvalues(x), main = "Index Plot of Hat Values")
  abline(h = c(2, 3) * p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(x), names(hatvalues(x)))
}

hat.plot(office.lm) #only one point is above 0.25

hatvalues(office.lm)[which(hatvalues(office.lm) > 0.25)] #point of 30 is leverage point

#try to identify the influence points
cutoff <- 4/(nrow(actual) - length(office.lm$coefficients) - 2)
plot(office.lm, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red") #points of 5, 28, and 31 are influence points

#refit the model after removing the leverage points and influence points
office.lm2 <- lm(actual ~ expected + factor(indicators), 
                 data = officesale[-c(5, 28, 31), ])
summary(office.lm2) 


#Information from case:  P(indicators=-1) = 12/32 , P(indicators=0) = 9/32 , P(indicators=1) = 11/32
#simulate actual sales

# Adjusted R-squared is increased to 0.9001. p-value is still far smaller than 0.05.
#The refitted model performs better.
n <- 1000
# Specify the log-normal model:
rlnorm2 <- function(n, mean, sd){
  rlnorm(n, log(mean*(1+sd^2/mean^2)^-0.5), log(1+sd^2/mean^2)^0.5)
}

# Simulate commerical sales, assuming a normal distribution:
expected_sim <- rlnorm2(n, mean(expected), sd(expected))

prob <- runif(n, 0, 1)
indicators_sim <- ifelse(prob < 12/32, -1, ifelse(prob > 21/32, 1, 0))

#  Create a data frame of expected sales figures along with their relevant indicators:
df_sim <- data.frame(expected = expected_sim, indicators = indicators_sim)

# Add a column to this new dataframe with predicted future sales figures using the revised model:
df_sim$Predcted_Actual <- predict(office.lm2, df_sim)

# Import library to help with x-axis notation:
library(scales)

# Graph predicted commerical project sales with a histogram:
ggplot(data = df_sim) + 
  geom_histogram(aes(x = Predcted_Actual, y = ..count../n), bins = 20) + 
  xlab("Predicted actual sales (in thousands)") + 
  ylab("Probability") + 
  labs(title = "Predicted Actual Sales for Office Park")+ 
  scale_x_continuous(labels = scales::comma)

#95% Summary Statistics for Commercial Project Sales:
mean(df_sim$Predcted_Actual)
mean(df_sim$Predcted_Actual)-qnorm(0.95)*sd(df_sim$Predcted_Actual)/(n^0.5)
mean(df_sim$Predcted_Actual)+qnorm(0.95)*sd(df_sim$Predcted_Actual)/(n^0.5)

# Min and Max of Commercial Project Sales:
min(df_sim$Predcted_Actual)
max(df_sim$Predcted_Actual)

# calculate cumulative FCF:
df_sim$cFCF <- 37875 - 24375 - 9000 - 12500 + 
  (df_sim$Predcted_Actual - 8636.03 - 38375.00)

# Graph histogram of cumulative FCF:
ggplot(data = df_sim) + 
  geom_histogram(aes(x = cFCF, y = ..count../n)) + 
  xlab("Cumulative FCF (in thousands)") + 
  ylab("Probability") + 
  labs(title = "Cumulative Free Cashflows for Office Park Investment")

# Summary Statistics of Cumulative FCF:
mean(df_sim$cFCF)
mean(df_sim$cFCF)-qnorm(0.95)*sd(df_sim$cFCF)/sqrt(n)
mean(df_sim$cFCF)+qnorm(0.95)*sd(df_sim$cFCF)/sqrt(n)
min(df_sim$cFCF)
max(df_sim$cFCF)

# calculate discounted FCF:
DiscFactor <- (.06)
df_sim$discounted <- 37875 - 24375/(1+DiscFactor/4) - 9000/1.06 - 12500/(1.06^2) + (df_sim$Predcted_Actual - 8636.03 - 38375.00)/(1.06^3)

# Graphy simulated discounted FCF:
ggplot(data = df_sim) + 
  geom_histogram(aes(x = discounted, y = ..count../n)) + 
  xlab("Net Present Value (in thousands)") + 
  ylab("Probability") + 
  labs(title = "Discounted Free Cashflows for Office Park Project (NPV)")

# Summary statistics of discounted FCF:
mean(df_sim$discounted)
mean(df_sim$discounted)-qnorm(0.95)*sd(df_sim$discounted)/sqrt(n)
mean(df_sim$discounted)+qnorm(0.95)*sd(df_sim$discounted)/sqrt(n)
max(df_sim$discounted)
min(df_sim$discounted)


#Residential Sales Portion of Project

# Define sales scenarios:
max_sr_res <- 130000  # Best-case scenario:  $130M in sales
min_sr_res <- 20000   # Worst-case scenario:  $20 M in sales
ml_sr_res  <- 42300   # Break-even (most expected) scenario:  $42.3 M in sales

#Define our triangular distribution model:
rtri<-function(n,min,ml,max){
  qtri<-function(U){
    F<-(ml-min)/(max-min)
    if(U<F) {min+(U*(max-min)*(ml-min))^.5}
    else{max-((1-U)*(max-min)*(max-ml))^.5}
  }
  y<-runif(n)
  sapply(y,qtri)
}

# Create new variable that is a result of simulating residential sales within our given parameters:
u_sale_r <- rtri(n,min_sr_res, ml_sr_res, max_sr_res)

# Add this new variable to a new dataframe named "Res_Sales":
res_sales <-data.frame(u_sale_r)

#95% summary statistics for residential sales:
mean(res_sales$u_sale_r)
mean(res_sales$u_sale_r)-qnorm(0.95)*sd(res_sales$u_sale_r)/sqrt(n)
mean(res_sales$u_sale_r)+qnorm(0.95)*sd(res_sales$u_sale_r)/sqrt(n)

#Confirm expected min and max of residential sales:
min(res_sales$u_sale_r)
max(res_sales$u_sale_r)

#Histogram of residential sales:
ggplot(data = res_sales) + 
  geom_histogram(aes(x = u_sale_r, y = ..count../n)) + 
  xlab("Residential Sales (in thousands)") + 
  ylab("Probability") + 
  labs(title = "Expected Residential Project Sales") +
  scale_x_continuous(labels = scales::comma)

# Simulate construction costs as normally distributed with a mean of $20 M and SD of $1 M. 
res_sales$construction_costs <- rnorm(n=1000,mean=20000,sd=1000)

# Plot expected construction costs as a histrogram:
ggplot(data = res_sales) + 
  geom_histogram(aes(x = construction_costs, y = ..count../n)) + 
  xlab("Construction Costs (in thousands)") + 
  ylab("Probability") + 
  labs(title = "Expected Construction Costs for Residential Project")

# calculate cumulative FCF assuming an interest rate of 7% and 8.636 M in interest payments
res_sales$cFCF <- 37875 - 4375 - res_sales$construction_costs - 9000 - 12500 + (res_sales$u_sale_r - 8636.03 - 38375.00)

# Plot Cumulative Free Cashflows as a histogram:
ggplot(data = res_sales) + 
  geom_histogram(aes(x = cFCF, y = ..count../n)) + 
  xlab("Cumulative FCF (in thousands)") + 
  ylab("Probability") + 
  labs(title = "Cumulative FCF for Residential Project")

# 95% Summary Statistics for cumulative FCF:
mean(res_sales$cFCF)
mean(res_sales$cFCF)-qnorm(0.95)*sd(res_sales$cFCF)/sqrt(n)
mean(res_sales$cFCF)+qnorm(0.95)*sd(res_sales$cFCF)/sqrt(n)

# Introduce discounting to the free cashflow model:
res_sales$discounted <- 37875 - 4375/(1+DiscFactor/4) - 
  res_sales$construction_costs/(1+DiscFactor/4) - 9000/1.06 - 12500/(1.06^2) + 
  (res_sales$u_sale_r - 8636.03 - 38375.00)/(1.06^3)

#95% summary statistics:
mean(res_sales$discounted)
mean(res_sales$discounted)-qnorm(0.95)*sd(res_sales$discounted)/sqrt(n)
mean(res_sales$discounted)+qnorm(0.95)*sd(res_sales$discounted)/sqrt(n)
max(res_sales$discounted)
min(res_sales$discounted)

# Graph histogram of NPV of residential project:
ggplot(data = res_sales) + 
  geom_histogram(aes(x = discounted, y = ..count../n)) + 
  xlab("Net Present Value (in thousands)") + 
  ylab("Probability") + 
  labs(title = "Discounted Free Cashflows for Residential Project (NPV)")

#Probability NPV will be negative in each  project:
sum(df_sim$discounted<0)/n  #18.3%
sum(res_sales$discounted<0)/n  #39%


# We should create another variable that calculates NPV, but not including interest payments,
# as interest payments will be calculated in our for-loop.
res_sales$discounted_no_int <- 37875 - 4375/(1+DiscFactor/4) - 
  res_sales$construction_costs/(1+DiscFactor/4) - 9000/1.06 - 12500/(1.06^2) + 
  (res_sales$u_sale_r - 38375.00)/(1.06^3)

#calculate interest through 7% to 11% 
for(IR in c(0.07, 0.08, 0.09, 0.10, 0.11)){
  IT <- fv(IR,3,-38375,0) - 38375
  total_pmt <- 38735 + IT
  CDC_NPV <- mean(res_sales$discounted_no_int) - IT/(1.06)^3
  SBT_NPV <- -38735 + total_pmt/1.06^3
  cat("When interest rate is", IR,", interest will be $", IT, 
      "total payment to the Bank will be", total_pmt,", 
      and expected NPV of the residential project is $",CDC_NPV,". 
      NPV from POV of SBT will be",SBT_NPV,".")
}

