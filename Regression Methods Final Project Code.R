## Jeffrey Dean

## Data Source: https://fred.stlouisfed.org/

## Final Project Regression Methods 

## Code for Regression Project, Write Up is included.

## This project is to support if a liquidity trap exists based on factors correlation relative to 
## interest rates.  A few items there are related but seperate to support a conclusion is an economy's
## measurement of consumer spending, inflation, and capital investment spending relative to a change in
## interest rates.

## The data contained here will be discussed further in writing to support conclusions found.
## The linear models are run with variables regressed against interest rates to support how interest 
## rates impact certain factors.  Plots with trend lines and and residual plots will be done.  
## Multicollinearty will be examined as factors are relatively related and models of interaction terms
## will be conducted to see if terms have an abnormally high correlation. Given inputs are standard and 
## measureable, we support the inputs are parametric and least squares regression is the ideal way of reducing
## variance with no bias added in the models.  

## Linear Models

date_info <- Regression.Methods.Final.Project.Data$Date1
us_interest <- Regression.Methods.Final.Project.Data$Interest_Rate_US
euro_interest <- Regression.Methods.Final.Project.Data$Interest_Rate_Euro
us_inflation <- Regression.Methods.Final.Project.Data$Inflation_US
euro_inflation <- Regression.Methods.Final.Project.Data$Inflation_Euro
consumer_spend_us <- Regression.Methods.Final.Project.Data$Annual_Consumption_US
consumer_spend_euro <- Regression.Methods.Final.Project.Data$Annual_Consumption_Euro
capital_spend_us <- Regression.Methods.Final.Project.Data$Total_Cap_Ex_US
capital_spend_euro <- Regression.Methods.Final.Project.Data$Total_Cap_Ex_Euro
  
## 1 Factor Models

interest_inflation_us_model <- lm(us_inflation ~ us_interest)
summary(interest_inflation_us_model)

interest_inflation_euro_model <- lm(euro_inflation ~ euro_interest)
summary(interest_inflation_euro_model)

interest_consumer_us_model <- lm(consumer_spend_us ~ us_interest)
summary(interest_consumer_us_model)

interest_consumer_euro_model <- lm(consumer_spend_euro ~ euro_interest)
summary(interest_consumer_euro_model)

interest_capital_us_model <- lm(capital_spend_us ~ us_interest)
summary(interest_capital_us_model)

interest_capital_euro_model <- lm(capital_spend_euro ~ euro_interest)
summary(interest_capital_euro_model)

## Full Models

full_us_model <- lm(us_interest ~ us_inflation + consumer_spend_us + capital_spend_us)
summary(full_us_model)

full_euro_model <- lm(euro_interest ~ euro_inflation + consumer_spend_euro + capital_spend_euro)
summary(full_euro_model) 

## Linear Models with Interaction 

full_us_model_interaction <- lm(us_interest ~ us_inflation + consumer_spend_us + capital_spend_us + consumer_spend_us*capital_spend_us)
summary(full_us_model_interaction)

full_euro_model_interaction <- lm(euro_interest ~ euro_inflation + consumer_spend_euro + capital_spend_euro + euro_inflation*capital_spend_euro)
summary(full_euro_model_interaction)

## Regression Plots

## US Plots

plot(date_info, us_interest, main = "US Interest Rates",xlab = "Dates", ylab = "Interest Rates")
plot(date_info, us_inflation, main = "US Inflation Rates",xlab = "Dates", ylab = "Inflation Rates")
plot(date_info, consumer_spend_us, main = "US Consumer Spending",xlab = "Dates", ylab = "Consumer Spending")
plot(date_info, capital_spend_us, main = "US Capital Spending",xlab = "Dates", ylab = "Capital Spending")

plot(us_inflation, us_interest, main = "US Interest Rates vs US Inflation")
plot(consumer_spend_us, us_interest, main = "US Interest Rates vs US Consumer Spending")
plot(capital_spend_us, us_interest, main = "US Interest Rates vs US Capital Spending")

## Euro Plots

plot(date_info, euro_interest, main = "Euro Interest Rates",xlab = "Dates", ylab = "Interest Rates")
plot(date_info, euro_inflation, main = "Euro Inflation Rates",xlab = "Dates", ylab = "Inflation Rates")
plot(date_info, consumer_spend_euro, main = "Euro Consumer Spending",xlab = "Dates", ylab = "Consumer Spending")
plot(date_info, capital_spend_euro, main = "Euro Capital Spending",xlab = "Dates", ylab = "Capital Spending")

plot(euro_inflation, euro_interest, main = "Euro Interest Rates vs Euro Inflation")
plot(consumer_spend_euro, euro_interest, main = "Euro Interest Rates vs Euro Consumer Spending")
plot(capital_spend_euro, euro_interest, main = "Euro Interest Rates vs Euro Capital Spending")

## Residual Plots

resid(full_us_model)
plot(resid(full_us_model), main = "US Model Residuals")

resid(full_euro_model)
plot(resid(full_euro_model), main = "Euro Model Residuals")

## Follow Up, New Input- Savings Rate

## Seeing there is multicollinearity among different variables at work here, I tried to replace 
## consumer spending as a factor and substitute personal savings rate (across an economy) and regress it against 
## interest rates and the other factors to get a better picture of what is occuring.  

us_savings_rate <- Regression.Methods.Final.Project.Data$Savings_Rate_US
euro_savings_rate <- Regression.Methods.Final.Project.Data$Savings_Rate_Euro_Area

interest_savings_us_model <- lm(us_savings_rate ~ us_interest)
summary(interest_savings_us_model)

interest_savings_euro_model <- lm(euro_savings_rate ~ euro_interest)
summary(interest_savings_euro_model)

plot(us_savings_rate, us_interest, main = "US Savings Rate vs Interest Rates")
plot(euro_savings_rate, euro_interest, main = "Euro Savings Rate vs Interest Rates")

new_full_us_model <- lm(us_interest ~ us_inflation + us_savings_rate + capital_spend_us)
summary(new_full_us_model)

new_full_euro_model <- lm(euro_interest ~ euro_inflation + euro_savings_rate + capital_spend_euro)
summary(new_full_euro_model)
