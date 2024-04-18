library(tidyverse)
library(ggplot2)


# *i By producing the data)
#Regression model 
set.seed(18)
x <- rnorm(200, mean = 0.1, sd= 0.3) # Independent Variable
y <- 2 + sin(x) + rnorm(200, mean = -0.1, sd= 0.4 ) # Dependent Variable
# its a produced data so there is no NA
# regression model
model <- lm(y ~ x) 
# summary model
summary(model) # so, x explains y by 33%. (R^2 and AdjustedR^2 close each other)
# p value is almost 0, we can say that this coefficient is statistically significant.

#### ANOVA

# Generating data for ANOVA

set.seed(18)

x1 <- rnorm(100, mean = 7, sd = 1)
x2 <- rnorm(100, mean = 5, sd = 1)
x3 <- rnorm(100, mean = 8, sd = 1)

# Combine the data into a single data frame
data <- data.frame(Value = c(x1, x2, x3),Group = factor(rep(c("x1", "x2", "x3"), each = 100))
)

# Visualize the data using boxplots
boxplot(Value ~ Group, data = data)

summary(anovaresult <- aov(Value ~ Group, data = data))
#P value is almost 0 so we can say that there is a difference between the groups.

# ANCOVA
# adding covariate
covariate <- rnorm(100, mean= 2, sd = 0.2)
data <- data.frame(Value = c(x1, x2, x3),Group = factor(rep(c("x1", "x2", "x3"), each = 100)), Covariate = covariate)

# visualizing the data - scatterplot
plot(data$Covariate, data$Value, col = data$Group, pch = 18,xlab = "Covariate", ylab = "Value")

summary(ancova_result <- lm(Value ~ Group + Covariate, data = data))
# R^2 = 0.577 The independent variables explain 57.77% of the variance on the dependent variable.
# p value is almost 0 so the model is significant



# *ii By pulling the data)

data <- read.csv("D:/weatherHistory.csv")

head(data)
options(scipen = 999) #turn off scientific number

str(data) 

set.seed(18)  # setting seed

sampledata <- data[sample(nrow(data), size = 500, replace = FALSE), ] #We are taking samples because our data set is very large.

print(sum(is.na(sampledata))) #there is no NA

cleandata <- na.omit(sampledata) #let's apply the 'NA' deletion process just in case.

newdata <- sampledata[, c("Humidity","Temperature..C.")]

head(newdata) #checked the data

colnames(newdata) <- c("Humidity", "Temperature") #We have changed the column names

# Regression analysis
reg_model <- lm(Humidity ~ Temperature, data = newdata)
summary(reg_model)  # looking at regression models summary

# In this model, the R-square and the p value are quite significant. 
#The R-squared value indicates that temperature, which is the independent variable, 
#explains about 39.29% of the variance in humidity, which is the dependent variable. 
#This indicates that the model explains a significant part of the variance in the data set.
#Also, the p value is quite low (<0.0000000000000000022), which indicates that the effect of the temperature variable on humidity is statistically significant. 
#This indicates that the relationship between temperature and humidity is not accidental, the temperature variable of the model is an important property for predicting the change in humidity.
#However, it is important to note that the model does not explain the entire change in humidity, since the R-squared value does not approach 100%.


hist1 <- ggplot(newdata, aes(x = Humidity)) + geom_histogram(fill = "darkseagreen2") +labs(title = "Histogram", x = "Humidity")

hist1

hist2 <- ggplot(newdata, aes(x = Temperature)) + geom_histogram(fill = "darkseagreen2") +labs(title = "Histogram", x = "Humidity")

hist2

########################

# ANOVA
anovaresult <- aov(Temperature..C. ~ Summary, data = sampledata)

#looking anova result
summary(anovaresult)

# The p-value is very close to 0, 
# indicating that the effect of different weather categories on temperature is statistically significant."


ggplot(sampledata, aes(x = Summary, y = Temperature..C.)) +
  geom_boxplot() +
  labs(x = "Summary", y = "Temperature (C)",
       title = "Temperature by Summary") +       #I think I need a very large screen to interpret it..
  theme_minimal()

############################

ancovamodel <- lm(Apparent.Temperature..C. ~ Summary , data=data)

# ANCOVA modelini analiz edelim
summary(ancovamodel)

#The R-squared value of 0.1869 indicates that the model explains a low percentage of the variance. However, considering the p-value, which is very close to 0, it indicates that the model is significant.
