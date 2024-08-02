housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
  view <- factor(view)
  backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

# Line that prints the variables
print("Variables")
sapply(housing, class)

# 3. Model #1 - First Order Regression Model with Quantitative and Qualitative Variables
# Correlation Analysis

# Line that creates the scatterplot of price and sqft_living
plot(housing$sqft_living, housing$price,
     main = "Scatterplot of Home Price and Living Area in Sq. Ft.",
     xlab = "Living Area in Sq. Ft.", ylab = "Home Price",
     col="purple", 
     pch = 19)

# Line that creates the scatterplot of price and gae of the home
plot(housing$age, housing$price,
     main = "Scatterplot of Home Price and Age of the Home",
     xlab = "Age of the Home", ylab = "Home Price",
     col="purple", 
     pch = 19)

# Create report of te relation coefficients between price + sqft_living
# price + age
modelone <- c("price","sqft_living", "age")
housing_subset <- housing[modelone]

# Print the Correlation Matrix
print("Correlation Matrix")
corr_matrix <- cor(housing_subset, method = "pearson")
round(corr_matrix, 6)

# Reporting Results
# Subsetting data to include variables price, sqft_living, sqft_above, 
# age, bathrooms, and view
modelonemultreg <- c("price", "sqft_living", "sqft_above", "age", "bathrooms", "view")
housing_subset <- housing[modelonemultreg]

# Create the model 1
mod1multreg <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data=housing_subset)
summary(mod1multreg)

# Fitted values
fitted_values <- fitted(mod1multreg) 
fitted_values

# Residuals
residuals <- residuals(mod1multreg)
residuals

# Residuals against fitted values
plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col= "blue", 
     pch = 19)

# Normal Q-Q plot
qqnorm(residuals, pch = 19, col= "blue")
qqline(residuals, col = "red", lwd = 2)

# Make predictions using the model
# Prediction for price of a home with sqft_living = 2150, sqft_above = 1050,
# age= 15, bathrooms = 3 and view = 0
modelonepred1 <- data.frame(sqft_living = 2150, sqft_above = 1050, age= 15, bathrooms = 3, view = '0')

#print("Prediction Interval for a home with sqft_living = 2150, sqft_above = 1050, age= 15, bathrooms = 3 and view = 0")
print("90% Prediction Interval for a home with sqft_living = 2150, sqft_above = 1050, age= 15, bathrooms = 3 and view = 0")
prediction_pred_int <- predict(mod1multreg, modelonepred1, interval= "predict", level = 0.90) 
round(prediction_pred_int, 4)

#print("Confidence Interval for a home with sqft_living = 2150, sqft_above = 1050, age= 15, bathrooms = 3 and view = 0")
print("90% Confidence Interval for a home with sqft_living = 2150, sqft_above = 1050, age= 15, bathrooms = 3 and view = 0")
prediction_conf_int <- predict(mod1multreg, modelonepred1, interval= "confidence", level = 0.90) 
round(prediction_conf_int, 4)

# Make predictions using the model
# Prediction for price of a home with sqft_living = 4250, sqft_above = 2100, 
# age= 5, bathrooms = 5, view = '2'
modelonepred2 <- data.frame(sqft_living = 4250, sqft_above = 2100, age= 5, bathrooms = 5, view = '2')

#print("Prediction Interval for a home with sqft_living = 4250, sqft_above = 2100, age= 5, bathrooms = 5, view = 2")
print("90% Prediction Interval for a home with sqft_living = 4250, sqft_above = 2100, age= 5, bathrooms = 5, view = '2'")
prediction_pred_int <- predict(mod1multreg, modelonepred2, interval= "predict", level = 0.90) 
round(prediction_pred_int, 4)

#print("Confidence Interval for a home with sqft_living = 4250, sqft_above = 2100, age= 5, bathrooms = 5, view = 2")
print("90% Confidence Interval for a home with sqft_living = 4250, sqft_above = 2100, age= 5, bathrooms = 5, view = 2")
prediction_conf_int <- predict(mod1multreg, modelonepred2, interval= "confidence", level = 0.90) 
round(prediction_conf_int, 4)

# 4. Model #2 - Complete Second Order Regression Model with Quantitative Variables
# Correlation Analysis

# Line that creates the scatterplot of price and school_rating
plot(housing$school_rating, housing$price,
     main = "Scatterplot of Home Price and School Rating",
     xlab = "School Rating", ylab = "Home Price",
     col="blue", 
     pch = 19)

# Line that creates the scatterplot of price and crime
plot(housing$crime, housing$price,
     main = "Scatterplot of Home Price and Crime",
     xlab = "Crime", ylab = "Home Price",
     col="blue", 
     pch = 19)

# Reporting Results
# Subsetting data to include variables price, school_rating and crime 
print("Complete Second Order Model for price using school_rating and crime predictors")
modeltwomultreg <- c("price", "school_rating", "crime")
housing_subset <- housing[modeltwomultreg]

# Create the model 2
mod2multreg <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2), data=housing_subset)
summary(mod2multreg)

# Fitted values
fitted_values <- fitted(mod2multreg) 
fitted_values

# Residuals
residuals <- residuals(mod2multreg)
residuals

# Residuals against fitted values
plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col= "blue", 
     pch = 19)

# Normal Q-Q plot
qqnorm(residuals, pch = 19, col= "blue")
qqline(residuals, col = "red", lwd = 2)

# Make predictions using the model
# Prediction for price of a home with school_rating = 9.80 and crime = 81.02
modeltwopred1 <- data.frame(school_rating = 9.80, crime = 81.02)

#print("Prediction Interval for price of a home with school_rating = 9.80 and crime = 81.02")
print("90% Prediction Interval for a price of a home with school_rating = 9.80 and crime = 81.02")
prediction_pred_int <- predict(mod2multreg, modeltwopred1, interval= "predict", level = 0.90) 
round(prediction_pred_int, 4)

#print("Confidence Interval for a price of a home with school_rating = 9.80 and crime = 81.02")
print("90% Confidence Interval for a price of a home with school_rating = 9.80 and crime = 81.02")
prediction_conf_int <- predict(mod2multreg, modeltwopred1, interval= "confidence", level = 0.90) 
round(prediction_conf_int, 4)

# Make predictions using the model
# Prediction for price of a home with school_rating = 4.28 and crime = 215.50
modeltwopred2 <- data.frame(school_rating = 4.28, crime = 215.50)

#print("Prediction Interval for price of a home with school_rating = 4.28 and crime = 215.50")
print("90% Prediction Interval for a price of a home with school_rating = 4.28 and crime = 215.50")
prediction_pred_int <- predict(mod2multreg, modeltwopred2, interval= "predict", level = 0.90) 
round(prediction_pred_int, 4)

#print("Confidence Interval for a price of a home with school_rating = 4.28 and crime = 215.50")
print("90% Confidence Interval for a price of a home with school_rating = 4.28 and crime = 215.50")
prediction_conf_int <- predict(mod2multreg, modeltwopred2, interval= "confidence", level = 0.90) 
round(prediction_conf_int, 4)

# 5. Nested Models F-Test5. Nested Models F-Test
# Subsetting data to include variables price, school_rating and crime
print("Nested Models F-Test for first order regression model on price using school_rating and crime ")
modftest <- c("price", "school_rating", "crime")
housing_subset <- housing[modftest]

# Line that shows the first regression model forprice using school_rating and crime
firstreg <- lm(price ~ school_rating + crime + school_rating:crime, data=housing)
summary(firstreg)

# Line that shows the Complete Model
fit_complete <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2), data=housing_subset)

# Line that shows the Reduced Model
fit_reduced <- lm(price ~ school_rating + crime + school_rating:crime, data=housing_subset)

#  Line that shows the F-Test
anova(fit_complete, fit_reduced)