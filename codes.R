#STAT 467 PROJECT LATEST

library(timeSeriesDataSets)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(pdR)
library(TSA)
library(lmtest)
library(FinTS)
library(readxl)
library(bestNormalize)
library(MVN)
library(ICSNP)
library(corrplot)
library(lmtest)
library(car)


data <- read_excel("C:/Users/programlama/OneDrive/Desktop/stat497/stat467dataset.xlsx")

bos1 <- sum(data$normalized_losses == "?")
bos1
bos2 <- sum(data$num_of_doors == "?")
bos2
bos3 <- sum(data$bore == "?")
bos3
bos4 <- sum(data$stroke == "?")
bos4
bos5 <- sum(data$horsepower == "?")
bos5
bos6 <- sum(data$peak_rpm == "?")
bos6
bos7 <- sum(data$price== "?")
bos7

data$normalized_losses[data$normalized_losses == "?"] <- NA
data$normalized_losses <- as.numeric(data$normalized_losses)
mean1 <- mean(data$normalized_losses, na.rm = TRUE)
mean1
data$normalized_losses[is.na(data$normalized_losses)] <- mean1


calculate_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

data$num_of_doors[data$num_of_doors == "?"] <- NA
mode_num_of_doors <- calculate_mode(data$num_of_doors)
data$num_of_doors[is.na(data$num_of_doors)] <- mode_num_of_doors

data$bore[data$bore == "?"] <- NA
data$bore <- as.numeric(data$bore)
mean_bore <- mean(data$bore, na.rm = TRUE)
data$bore[is.na(data$bore)] <- mean_bore

data$stroke[data$stroke == "?"] <- NA
data$stroke <- as.numeric(data$stroke)
mean_stroke <- mean(data$stroke, na.rm = TRUE)
data$stroke[is.na(data$stroke)] <- mean_stroke

data$horsepower[data$horsepower == "?"] <- NA
data$horsepower <- as.numeric(data$horsepower)
mean_horsepower <- mean(data$horsepower, na.rm = TRUE)
data$horsepower[is.na(data$horsepower)] <- mean_horsepower

data$peak_rpm[data$peak_rpm == "?"] <- NA
data$peak_rpm <- as.numeric(data$peak_rpm)
mean_peak_rpm <- mean(data$peak_rpm, na.rm = TRUE)
data$peak_rpm[is.na(data$peak_rpm)] <- mean_peak_rpm

data$price[data$price == "?"] <- NA
data$price <- as.numeric(data$price)
mean_price <- mean(data$price, na.rm = TRUE)
data$price[is.na(data$price)] <- mean_price

colnames(data)
categorical_columns <- c(
  "make", "fuel_type", "aspiration", "num_of_doors",
  "body_style", "drive_wheels", "engine_location",
  "engine_type", "num_of_cylinders", "fuel_system"
)

for (col in categorical_columns) {
  mapping <- setNames(0:(length(unique(data[[col]])) - 1), unique(data[[col]]))
    new_col_name <- paste0(col, "_dummy")
  data[[new_col_name]] <- as.numeric(mapping[data[[col]]])
    print(paste("Mapping for", col, ":"))
  print(mapping)
}
summary(data)
data1 <- data[, !(names(data) %in% categorical_columns)]
summary(data1)

#Normalizing


#Bore
best_trans_bore <- bestNormalize(data1$bore)
data1$bore_transformed <- predict(best_trans_bore, data1$bore)
summary(data1)
library(ggplot2)
ggplot(data1, aes(x = bore)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for bore_amnt", x = "bore_amnt")


ggplot(data1, aes(x = bore_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for loan_amnt", x = "loan_amnt")
qqnorm(data1$price_transformed); qqline(data1$price_transformed, col = "red")

#Stroke
best_trans_stroke <- bestNormalize(data1$stroke)
data1$stroke_transformed <- predict(best_trans_stroke, data1$stroke)
summary(data1)
library(ggplot2)
ggplot(data1, aes(x = price)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for loan_amnt", x = "loan_amnt")

ggplot(data1, aes(x = price_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for loan_amnt", x = "loan_amnt")
qqnorm(data1$price_transformed); qqline(data1$stroke_transformed, col = "red")
mvn(data1[,c("stroke_transformed")], mvnTest = "royston")

#compression ratio
best_trans_compressionrt <- bestNormalize(data1$compression_ratio)
data1$compression_transformed <- predict(best_trans_compressionrt, data1$compression_ratio)
summary(data1)
library(ggplot2)
ggplot(data1, aes(x = `compression-ratio`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for Compression Ratio", x = "Compression Ratio")
ggplot(data1, aes(x = compression_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for Compression Ratio", x = "Compression Ratio")
qqnorm(data1$compression_transformed); qqline(data1$compression_transformed, col = "red")
mvn(data1[,c("compression_transformed")], mvnTest = "royston")

#horsepower
best_trans_horsepower <- bestNormalize(data1$horsepower)
data1$horsepower_transformed <- predict(best_trans_horsepower, data1$horsepower)
summary(data1)
library(ggplot2)
ggplot(data1, aes(x = horsepower)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for Compression Ratio", x = "Compression Ratio")

ggplot(data1, aes(x = horsepower_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for Compression Ratio", x = "Compression Ratio")
qqnorm(data1$horsepower_transformed); qqline(data1$horsepower_transformed, col = "red")

#peak-rpm
best_trans_peakrpm <- bestNormalize(data1$peak_rpm)
data1$peakrpm_transformed <- predict(best_trans_peakrpm, data1$peak_rpm)
summary(data1)
library(ggplot2)
ggplot(data1, aes(x = `peak-rpm`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for Peak RPM", x = "Peak RPM")
ggplot(data1, aes(x = peakrpm_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for Peak RPM", x = "Peak RPM")
qqnorm(data1$peakrpm_transformed); qqline(peakrpm_transformed, col = "red")

#city-mpg
best_trans_citympg <- bestNormalize(data1$city_mpg)
data1$citympg_transformed <- predict(best_trans_citympg, data1$city_mpg)
summary(data1)
ggplot(data1, aes(x = `city-mpg`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for City MPG", x = "City MPG")

ggplot(data1, aes(x = citympg_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for City MPG", x = "City MPG")
grid.newpage()
qqnorm(data1$citympg_transformed); qqline(citympg_transformed, col = "red")

#highway-mpg
best_trans_highwaympg <- bestNormalize(data1$highway_mpg)
data1$highwaympg_transformed <- predict(best_trans_highwaympg, data1$highway_mpg)
summary(data1)
ggplot(data1, aes(x = `highway-mpg`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for Highway MPG", x = "Highway MPG")

ggplot(data1, aes(x = highwaympg_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for Highway MPG", x = "Highway MPG")
qqnorm(data1$normal_highway_mpg); qqline(normal_highway_mpg, col = "red")

#-----------------------------------------------------------------------------

#Checking unnormalized ones individually

mvn(data1[,c("bore_transformed")], mvnTest="royston")

#Compression Ratio
best_trans_compression <- bestNormalize(data1$compression_ratio)
data1$compression_transformed <- predict(best_trans_compression, data1$compression_ratio)
summary(data1)
ggplot(data1, aes(x = compression_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for `compression-ratio`", x = "compression-ratio")

mvn(data1[,c("compression_transformed")], mvnTest="royston")

#Horsepower
best_trans_horsepower <- bestNormalize(data1$horsepower)
data1$horsepower_transformed <- predict(best_trans_horsepower, data1$horsepower)
summary(data1)
ggplot(data1, aes(x = horsepower)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for horsepower", x = "horsepower")

ggplot(data1, aes(x = horsepower_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for horsepower", x = "horsepower")
qqnorm(data1$horsepower_transformed); qqline(horsepower_transformed, col = "red")

mvn(data1[,c("horsepower_transformed")], mvnTest="royston")

#peak-rpm (normal de??il)
best_trans_peak <- bestNormalize(data1$peak_rpm)
data1$peak_transformed <- predict(best_trans_peak, data1$peak_rpm)
summary(data1)
ggplot(data1, aes(x = `peak-rpm`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for peak", x = "peak")

ggplot(data1, aes(x = peak_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for peak", x = "peak")
qqnorm(data1$peak_transformed); qqline(peak_transformed, col = "red")

mvn(data1[,c("peak_transformed")], mvnTest="royston")

#city-mpg
best_trans_city <- bestNormalize(data1$city_mpg)
data1$city_transformed <- predict(best_trans_city, data1$city_mpg)
summary(data1)
ggplot(data1, aes(x = `city-mpg`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for city mpg", x = "city mpg")

ggplot(data1, aes(x = city_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for peak", x = "peak")
qqnorm(data1$city_transformed); qqline(city_transformed, col = "red")
mvn(data1[,c("city_transformed")], mvnTest="royston")

#highway-mpg
best_trans_highway <- bestNormalize(data1$highway_mpg)
data1$highway_transformed <- predict(best_trans_highway, data1$highway_mpg)
summary(data1)
ggplot(data1, aes(x = `highway-mpg`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for city mpg", x = "city mpg")
ggplot(data1, aes(x = highway_transformed)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot for peak", x = "peak")
qqnorm(data1$highway_transformed); qqline(highway_transformed, col = "red")

mvn(data1[,c("highway_transformed")], mvnTest="royston")
mvn(log(data1), mvnTest = "royston")$multivariateNormality

bestNormalize(data1[,-c("fuel_type_dummy","aspiration_dummy","num_of_doors_dummy","body_style_dummy","drive_wheels_dummy",
                        "engine_location_dummy","engine_type_dummy","num_of_cylinders_dummy","fuel_system_dummy")])

colnames(data1)
str(data1)

#-------------------------------------------------------------------
#manova one-way


library(bestNormalize)
price_boxcox <- bestNormalize(data1$price, method = "BoxCox")
data1$price_boxcox_transformed <- predict(price_boxcox, data1$price)
mvn(data1[,c("price_boxcox_transformed")], mvnTest="royston")


subset_data2 <- data1[, c("body_style_dummy","price_boxcox_transformed", "horsepower_transformed")]


library(rstatix)
subset_data2 %>% group_by(body_style_dummy) %>%  shapiro_test(price_boxcox_transformed, horsepower_transformed)

library(heplots)
boxM(Y = cbind(subset_data2$price_boxcox_transformed,subset_data2$horsepower_transformed), group = factor(subset_data2$body_style_dummy))


#Since the p-value is not significant (p > 0.05), it is reasonable to consider this sample as approximately normal. For Box???s M test, we fail to reject the null hypothesis, indicating that the variance-covariance matrices are equal across the groups for each combination of the dependent variable formed by the independent variable.


#After that we will conduct the hypothesis.
m1 <- manova(cbind(price_boxcox_transformed,horsepower_transformed) ~ body_style_dummy, data = subset_data2)
summary(m1)

summary.aov(m1)

library(ICSNP)

library(dplyr)

subset_data <- data1[, c("price_boxcox_transformed", "horsepower_transformed", "fuel_type_dummy")]


#------------------ making horsepower normal steps
library(bestNormalize)


# Subset data for fuel_type_dummy == 1
fuel_1_horsepower <- subset_data$horsepower_transformed[subset_data$fuel_type_dummy == 1]


# Apply OrderNorm transformation
order_norm_trans <- orderNorm(fuel_1_horsepower)


# Transform the data
transformed_values <- predict(order_norm_trans, newdata = fuel_1_horsepower)


# Replace the values back into the dataset
subset_data$horsepower_transformed_fixed <- subset_data$horsepower_transformed
subset_data$horsepower_transformed_fixed[subset_data$fuel_type_dummy == 1] <- transformed_values
#-------------


colnames(subset_data)


library(rstatix)
subset_data %>% group_by(fuel_type_dummy) %>%  shapiro_test(price_boxcox_transformed, horsepower_transformed_fixed)

library(heplots)
#Null hypothesis: variance-covariance matrices are equal for each combination formed by each group in the independent variable
boxM(Y = cbind(subset_data$horsepower_transformed_fixed,subset_data$price_boxcox_transformed), group = factor(subset_data$fuel_type_dummy))
#As the p-value is non-significant (p > 0.05)
#for Box???s M test, we fail to reject the null
#hypothesis and conclude that variance-covariance matrices are equal for each combination of
#the dependent variable formed by each group in the independent variable.


library(ICSNP)
HotellingsT2(cbind(subset_data$horsepower_transformed_fixed,subset_data$price_boxcox_transformed) ~ subset_data$fuel_type_dummy)
# Since p-value = 0.00006885 > 0.05, we fail to reject H0. Therefore, we don???t have enough evidence to prove that the mean of responses change with respect to fuel type.


# Mean vectors for groups
xbar_0 <- colMeans(cbind(
  subset_data[subset_data$fuel_type_dummy == 0, ]$horsepower_transformed_fixed,
  subset_data[subset_data$fuel_type_dummy == 0, ]$price_boxcox_transformed
))
xbar_1 <- colMeans(cbind(
  subset_data[subset_data$fuel_type_dummy == 1, ]$horsepower_transformed_fixed,
  subset_data[subset_data$fuel_type_dummy == 1, ]$price_boxcox_transformed
))


# Group sizes
n1 <- dim(subset_data[subset_data$fuel_type_dummy == 0, ])[1]
n2 <- dim(subset_data[subset_data$fuel_type_dummy == 1, ])[1]


# Parameters
p <- 2
m <- 2  # Number of variables
f <- qf(0.05, p, (n1 + n2 - p - 1), lower.tail = FALSE)
c_square <- (((n1 + n2 - 2) * p) / (n1 + n2 - p - 1)) * f
t <- qt(0.05 / (2 * m), n1 + n2 - 2, lower.tail = FALSE)


# Standard deviations for groups
sd1 <- sd(subset_data$horsepower_transformed_fixed)
sd2 <- sd(subset_data$price_boxcox_transformed)


# Simultaneous Confidence Intervals for mean differences
LC1_sci <- (xbar_0[1] - xbar_1[1]) - sqrt(c_square) * sqrt((1 / n1) + (1 / n2)) * sd1
UC1_sci <- (xbar_0[1] - xbar_1[1]) + sqrt(c_square) * sqrt((1 / n1) + (1 / n2)) * sd1
SCI_1 <- c(LC1_sci, UC1_sci)


LC2_sci <- (xbar_0[2] - xbar_1[2]) - sqrt(c_square) * sqrt((1 / n1) + (1 / n2)) * sd2
UC2_sci <- (xbar_0[2] - xbar_1[2]) + sqrt(c_square) * sqrt((1 / n1) + (1 / n2)) * sd2
SCI_2 <- c(LC2_sci, UC2_sci)


# Bonferroni Confidence Intervals for mean differences
LC1_bci <- (xbar_0[1] - xbar_1[1]) - t * sqrt((1 / n1) + (1 / n2)) * sd1
UC1_bci <- (xbar_0[1] - xbar_1[1]) + t * sqrt((1 / n1) + (1 / n2)) * sd1
BCI_1 <- c(LC1_bci, UC1_bci)


LC2_bci <- (xbar_0[2] - xbar_1[2]) - t * sqrt((1 / n1) + (1 / n2)) * sd2
UC2_bci <- (xbar_0[2] - xbar_1[2]) + t * sqrt((1 / n1) + (1 / n2)) * sd2
BCI_2 <- c(LC2_bci, UC2_bci)


# Print Results
cat("Simultaneous Confidence Intervals:\n")
cat("Horsepower: ", SCI_1, "\n")
cat("Price: ", SCI_2, "\n\n")


cat("Bonferroni Confidence Intervals:\n")
cat("Horsepower: ", BCI_1, "\n")
cat("Price: ", BCI_2, "\n")

#-------------------------------------------------------------------
#Multivariate Multiple Linear Regression


data1 <- data[, !(names(data) %in% categorical_columns)]


#normalizing curb weight
best_trans_cw <- bestNormalize(data1$curb_weight)
best_trans_cw #order norm
data1$normal_cw <- predict(best_trans_cw, data1$curb_weight) 
mvn(data1[,c("normal_cw")], mvnTest = "royston")
#Assuming normal


#Trying BoxCox transformation
lambda <- BoxCox.lambda(data1$price)
data1$price_transformed <- BoxCox(data1$price, lambda)

ggplot(data1, aes(x = price_transformed)) +
  geom_density(fill = "blue", alpha = 0.5)

mvn(data1[,c("price_transformed")], mvnTest = "royston")
#Did not work

#Try to apply ORQ
orderNorm_price <- orderNorm(data1$price)
data1$price_transformed <- predict(orderNorm_price, data1$price)
ggplot(data1, aes(x = price_transformed)) +
  geom_density(fill = "blue", alpha = 0.5)

mvn(data1[,c("price_transformed")], mvnTest = "royston")
#It worked.

#Curb weight normalizing

data1_selected <- data1[,c("symboling","price_transformed","horsepower_transformed","bore_transformed","body_style_dummy","highway_transformed","normal_cw")]

#Checking correlation values to construct a valid regression model:
corrplot(cor(data1_selected), "square")
str(data1)
colnames(data1_selected)

m1 <- lm(price_transformed ~ bore_transformed + body_style_dummy + highway_transformed + normal_cw + symboling, data = data1_selected)
m2 <- lm(horsepower_transformed ~ bore_transformed + body_style_dummy + highway_transformed + normal_cw + symboling, data = data1_selected)
mlm <- lm(cbind(price_transformed,horsepower_transformed) ~ bore_transformed + body_style_dummy + highway_transformed + normal_cw + symboling, data=data1_selected)
summary(mlm)

vif(m1)
vif(m2) #VIF values are satisfied to proceed since they are in interval (1,5)

#We have to check linearity for each dependent variable
plot(mlm$fitted.values[,1], resid(mlm)[,1],
     main = "Residuals vs. Fitted for Price")+
  abline(h=0, col="red", lwd=3)


plot(mlm$fitted.values[,2], resid(mlm)[,2],
     main = "Residuals vs. Fitted for Horsepower")+
  abline(h=0, col="red", lwd=3)
#There is no pattern around the centered line, shows linearity.


#To check normality, we can use Q-Q Plots for both dependent variables:
qqnorm(residuals(mlm)[,1], main="Q-Q Plot for Price")
qqline(residuals(mlm)[,1], col="red")

qqnorm(residuals(mlm)[,2], main="Q-Q Plot for Horsepower")
qqline(residuals(mlm)[,2], col="red")

#Moreover, we can use Shapiro-Wilk test:
shapiro.test(resid(mlm)[,1])
shapiro.test(resid(mlm)[,2])

mvn(data1_selected, mvnTest = "royston")

#For checking homoscedasticity, we can check fitted vs. residuals values. Moreover, we can use Breusch-Pagan test:  
bptest(m1)
bptest(m2)
#Since the p-values are larger than the significance level for both single models, we can conclude that homosedasticity exists.
#H0: Homoscedasticity exists, H1: Heteroscedasticity exists.

#To check independence of residuals, we can use Durbin-Watson test:
durbinWatsonTest(m1)
durbinWatsonTest(m2)
#Since output is below 2 for both single dependent models, there is a positive autocorrelation between residuals. Thus, residuals are not independent.

#-------------------------------------------------------------------
#PCA

dim(data)
View(data)
colnames(data2)
mydata <- data[,-c(1,3:9,15,16,18)]
dim(mydata)
View(mydata)
library(car)
scatterplotMatrix(mydata, diagonal="histogram")
res <- cor(mydata, method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust")
library(corrplot)
mydata<-scale(mydata)
View(mydata)
cov(mydata)
cor(mydata)
mydata1<-mydata[,-c(11,15)]
View(mydata1)
pca1 <- prcomp(mydata1)
summary(pca1)
names(pca1)
pca1$rotation
pca1$x
pca1$sdev
library(factoextra)
fviz_eig(pca1,addlabels=TRUE) #represent the proportion values
pca<-pca1$x[,1:5]
head(pca)
res1 <- cor(pca, method="pearson")
corrplot::corrplot(res1, method= "color", order = "hclust")
cor(mydata1,pca)
biplot(pca1, col = c("gray", "black"))
fviz_pca_var(pca1, col.var = "contrib")
fviz_pca_ind(pca1, label="none", habillage=data$`body-style`,
             addEllipses=TRUE, ellipse.level=0.95) #yorumlayamad??m
#PCA Regression
ols.data <- data.frame(mydata[,c(11,15)],pca)
View(ols.data)
lmodel <- lm(cbind(horsepower,price) ~ ., data = ols.data)
summary(lmodel)
mse_horsepower <- mean((ols.data$horsepower - predict(lmodel))^2) #mse for horsepower
mse_price <- mean((ols.data$price - predict(lmodel))^2) #mse for price
cat("MSE for price:", mse_price, "\n")
cat("MSE for horsepower:", mse_horsepower, "\n")

## Factor Analysis
cm <- cor(mydata, method="pearson")
corrplot::corrplot(cm, method= "number", order = "hclust")
library(psych)
KMO(r=cm)

cortest.bartlett(cm,nrow(mydata))

parallel <- fa.parallel(mydata, fm = "minres", fa = "fa")

parallel
factanal(mydata, factors = 3, method ="mle")$PVAL
factanal(mydata, factors = 4, method = "mle")$PVAL
factanal(mydata, factors = 6, method = "mle")$PVAL
factanal(mydata, factors = 7, method = "mle")$PVAL
factanal(mydata, factors = 8, method = "mle")$PVAL
factanal(mydata, factors = 9, method = "mle")$PVAL 
library(psych)
f<-factanal(mydata, factors = 3)
f

Factor 1 is dominated by variables such as engine-size, curb-weight, horsepower, and price, suggesting it represents overall vehicle performance and size. Negative loadings for city-mpg and highway-mpg indicate an inverse relationship with efficiency.
Factor 2 is dominated by wheel-base, length, and width, suggesting it reflects vehicle dimensions.
Factor 3 is largely defined by height, indicating it captures a unique dimension related to vehicle height.
Factor 4 is dominated by stroke, suggesting a specific mechanical component influence.
Factor 5 is driven by compression-ratio, reflecting engine compression characteristics.
Factors 6 through 9 contribute smaller portions to variance and may represent niche or residual relationships among variable"

#The first 6 factors have Eigenvalues > 1 (SS Loadings > 1), making them significant contributors according to the Kaiser Criterion.
Together, the 9 factors explain approximately 85.5% of the total variance in the dataset. This implies the dimensionality of the dataset has been effectively reduced from 15 variables to 9 factors, with only 14.5% of the variance lost.
Factor 1 explains 37.1% of the total variance.
Factor 2 adds 14.4%, and cumulative explained variance reaches 51.5% with the first two factors."

load <- f$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)

"Factor1 (Performance and Size): Associated with variables such as engine-size, horsepower, and price.
Factor2 (Structural Dimensions): Associated with variables such as wheel-base, height, and length."

f1<-mydata[,names(f$loadings[,3])[abs(f$loadings[,3])>0]]
summary(alpha(f1, check.keys=TRUE))

"'raw-alpha' value for the entire factor. That tells us, overall, how consistent the variables are within the factor."
"all of our raw alpha values are really small (normalde 0.8 civar?? olmas?? laz??m bizim 0.13 en y??kse??i(9 fakt??r i??inde)")

scores<-factanal(mydata, factors = 3,scores="regression")$scores
head(scores)

cm1 <- cor(scores, method="pearson")
corrplot::corrplot(cm1, method= "number", order = "hclust")

#They are almost uncorrelated which guarantees that no multicollinearity problem in linear regression.

#Regression Kodlarda Yoktu Kendim buldu??um kadar??yla
facto_df <- as.data.frame(scores)
mydata <- as.data.frame(mydata)

facto_df$price <- mydata$price
facto_df$horsepower <- mydata$horsepower

multivar_model <- lm(cbind(price, horsepower) ~ ., data = facto_df)

summary(multivar_model)
predicted_values <- as.data.frame(predict(multivar_model))
mse_price <- mean((facto_df$price - predicted_values$price)^2)
mse_horsepower <- mean((facto_df$horsepower - predicted_values$horsepower)^2)

cat("MSE for price:", mse_price, "\n")
cat("MSE for horsepower:", mse_horsepower, "\n")

#----------------------------------------------------------------------#Discriminant Analysis
library(MASS)
library(klaR)
library(ggplot2)
library(GGally)
library(mlbench)


# Enable the r-universe repo
options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))


# Install ggord
library(ggord)
colnames(data1)

#Selecting variables to work with
data1_selected1 <- data1[,c("symboling","price_transformed","horsepower_transformed","bore_transformed","body_style_dummy","highway_transformed","peakrpm_transformed", "aspiration_dummy","normal_cw","stroke_transformed","normal_engine_size","compression_transformed")]
#Transforming the target value in a factor form, which has to be categoric:
data1_selected1$risk_category <- ifelse(data1_selected1$symboling %in% c(-2,-1,0), "safe", "risky") #Editing symboling value into two factors
data1_selected1$risk_category <- as.factor(data1_selected1$risk_category)

dummy_from_original <- data[,c("aspiration","body_style")]
data1_selected1_new <- data1_selected1[,c("risk_category","price_transformed","horsepower_transformed","bore_transformed","highway_transformed","peakrpm_transformed","normal_cw","stroke_transformed","normal_engine_size","compression_transformed")]
data1_selected_new <- cbind(data1_selected1_new, dummy_from_original)
str(data1_selected_new)
data1_selected_new$aspiration <- as.factor(data1_selected_new$aspiration)
data1_selected_new$body_style <- as.factor(data1_selected_new$body_style)

#Checking the structure and summary of the data
str(data1_selected_new)
summary(data1_selected_new)

#After numerical analysis, we can visualize our findings.
GGally::ggpairs(data1_selected_new)

#Drawing the same plot for distinction of the variables on our target value:
GGally::ggpairs(data1_selected_new, aes(color=risk_category))

str(train)

#For further step, we have to split data into two parts, train and test:
sample <- sample(c(TRUE,FALSE), nrow(data1_selected_new), replace=TRUE, prob=c(0.8, 0.2))
train <- data1_selected_new[sample,]
test <- data1_selected_new[!sample,]

#Getting the values of the loadings of the discriminant functions:
model <- lda(risk_category~., data=train)
print(model) #According to the model, %87 of the training observations seem risky. Also, the model can be constructed with the coefficients obtained by FDA.

#Linear discriminants received can be seen from the plot below:
plot(model)

#The plot below provides us the classification of every combination in the train data:
sample_partition <- sample(c(TRUE,FALSE), nrow(data1_selected1), replace=TRUE, prob=c(0.8, 0.2))
train_partition <- data1_selected1[sample,]
partimat(as.factor(risk_category)~., data = train_partition, method = "lda")

#Now, it is time for evaluating the performance of the model:
train_predict <- predict(model, train)$class
table_train <- table(Predicted = train_predict, Actual = train$risk_category)
print(table_train)

#To see the testing result more precisely, we can calculate the accuracy of the model:
sum(diag(table_train))/sum(table_train)

#Time to calculate the test performance:
test_predict <- predict(model, test)$class
table_test <- table(Predicted = test_predict, Actual = test$risk_category)
print(table_test)

#Calculating the accuracy:
sum(diag(table_test))/sum(table_test)

#clustering
library(cluster)
library(ggplot2)
library(car)
library(dplyr)
library(factoextra)
library(clustree)

data1$risk_category <- ifelse(data1$symboling %in% c(-2,-1,0), "safe", "risky") #Editing symboling value into two factors
data1$risk_category <- as.factor(data1$risk_category)
subset_data3 <- data1[, c("curb_weight","engine_size","price", "horsepower","risk_category")]
numerical <- subset_data3[,c("curb_weight","engine_size","price", "horsepower")]

#Agglomerative hierarchical clustering
dm <- dist(numerical)
dm

par(mfrow=c(2,2),mar=c(1,2,1,2))
plot(cs <- hclust(dm, method = "single"),main = "Single Linkage")
plot(cc <- hclust(dm, method = "complete"),main = "Complete Linkage")
plot(ca <- hclust(dm, method = "average"),main = "Average Linkage")
plot(cw <- hclust(dm, method = "ward.D2"),main = "Ward Method")

par(mfrow=c(1,1))

X <- scale(numerical, center = FALSE, scale = TRUE)
dx <- dist(X) 
plot(cc <- hclust(dx), main = "Automobile clustering")

#Divisive Hierarchical Clustering
fviz_dend(cc, cex = 0.5,
          k = 4, # Cut in four groups
          palette = "jco" # Color palette
)


#K-means Clustering
scatterplotMatrix(subset_data3)

# Determine the optimal number of clusters using the Elbow Method
wss <- sapply(2:10, function(k) {
  kmeans(X, centers = k, nstart = 25)$tot.withinss
})

# Plot the Elbow Method results
elbow_plot <- ggplot(data.frame(k = 2:10, wss = wss), aes(x = k, y = wss)) +
  geom_point() + geom_line() +
  ggtitle("Elbow Method for Optimal Clusters") +
  xlab("Number of Clusters") + ylab("Total Within Sum of Squares")
print(elbow_plot)

# Determine the optimal number of clusters using NbClust
nb <- NbClust(X, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
# Extract the optimal number of clusters suggested by NbClust
optimal_clusters <- as.numeric(names(which.max(table(nb$Best.n[1,]))))
cat("optimal number of cluster is", optimal_clusters)

# Perform K-means clustering with the optimal number of clusters
final_kmeans <- kmeans(X, centers = optimal_clusters, nstart = 25)
subset_data3$cluster <- final_kmeans$cluster

# Create clustree data by adding cluster assignments for each k to the scaled data
clustree_data <- X
for (k in 1:optimal_clusters) {
  set.seed(42)
  clustree_data <- cbind(clustree_data, kmeans(X, centers = k, nstart = 25)$cluster)
}

# Convert to data frame and add proper names for clustree columns
clustree_data <- as.data.frame(clustree_data)
colnames(clustree_data)[(ncol(X) + 1):ncol(clustree_data)] <- paste0("cluster", 1:optimal_clusters)

# Create clustree plot
clustree_plot <- clustree(clustree_data, prefix = "cluster")

# Add the risk_category back into the clustering data for labeling
subset_data3$risk_category <- subset_data3$risk_category[!is.na(subset_data3$price)]

# Perform PCA for plotting
pca_result <- prcomp(X)
pca_data <- data.frame(pca_result$x[, 1:2], cluster = subset_data3$cluster)

# Create the PCA plot with clusters and annotate with addr_state
cluster_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = factor(cluster))) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = subset_data3$risk_category), check_overlap = TRUE, vjust = 1.5, hjust = 1.5) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "PCA Cluster Plot with Risk Labels")

# Print the plots
print(clustree_plot)
print(cluster_plot)
















