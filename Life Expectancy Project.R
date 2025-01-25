###BS806 Final Project
library(tidyverse)
library(olsrr)
library(car)
library(pROC)
data <- read.csv("C:/Users/noahs/OneDrive/Desktop/Fall 2024/BS806 Multivariable Analysis/Project/Life Expectancy Data Updated.csv")
view(data)
ncol(data)
nrow(data)
names(data)
sum(is.na(data))
data$Economy_status_Developing <- NULL
data$Region[which(data$Country == "Mexico")] <- "Central America and Caribbean"
data$Region_Category <- ifelse(data$Region %in% c("Asia", "Oceania"), 1,
                               ifelse(data$Region %in% c("North America", "European Union", "Rest of Europe"), 2,
                                      ifelse(data$Region == "Africa", 3,
                                             ifelse(data$Region %in% c("South America", "Central America and Caribbean"), 4, 
                                                    ifelse(data$Region == "Middle East", 5, NA)))))
data$Region_Category <- as.factor(data$Region_Category)
data2014 <- data %>% filter(Year == 2014)

### Clustering 
rownames(data2014) <- data2014[,1]
data2014 <- data2014[,4:20]
data.2 <- scale(data2014)

#view(data.2)
set.seed(7)
Wss <- c()
for (i in 1:10){
  km <- kmeans(data.2, i)
  Wss <- c(Wss, km$tot.withinss )}
plot(c(1:10), Wss, type = "l")
#choose 4
set.seed(7)
km <- kmeans(data.2, 3)
km$size
cluster.characteristics <- km$centers
view(cluster.characteristics)
set.seed(2014)
pca <- prcomp(data.2, scale = T)
plot(pca$x[,1], pca$x[,2])
for (i in 1:3){
  points(pca$x[which(km$cluster == i), 1],
         pca$x[which(km$cluster == i), 2],col = i)
}
clustered_countries <- data.frame(
  Country = rownames(data.2),  
  Cluster = km$cluster)

datacluster <- left_join(data, clustered_countries, by = "Country")

cluster.characteristics1 <- cbind(km$size, cluster.characteristics)
colnames(cluster.characteristics1)[1]<- "Size of Cluster"
view(cluster.characteristics1)
#################################
###RUN ABOVE BEFORE RUNNING BELOW
#################################





###Countries to CSV
asian.countries  <- unique(data$Country[data$Region_Category == 1])
view(asian.countries)
write.csv(asian.countries, "asian_countries.csv", row.names = FALSE)

na.eu.countries  <- unique(data$Country[data$Region_Category == 2])
view(na.eu.countries)
write.csv(na.eu.countries, "na_eu_countries.csv", row.names = FALSE)

africa.countries  <- unique(data$Country[data$Region_Category == 3])
view(africa.countries)
write.csv(africa.countries, "africa_countries.csv", row.names = FALSE)

sa.countries  <- unique(data$Country[data$Region_Category == 4])
view(sa.countries)
write.csv(sa.countries, "sa_countries.csv", row.names = FALSE)

me.countries  <- unique(data$Country[data$Region_Category == 5])
view(me.countries)
write.csv(me.countries, "me_countries.csv", row.names = FALSE)

cluster1.countries  <- unique(cluster1$Country)
view(cluster1.countries)
write.csv(cluster1.countries, "cluster1_countries.csv", row.names = FALSE)

cluster2.countries  <- unique(cluster2$Country)
view(cluster2.countries)
write.csv(cluster2.countries, "cluster2_countries.csv", row.names = FALSE)

cluster3.countries  <- unique(cluster3$Country)
view(cluster3.countries)
write.csv(cluster3.countries, "cluster3_countries.csv", row.names = FALSE)

data2014.cluster <- datacluster %>% filter(Year == 2014)

cluster1 <- data2014.cluster %>% filter(Cluster == 1)
cluster2 <- data2014.cluster %>% filter(Cluster == 2) 
cluster3 <- data2014.cluster %>% filter(Cluster == 3) 




#Linear Regression and Variable Selection; stratified by region
#Asia Stratum
view(data)
nrow(asia)
asia<- data %>% filter(Region %in% c("Asia", "Oceania")) %>% filter(Year == 2014) %>% select(4:20)
asia$Diphtheria <- NULL
asia$Thinness_five_nine_years <- NULL
asia$Infant_deaths <- NULL
asia.mod <- lm(Life_expectancy ~ ., data = asia); summary(asia.mod)

ols_vif_tol(asia.mod)

#remove economic status developed, diphtheria, thinness 5-9, infant_deaths, Polio from the variable selection
#variable selection asia
f.asia <- ~ Under_five_deaths + Adult_mortality + Alcohol_consumption + Hepatitis_B + Measles + Polio + BMI  + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed 
m.asia <- lm(Life_expectancy ~ 1, data = asia)
m.asia.forward <- step(m.asia, scope = f.asia, direction = "forward", k = 2)

summary(m.asia.forward)
summary(m.asia.forward)$coefficients
AIC(m.asia.forward)

#multicollinearity check
ols_vif_tol(m.asia.forward)
#Test for normality
shapiro.test(m.asia.forward$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.asia.forward), residuals(m.asia.forward), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals(m.asia.forward), ylab = "residuals")
qqline(residuals(m.asia.forward))
qqPlot(m.asia.forward$residuals)
hist(residuals(m.asia.forward))
###Predict 2000 and 2015
asia2015<- data %>% filter(Region %in% c("Asia", "Oceania")) %>% filter(Year == 2015) %>% select(4:20)
asiapred1 <- as.vector(predict(m.asia.forward, newdata = asia2015))

asia2000<- data %>% filter(Region %in% c("Asia", "Oceania")) %>% filter(Year == 2000) %>% select(4:20)
asiapred2 <- as.vector(predict(m.asia.forward, newdata = asia2000))

mae.asia1 <- mean(abs(asia2015$Life_expectancy - asiapred1)); mae.asia1
mae.asia2 <- mean(abs(asia2000$Life_expectancy - asiapred2)); mae.asia2
# Mean Squared Error (MSE)
mse.asia1 <- mean((asia2015$Life_expectancy - asiapred1)^2); mse.asia1
mse.asia2 <- mean((asia2000$Life_expectancy - asiapred2)^2); mse.asia2
# Root Mean Squared Error (RMSE)
rmse.asia1 <- sqrt(mse.asia1);rmse.asia1
rmse.asia2 <- sqrt(mse.asia2);rmse.asia2
# R-squared
r_squared.asia1 <- 1 - (sum((asia2015$Life_expectancy - asiapred1)^2) / 
                    sum((asia2015$Life_expectancy - mean(asia2015$Life_expectancy))^2)); r_squared.asia1
r_squared.asia2 <- 1 - (sum((asia2000$Life_expectancy - asiapred2)^2) / 
                          sum((asia2000$Life_expectancy - mean(asia2000$Life_expectancy))^2)); r_squared.asia2

######North America Stratum
nrow(na.eu)
na.eu<- data %>% filter(Region %in% c("North America", "European Union", "Rest of Europe")) %>% filter(Year == 2014) %>% select(4:20)
na.eu$Diphtheria <- NULL
na.eu$Thinness_five_nine_years <- NULL
na.eu$Infant_deaths <- NULL
na.eu.mod <- lm(Life_expectancy ~ ., data = na.eu); summary(na.eu.mod)

ols_vif_tol(na.eu.mod)

#variable selection NA
f.na.eu <- ~ Under_five_deaths + Adult_mortality + Alcohol_consumption + Polio + Hepatitis_B + Measles + BMI  + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed 
m.na.eu <- lm(Life_expectancy ~ 1, data = na.eu)
m.na.eu.forward <- step(m.na.eu, scope = f.na.eu, direction = "forward", k = 2)

summary(m.na.eu.forward)
AIC(m.na.eu.forward)
summary(m.na.eu.forward)$coefficients

#multicollinearity check
ols_vif_tol(m.na.eu.forward)
#Test for normality
shapiro.test(m.na.eu.forward$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.na.eu.forward), residuals(m.na.eu.forward), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals(m.na.eu.forward), ylab = "residuals")
qqline(residuals(m.na.eu.forward))
qqPlot(m.na.eu.forward$residuals)
hist(residuals(m.na.eu.forward))

###Predict 2000 and 2015
na.eu2015<- data %>% filter(Region_Category == 2) %>% filter(Year == 2015) %>% select(4:20)
na.eupred1 <- as.vector(predict(m.na.eu.forward, newdata = na.eu2015))

na.eu2000<- data %>% filter(Region_Category == 2) %>% filter(Year == 2000) %>% select(4:20)
na.eupred2 <- as.vector(predict(m.na.eu.forward, newdata = na.eu2000))

mae.na.eu1 <- mean(abs(na.eu2015$Life_expectancy - na.eupred1)); mae.na.eu1
mae.na.eu2 <- mean(abs(na.eu2000$Life_expectancy - na.eupred2)); mae.na.eu2
# Mean Squared Error (MSE)
mse.na.eu1 <- mean((na.eu2015$Life_expectancy - na.eupred1)^2); mse.na.eu1
mse.na.eu2 <- mean((na.eu2000$Life_expectancy - na.eupred2)^2); mse.na.eu2
# Root Mean Squared Error (RMSE)
rmse.na.eu1 <- sqrt(mse.na.eu1);rmse.na.eu1
rmse.na.eu2 <- sqrt(mse.na.eu2);rmse.na.eu2
# R-squared
r_squared.na.eu1 <- 1 - (sum((na.eu2015$Life_expectancy - na.eupred1)^2) / 
                          sum((na.eu2015$Life_expectancy - mean(na.eu2015$Life_expectancy))^2)); r_squared.na.eu1
r_squared.na.eu2 <- 1 - (sum((na.eu2000$Life_expectancy - na.eupred2)^2) / 
                          sum((na.eu2000$Life_expectancy - mean(na.eu2000$Life_expectancy))^2)); r_squared.na.eu2


###Africa Stratum

nrow(africa)
africa<- data %>% filter(Region %in% c("Africa","Middle Easy")) %>% filter(Year == 2014) %>% select(4:20)
africa$Diphtheria <- NULL
africa$Polio <- NULL
africa$Thinness_five_nine_years <- NULL
africa$Infant_deaths <- NULL
africa.mod <- lm(Life_expectancy ~ ., data = africa); summary(africa.mod)

ols_vif_tol(africa.mod)

#remove economic status developed, diphtheria, thinness 5-9, infant_deaths, Polio from the variable selection
#variable selection Africa
f.africa <- ~ Under_five_deaths + Adult_mortality + Alcohol_consumption + Hepatitis_B + Measles + BMI  + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed 
m.africa <- lm(Life_expectancy ~ 1, data = africa)
m.africa.forward <- step(m.africa, scope = f.africa, direction = "forward", k = 2)

summary(m.africa.forward)
AIC(m.africa.forward)
summary(m.africa.forward)$coefficients

#multicollinearity check
ols_vif_tol(m.africa.forward)
#Test for normality
shapiro.test(m.africa.forward$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.africa.forward), residuals(m.africa.forward), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals(m.africa.forward), ylab = "residuals")
qqline(residuals(m.africa.forward))
qqPlot(m.africa.forward$residuals)
hist(residuals(m.africa.forward))

###Predict 2000 and 2015
africa2015<- data %>% filter(Region_Category == 3) %>% filter(Year == 2015) %>% select(4:20)
africapred1 <- as.vector(predict(m.africa.forward, newdata = africa2015))

africa2000<- data %>% filter(Region_Category == 3) %>% filter(Year == 2000) %>% select(4:20)
africapred2 <- as.vector(predict(m.africa.forward, newdata = africa2000))

mae.africa1 <- mean(abs(africa2015$Life_expectancy - africapred1)); mae.africa1
mae.africa2 <- mean(abs(africa2000$Life_expectancy - africapred2)); mae.africa2
# Mean Squared Error (MSE)
mse.africa1 <- mean((africa2015$Life_expectancy - africapred1)^2); mse.africa1
mse.africa2 <- mean((africa2000$Life_expectancy - africapred2)^2); mse.africa2
# Root Mean Squared Error (RMSE)
rmse.africa1 <- sqrt(mse.africa1);rmse.africa1
rmse.africa2 <- sqrt(mse.africa2);rmse.africa2
# R-squared
r_squared.africa1 <- 1 - (sum((africa2015$Life_expectancy - africapred1)^2) / 
                           sum((africa2015$Life_expectancy - mean(africa2015$Life_expectancy))^2)); r_squared.africa1
r_squared.africa2 <- 1 - (sum((africa2000$Life_expectancy - africapred2)^2) / 
                           sum((africa2000$Life_expectancy - mean(africa2000$Life_expectancy))^2)); r_squared.africa2

###Central/South Stratum

nrow(ca.sa)
ca.sa<- data %>% filter(Region %in% c("Central America and Caribbean", "South America")) %>% filter(Year == 2014) %>% select(4:20)
ca.sa$Diphtheria <- NULL
ca.sa$Thinness_five_nine_years <- NULL
ca.sa$Infant_deaths <- NULL
ca.sa.mod <- lm(Life_expectancy ~ ., data = ca.sa); summary(ca.sa.mod)

ols_vif_tol(ca.sa.mod)


#variable selection SA
f.ca.sa <- ~ Under_five_deaths + Adult_mortality + Alcohol_consumption + Hepatitis_B + Polio + Measles + BMI  + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed 
m.ca.sa <- lm(Life_expectancy ~ 1, data = ca.sa)
m.ca.sa.forward <- step(m.ca.sa, scope = f.ca.sa, direction = "forward", k = 2)

summary(m.ca.sa.forward)
AIC(m.ca.sa.forward)
summary(m.ca.sa.forward)$coefficients

#multicollinearity check
ols_vif_tol(m.ca.sa.forward)
#Test for normality
shapiro.test(m.ca.sa.forward$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.ca.sa.forward), residuals(m.ca.sa.forward), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals(m.ca.sa.forward), ylab = "residuals")
qqline(residuals(m.ca.sa.forward))
qqPlot(m.ca.sa.forward$residuals)
hist(residuals(m.ca.sa.forward))

###PREDICT 2000 and 2015
ca.sa2015 <- data %>% filter(Region_Category == 4) %>% filter(Year == 2015) %>% select(4:20)
casapred1 <- as.vector(predict(m.ca.sa.forward, newdata = ca.sa2015))

ca.sa2000 <- data %>% filter(Region_Category == 4) %>% filter(Year == 2000) %>% select(4:20)
casapred2 <- as.vector(predict(m.ca.sa.forward, newdata = ca.sa2000))

# Mean Absolute Error (MAE)
mae.ca.sa1 <- mean(abs(ca.sa2015$Life_expectancy - casapred1)); mae.ca.sa1
mae.ca.sa2 <- mean(abs(ca.sa2000$Life_expectancy - casapred2)); mae.ca.sa2

# Mean Squared Error (MSE)
mse.ca.sa1 <- mean((ca.sa2015$Life_expectancy - casapred1)^2); mse.ca.sa1
mse.ca.sa2 <- mean((ca.sa2000$Life_expectancy - casapred2)^2); mse.ca.sa2

# Root Mean Squared Error (RMSE)
rmse.ca.sa1 <- sqrt(mse.ca.sa1); rmse.ca.sa1
rmse.ca.sa2 <- sqrt(mse.ca.sa2); rmse.ca.sa2

# R-squared
r_squared.ca.sa1 <- 1 - (sum((ca.sa2015$Life_expectancy - casapred1)^2) / 
                           sum((ca.sa2015$Life_expectancy - mean(ca.sa2015$Life_expectancy))^2)); r_squared.ca.sa1
r_squared.ca.sa2 <- 1 - (sum((ca.sa2000$Life_expectancy - casapred2)^2) / 
                           sum((ca.sa2000$Life_expectancy - mean(ca.sa2000$Life_expectancy))^2)); r_squared.ca.sa2


###Middle East Stratum
mid.east<- data %>% filter(Region %in% c("Middle East")) %>% filter(Year == 2014) %>% select(4:20)
mid.east$Diphtheria <- NULL
mid.east$Polio <- NULL
mid.east$Thinness_five_nine_years <- NULL
mid.east$Infant_deaths <- NULL
mid.east$Adult_mortality <- NULL
mid.east$Hepatitis_B <- NULL
mid.east$BMI <- NULL

mid.east.mod <- lm(Life_expectancy ~ ., data = mid.east); summary(mid.east.mod)

ols_vif_tol(mid.east.mod)

#view(mid.east)
#remove economic status developed, diphtheria, thinness 5-9, infant_deaths from the variable selection
#variable selection Africa
f.mid.east <- ~ Under_five_deaths  + Alcohol_consumption + Measles+ Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed 
m.mid.east <- lm(Life_expectancy ~ 1, data = mid.east)
m.mid.east.forward <- step(m.mid.east, scope = f.mid.east, direction = "forward", k = 2)

summary(m.mid.east.forward)
summary(m.mid.east.forward)$coefficients
AIC(m.mid.east.forward)

#multicollinearity check
ols_vif_tol(m.mid.east.forward)
#Test for normality
shapiro.test(m.mid.east.forward$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.mid.east.forward), residuals(m.mid.east.forward), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals(m.mid.east.forward), ylab = "residuals")
qqline(residuals(m.mid.east.forward))
qqPlot(m.mid.east.forward$residuals)
hist(residuals(m.mid.east.forward))

###PREDICT 2000 and 2015

mid.east2015 <- data %>% filter(Region_Category == 5) %>% filter(Year == 2015) %>% select(4:20)
mideastpred1 <- as.vector(predict(m.mid.east.forward, newdata = mid.east2015))

mid.east2000 <- data %>% filter(Region_Category == 5) %>% filter(Year == 2000) %>% select(4:20)
mideastpred2 <- as.vector(predict(m.mid.east.forward, newdata = mid.east2000))

# Mean Absolute Error (MAE)
mae.mid.east1 <- mean(abs(mid.east2015$Life_expectancy - mideastpred1)); mae.mid.east1
mae.mid.east2 <- mean(abs(mid.east2000$Life_expectancy - mideastpred2)); mae.mid.east2

# Mean Squared Error (MSE)
mse.mid.east1 <- mean((mid.east2015$Life_expectancy - mideastpred1)^2); mse.mid.east1
mse.mid.east2 <- mean((mid.east2000$Life_expectancy - mideastpred2)^2); mse.mid.east2

# Root Mean Squared Error (RMSE)
rmse.mid.east1 <- sqrt(mse.mid.east1); rmse.mid.east1
rmse.mid.east2 <- sqrt(mse.mid.east2); rmse.mid.east2

# R-squared
r_squared.mid.east1 <- 1 - (sum((mid.east2015$Life_expectancy - mideastpred1)^2) / 
                              sum((mid.east2015$Life_expectancy - mean(mid.east2015$Life_expectancy))^2)); r_squared.mid.east1
r_squared.mid.east2 <- 1 - (sum((mid.east2000$Life_expectancy - mideastpred2)^2) / 
                              sum((mid.east2000$Life_expectancy - mean(mid.east2000$Life_expectancy))^2)); r_squared.mid.east2




####CLUSTER REGRESSION

data2014.cluster <- datacluster %>% filter(Year == 2014)


### Linear Regression stratified by clusters 
#Cluster 1
cluster1 <- data2014.cluster %>% filter(Cluster == 1) %>% select(4:20)
view(cluster1)

cluster1$Diphtheria <- NULL
cluster1$Thinness_five_nine_years <- NULL
cluster1$Infant_deaths <- NULL
cluster1$Economy_status_Developed <- NULL
cluster1.mod <- lm(Life_expectancy ~ ., data = cluster1); summary(cluster1.mod)

ols_vif_tol(cluster1.mod)


#variable selection
f.cluster1 <- ~ Adult_mortality + Under_five_deaths + Alcohol_consumption + Hepatitis_B + Polio + Measles  + BMI  + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed
m.cluster1 <- lm(Life_expectancy ~ 1, data = cluster1)
m.cluster1.forward <- step(m.cluster1, scope = f.cluster1, direction = "forward", k = 2)

summary(m.cluster1.forward)
summary(m.cluster1.forward)$coefficients
AIC(m.cluster1.forward)
#multicollinearity check
ols_vif_tol(m.cluster1.forward)
#Test for normality
shapiro.test(m.cluster1.forward$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.cluster1.forward), residuals(m.cluster1.forward), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals(m.cluster1.forward), ylab = "residuals")
qqline(residuals(m.cluster1.forward))
qqPlot(m.cluster1.forward$residuals)
hist(residuals(m.cluster1.forward))

km$size

cluster.1.2015 <- datacluster %>% filter(Cluster == 1) %>% filter(Year == 2015)
cluster.1.pred1 <- as.vector(predict(m.cluster1.forward, newdata = cluster.1.2015))

cluster.1.2000 <- datacluster %>% filter(Cluster == 1) %>% filter(Year == 2000)
cluster.1.pred2 <- as.vector(predict(m.cluster1.forward, newdata = cluster.1.2000))

# Mean Absolute Error (MAE)
mae.cluster.1.2015 <- mean(abs(cluster.1.2015$Life_expectancy - cluster.1.pred1)); mae.cluster.1.2015
mae.cluster.1.2000 <- mean(abs(cluster.1.2000$Life_expectancy - cluster.1.pred2)); mae.cluster.1.2000

# Mean Squared Error (MSE)
mse.cluster.1.2015 <- mean((cluster.1.2015$Life_expectancy - cluster.1.pred1)^2); mse.cluster.1.2015
mse.cluster.1.2000 <- mean((cluster.1.2000$Life_expectancy - cluster.1.pred2)^2); mse.cluster.1.2000

# Root Mean Squared Error (RMSE)
rmse.cluster.1.2015 <- sqrt(mse.cluster.1.2015); rmse.cluster.1.2015
rmse.cluster.1.2000 <- sqrt(mse.cluster.1.2000); rmse.cluster.1.2000

# R-squared
r_squared.cluster.1.2015 <- 1 - (sum((cluster.1.2015$Life_expectancy - cluster.1.pred1)^2) / 
                                   sum((cluster.1.2015$Life_expectancy - mean(cluster.1.2015$Life_expectancy))^2)); r_squared.cluster.1.2015
r_squared.cluster.1.2000 <- 1 - (sum((cluster.1.2000$Life_expectancy - cluster.1.pred2)^2) / 
                                   sum((cluster.1.2000$Life_expectancy - mean(cluster.1.2000$Life_expectancy))^2)); r_squared.cluster.1.2000
anova(m.cluster1.forward, m.cluster2.forward)



#Cluster 2
cluster2 <- data2014.cluster %>% filter(Cluster == 2) %>% select(4:20)


cluster2$Diphtheria <- NULL
cluster2$Thinness_five_nine_years <- NULL
cluster2$Infant_deaths <- NULL
cluster2.mod <- lm(Life_expectancy ~ ., data = cluster2); summary(cluster2.mod)

ols_vif_tol(cluster2.mod)



#variable selection 
f.cluster2 <- ~ Under_five_deaths + Adult_mortality + Alcohol_consumption  + Hepatitis_B + Polio + Measles + BMI  + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed 
m.cluster2 <- lm(Life_expectancy ~ 1, data = cluster2)
m.cluster2.forward <- step(m.cluster2, scope = f.cluster2, direction = "forward", k = 2)

summary(m.cluster2.forward)
summary(m.cluster2.forward)$coefficients
AIC(m.cluster2.forward)
#multicollinearity check
ols_vif_tol(m.cluster2.forward)
#Test for normality
shapiro.test(m.cluster2.forward$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.cluster2.forward), residuals(m.cluster2.forward), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals(m.cluster2.forward), ylab = "residuals")
qqline(residuals(m.cluster2.forward))
qqPlot(m.cluster2.forward$residuals)
hist(residuals(m.cluster2.forward))


####Predict 2000 to 2015

cluster.2.2015 <- datacluster %>% filter(Cluster == 2) %>% filter(Year == 2015)
cluster.2.pred1 <- as.vector(predict(m.cluster2.forward, newdata = cluster.2.2015))

cluster.2.2000 <- datacluster %>% filter(Cluster == 2) %>% filter(Year == 2000)
cluster.2.pred2 <- as.vector(predict(m.cluster2.forward, newdata = cluster.2.2000))

# Mean Absolute Error (MAE)
mae.cluster.2.2015 <- mean(abs(cluster.2.2015$Life_expectancy - cluster.2.pred1)); mae.cluster.2.2015
mae.cluster.2.2000 <- mean(abs(cluster.2.2000$Life_expectancy - cluster.2.pred2)); mae.cluster.2.2000

# Mean Squared Error (MSE)
mse.cluster.2.2015 <- mean((cluster.2.2015$Life_expectancy - cluster.2.pred1)^2); mse.cluster.2.2015
mse.cluster.2.2000 <- mean((cluster.2.2000$Life_expectancy - cluster.2.pred2)^2); mse.cluster.2.2000

# Root Mean Squared Error (RMSE)
rmse.cluster.2.2015 <- sqrt(mse.cluster.2.2015); rmse.cluster.2.2015
rmse.cluster.2.2000 <- sqrt(mse.cluster.2.2000); rmse.cluster.2.2000

# R-squared
r_squared.cluster.2.2015 <- 1 - (sum((cluster.2.2015$Life_expectancy - cluster.2.pred1)^2) / 
                                   sum((cluster.2.2015$Life_expectancy - mean(cluster.2.2015$Life_expectancy))^2)); r_squared.cluster.2.2015
r_squared.cluster.2.2000 <- 1 - (sum((cluster.2.2000$Life_expectancy - cluster.2.pred2)^2) / 
                                   sum((cluster.2.2000$Life_expectancy - mean(cluster.2.2000$Life_expectancy))^2)); r_squared.cluster.2.2000



#Cluster 3
cluster3 <- data2014.cluster %>% filter(Cluster == 3) %>% select(4:20)

cluster3$Diphtheria <- NULL
cluster3$Thinness_five_nine_years <- NULL
cluster3$Infant_deaths <- NULL
cluster3.mod <- lm(Life_expectancy ~ ., data = cluster3); summary(cluster3.mod)

ols_vif_tol(cluster3.mod)

# Variable selection 
f.cluster3 <- ~ Under_five_deaths + Adult_mortality + Alcohol_consumption  + Hepatitis_B + Polio + Measles + BMI  + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed 
m.cluster3 <- lm(Life_expectancy ~ 1, data = cluster3)
m.cluster3.forward <- step(m.cluster3, scope = f.cluster3, direction = "forward", k = 2)

summary(m.cluster3.forward)
summary(m.cluster3.forward)$coefficients
AIC(m.cluster3.forward)

# Multicollinearity check
ols_vif_tol(m.cluster3.forward)

# Test for normality
shapiro.test(m.cluster3.forward$residuals)

# Variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.cluster3.forward), residuals(m.cluster3.forward), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals(m.cluster3.forward), ylab = "residuals")
qqline(residuals(m.cluster3.forward))
qqPlot(m.cluster3.forward$residuals)
hist(residuals(m.cluster3.forward))

#### Predict 2000 to 2015

cluster.3.2015 <- datacluster %>% filter(Cluster == 3) %>% filter(Year == 2015)
cluster.3.pred1 <- as.vector(predict)
                             




# Dummy Variables
data2014.dummy <- data %>% filter(Year == 2014) %>% select(4:21)

view(data2014.dummy)

data2014.dummy$Diphtheria <- NULL
data2014.dummy$Thinness_five_nine_years <- NULL
data2014.dummy$Infant_deaths <- NULL
data2014.dummy.mod <- lm(Life_expectancy ~ ., data = data2014.dummy); summary(data2014.dummy.mod)

ols_vif_tol(data2014.dummy.mod)

names(data2014.dummy)
mean(data$Life_expectancy[data$Region_Category ==1])
mean(data$Life_expectancy[data$Region_Category ==2])
mean(data$Life_expectancy[data$Region_Category ==3])
mean(data$Life_expectancy[data$Region_Category ==4])
mean(data$Life_expectancy[data$Region_Category ==5])
f.data2014.dummy<- ~ Under_five_deaths + Adult_mortality + Alcohol_consumption + Hepatitis_B + Polio + Measles + BMI  + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years  + Schooling + Economy_status_Developed + Region_Category
m.data2014.dummy <- lm(Life_expectancy ~ 1, data = data2014.dummy)
m.data2014.dummy.forward <- step(m.data2014.dummy, scope = f.data2014.dummy, direction = "forward", k = 2)
names(data2014.dummy)


summary(m.data2014.dummy.forward)
summary(m.data2014.dummy.forward)$coefficients

#multicollinearity check
ols_vif_tol(m.data2014.dummy.forward)
#Test for normality
shapiro.test(m.data2014.dummy.forward$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(m.data2014.dummy.forward), residuals(m.data2014.dummy.forward), xlab = "Fitted", ylab = "Residuals")
qqnorm(residuals(m.data2014.dummy.forward), ylab = "residuals")
qqline(residuals(m.data2014.dummy.forward))
qqPlot(m.data2014.dummy.forward$residuals)
hist(residuals(m.data2014.dummy.forward))


### Simple Dummy
dummy <- lm (Life_expectancy ~  Region_Category, data = data2014.dummy)
summary(dummy)

#multicollinearity check
ols_vif_tol(dummy)
#Test for normality
shapiro.test(dummy$residuals)
#variance and normality plots
par(mfrow=c(2,2))
plot(fitted(dummy), residuals(dummy), xlab = "Fitted", ylab = "Residuals")
qqnorm(residuals(dummy), ylab = "residuals")
qqline(residuals(dummy))
qqPlot(dummy$residuals)
hist(residuals(dummy))



AIC(dummy)
AIC(m.data2014.dummy.forward)

data %>% filter(Country == "United States") %>% view()

#FULL MODEL 1

view(data)

#Data2014 
data2014 <- data %>% filter(Year == 2014)
data2014 <- data2014 %>% select(4:20)
view(data2014)
data2014$Diphtheria <- NULL
data2014$Thinness_five_nine_years <- NULL
data2014$Infant_deaths <- NULL
data2014.mod <- lm(Life_expectancy ~ Adult_mortality + Under_five_deaths + GDP_per_capita, data = data2014); summary(data2014.mod)
AIC(data2014.mod)

#PREDICTION from this model
data2015 <- data %>% filter(Year == 2015)

pred2015 <- predict(data2014.mod, newdata = data2015); pred2015
real2015 <- data2015$Life_expectancy

data$Adult_mortality
MSE.full1 <- mean((real2015 - pred2015)^2)

# Mean Absolute Error (MAE)
MAE.full1 <- mean(abs(real2015 - pred2015))

# Root Mean Squared Error (RMSE)
RMSE.full1 <- sqrt(MSE.full1)

# R-squared
SS_residual.full1 <- sum((real2015 - pred2015)^2)
SS_total.full1 <- sum((real2015 - mean(real2015))^2)
R_squared.full1 <- 1 - (SS_residual.full1 / SS_total.full1)

# Print results
MSE.full1
MAE.full1
RMSE.full1
R_squared.full1

data2015combined <- cbind(real2015, pred2015)
colnames(data2015combined) <- c("Life_expectancy", "Predicted_Life_expectancy")
view(data2015combined)

ggplot(data2015combined, aes(x = Life_expectancy, y = Predicted_Life_expectancy)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "black") +
  labs(
    title = "Predicted vs. Real Life Expectancy for 2015 All Countries Predicted from Year 2014 Data",
    x = "Real Life Expectancy",
    y = "Predicted Life Expectancy"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12)) 


residuals <- real2015 - pred2015

ggplot(data.frame(Predicted = pred2015, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Predicted", x = "Predicted Life Expectancy", y = "Residuals") +
  theme_minimal()


ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(bins = 20, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()



###FUll model 2 from clusters:
# From Cluster 1 and 2 and 3: adult mortality, under five deaths, alcohol consumpion, bmi, population_mln

data2014.2 <- data %>% filter(Year == 2014)

data2014.model2 <- lm(Life_expectancy ~ Adult_mortality + Under_five_deaths +  Alcohol_consumption , data = data2014.2); summary(data2014.model2)
AIC(data2014.model2)


#PREDICTION from model 2
data2015 <- data %>% filter(Year == 2015)

pred2015.2 <- predict(data2014.model2, newdata = data2015); pred2015.2
real2015 <- data2015$Life_expectancy


MSE2 <- mean((real2015 - pred2015.2)^2)

# Mean Absolute Error (MAE)
MAE2 <- mean(abs(real2015 - pred2015.2))

# Root Mean Squared Error (RMSE)
RMSE2 <- sqrt(MSE2)

# R-squared
SS_residual2 <- sum((real2015 - pred2015.2)^2)
SS_total2 <- sum((real2015 - mean(real2015))^2)
R_squared2 <- 1 - (SS_residual2 / SS_total2)

# Print results
MAE2
RMSE2
R_squared2






## Extrapolation

#south america prediction 
#adult mortality, under five deaths
sa <- data %>% filter(Region_Category == 4) %>% filter(!Year %in% c( 2011, 2012, 2013, 2014, 2015))
sa.2015 <- data %>% filter(Region_Category == 4) %>% filter(Year == 2015)

sa.am.mod <- lm(Adult_mortality ~ Year, data = sa)
sa.am.2015 <- predict(sa.am.mod, newdata = sa.2015); sa.am.2015

sa.ufd.mod <- lm(Under_five_deaths ~ Year, data = sa)
sa.ufd.2015 <- predict(sa.ufd.mod, newdata = sa.2015); sa.ufd.2015

sa.mod1 <- lm(Life_expectancy ~ Adult_mortality + Under_five_deaths, data = sa)
sa.2015.pred.data <- data.frame(
  Adult_mortality = sa.am.2015,
  Under_five_deaths = sa.ufd.2015)



sa.pred.mod1 <- predict(sa.mod1, newdata = sa.2015.pred.data); sa.pred.mod1

sa.mod2 <- lm(Life_expectancy ~ Year, data = sa); summary(sa.mod2)
sa.pred.mod2 <- predict(sa.mod2, newdata = sa.2015); sa.pred.mod2 
#### Performance
## sa.pred.mod1 is from predicting the predictors based on how they changed from 2000-2010, then using those to predict life expectancy
## sa.pred.mod2 is from predicting life expectancy just based on how it changed from 2000-2010

t.test(sa.2015$Life_expectancy, sa.pred.mod1)
t.test(sa.2015$Life_expectancy, sa.pred.mod2)

.mae.sa.1 <- mean(abs(sa.2015$Life_expectancy - sa.pred.mod1)); mae.sa.1
mae.sa.2 <- mean(abs(sa.2015$Life_expectancy - sa.pred.mod2)); mae.sa.2
# Mean Squared Error (MSE)
mse.sa.1 <- mean((sa.2015$Life_expectancy - sa.pred.mod1)^2); mse.sa.1
mse.sa.2 <- mean((sa.2015$Life_expectancy - sa.pred.mod2)^2); mse.sa.2
# Root Mean Squared Error (RMSE)
rmse.sa.1 <- sqrt(mse.sa.1);rmse.sa.1
rmse.sa.2 <- sqrt(mse.sa.2);rmse.sa.2

