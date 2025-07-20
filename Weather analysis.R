library(tidyverse)
library(sf)
location <- read.csv("C:\\Users\\USER\\Desktop\\2024_Projects\\Project work from Deji\\weather patterns dataset\\Location.csv")
date_taken <- read.csv("C:\\Users\\USER\\Desktop\\2024_Projects\\Project work from Deji\\weather patterns dataset\\date_taken.csv")
Weather <-read.csv("C:\\Users\\USER\\Desktop\\2024_Projects\\Project work from Deji\\weather patterns dataset\\Weather_condition.csv")
Geography <- read.csv("C:\\Users\\USER\\Desktop\\2024_Projects\\Project work from Deji\\weather patterns dataset\\Geography.csv")
Nigeria <- cbind(location,date_taken, Weather
                 ,Geography)
#View(Nigeria)
F.Nigeria <- Nigeria %>% select(
  -c(ID_date_taken, ID_geography, ID_location,
     ID_Weather_condition
  ))
#View(F.Nigeria)
str(F.Nigeria)
dlookr::plot_na_pareto(F.Nigeria)

unique(F.Nigeria$temp == F.Nigeria$temp_min & F.Nigeria$temp == F.Nigeria$temp_max &
         F.Nigeria$temp_min == F.Nigeria$temp_max)
unique(F.Nigeria$pressure == F.Nigeria$sea_level)
unique(F.Nigeria$sunset == F.Nigeria$sunrise)
head(F.Nigeria, 2)
#Exploratory analysis
F.Nigeria_region <- F.Nigeria %>%
  group_by(region) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(F.Nigeria_region, aes(x = region, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Region") +  # Set x-axis label
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14), 
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank()  
  )


F.Nigeria_description <- F.Nigeria %>%
  group_by(description) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(F.Nigeria_description, aes(x = description, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Region") +  # Set x-axis label
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold",color  ="black", size = 14), 
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank()  
  )





# Plot the average population by region with error bars for standard deviation
F.Nigeria <- F.Nigeria %>% select(-c(temp_min, temp_max, sea_level,sunrise, sunset ))
k <-F.Nigeria %>% group_by(region) %>% summarize(avg = mean(population), sD =sd(population),
                                                vaR = var(population), Range = max(population) - min(population))
library(gridExtra)
k <- as.data.frame(k)
a<-ggplot(k, aes(x = region, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Population by Region", x = " ") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )





p <-F.Nigeria %>% group_by(region) %>% summarize(avg = mean(temp), sD =sd(temp),
                                           vaR = var(temp), Range = max(temp) - min(temp))
p <- as.data.frame(p)
wo<-ggplot(p, aes(x = region, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Temperature by Region", x = "Region", y = "Average Population") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )



q <-F.Nigeria %>% group_by(region) %>% summarize(avg = mean(pressure), sD =sd(pressure),
                                           vaR = var(pressure), Range = max(pressure) - min(pressure))

q <- as.data.frame(q)
e <-ggplot(q, aes(x = region, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Pressure by Region", x = "Region", y = "Average Population") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )



y <-v <-F.Nigeria %>% group_by(region) %>% summarize(avg = mean(humidity), sD =sd(humidity),
                                           vaR = var(humidity), Range = max(humidity) - min(humidity))
y <- as.data.frame(y)
r <-ggplot(y, aes(x = region, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Humidity by Region", x = "Region", y = "Average Population") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


t <-F.Nigeria %>% group_by(region) %>% summarize(avg = mean(ground_level), sD =sd(ground_level),
                                           vaR = var(ground_level), Range = max(ground_level) - min(ground_level))

t <- as.data.frame(t)
op <-ggplot(t, aes(x = region, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Ground level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


w <-F.Nigeria %>% group_by(region) %>% summarize(avg = mean(wind_speed),sD =sd(wind_speed),
                                           vaR = var(wind_speed), Range = max(wind_speed) - min(wind_speed))

w <- as.data.frame(w)
vo <-ggplot(w, aes(x = region, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Wind speed level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


d <-F.Nigeria %>% group_by(region) %>% summarize(avg = mean(wind_degree),sD =sd(wind_degree),
                                           vaR = var(wind_degree), Range = max(wind_degree) - min(wind_degree))

d <- as.data.frame(d)
uo <-ggplot(d, aes(x = region, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Wind degree level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

cl <-F.Nigeria %>% group_by(region) %>% summarize(avg = mean(cloud),sD =sd(cloud),
                                           vaR = var(cloud), Range = max(cloud) - min(cloud))

cl <- as.data.frame(cl)
fo <-ggplot(cl, aes(x = region, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Cloud level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


gridExtra::grid.arrange(a,e,vo,op, ncol=2)
gridExtra::grid.arrange(wo,uo,fo,r, ncol=2)

# Description
dl <- F.Nigeria %>% group_by(description) %>% summarize(avg = mean(population), sD =sd(population),
                                             vaR = var(population), Range = max(population) - min(population))

dl <- as.data.frame(dl)
ggplot(dl, aes(x = description, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Population level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

d2 <-F.Nigeria %>% group_by(description) %>% summarize(avg = mean(temp), sD =sd(temp),
                                             vaR = var(temp), Range = max(temp) - min(temp))
d2 <- as.data.frame(d2)
ggplot(d2, aes(x = description, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Temperature level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

d3 <-F.Nigeria %>% group_by(description) %>% summarize(avg = mean(pressure), sD =sd(pressure),
                                             vaR = var(pressure), Range = max(pressure) - min(pressure))
d3 <- as.data.frame(d3)
ggplot(d3, aes(x = description, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Pressure level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


d4 <-F.Nigeria %>% group_by(description) %>% summarize(avg = mean(humidity), sD =sd(humidity),
                                             vaR = var(humidity), Range = max(humidity) - min(humidity))
d4 <- as.data.frame(d4)
ggplot(d4, aes(x = description, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Pressure level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


d5 <-F.Nigeria %>% group_by(description) %>% summarize(avg = mean(ground_level), sD =sd(ground_level),
                                             vaR = var(ground_level), Range = max(ground_level) - min(ground_level))
d5 <- as.data.frame(d5)
ggplot(d5, aes(x = description, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Pressure level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
d6 <-F.Nigeria %>% group_by(description) %>% summarize(avg = mean(wind_speed),sD =sd(wind_speed),
                                             vaR = var(wind_speed), Range = max(wind_speed) - min(wind_speed))
d6 <- as.data.frame(d6)
ggplot(d6, aes(x = description, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Pressure level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

d7 <-F.Nigeria %>% group_by(description) %>% summarize(avg = mean(wind_degree),sD =sd(wind_degree),
                                             vaR = var(wind_degree), Range = max(wind_degree) - min(wind_degree))
d7 <- as.data.frame(d7)
ggplot(d7, aes(x = description, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Pressure level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

d8 <-F.Nigeria %>% group_by(description) %>% summarize(avg = mean(cloud),sD =sd(cloud),
                                             vaR = var(cloud), Range = max(cloud) - min(cloud))
d8 <- as.data.frame(d8)
ggplot(d8, aes(x = description, y = avg)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_errorbar(aes(ymin = avg - sD, ymax = avg + sD), width = 0.2) +
  labs(title = "Average Pressure level by Region") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
# Desriptive statict of the continous data
library(summarytools)
dfSummary(F.Nigeria %>% select(-c(city, region, description, date_taken)))
k <-F.Nigeria %>% select(-c(timezone,longitude,latitude,city, region, description, date_taken, country))
cor(k)
corrplot(cor(k), method = "number", col = "black")  


#install.packages("ggmap")
library(ggmap)
library(sf)
#install.packages("leaflet")
library(leaflet)

leaflet(data = F.Nigeria) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = ~sqrt(population) / 1000, 
    color = ~factor(region),
    popup = ~paste(region, "<br>Population:", population),
    fillOpacity = 0.7
  ) %>%
  addLegend(
    "bottomright",
    pal = colorFactor(rainbow(length(unique(F.Nigeria$region))), F.Nigeria$region),
    values = ~region,
    title = "Region",
    opacity = 1
  )

library(mapview)


data_sf <- st_as_sf(F.Nigeria, coords = c("longitude", "latitude"), crs = 4326)


mapview(data_sf, zcol = "region", legend = TRUE)
mapview(data_sf, zcol = "description")
mapview(data_sf, zcol = "temp")
mapview(data_sf, zcol = "pressure")
mapview(data_sf, zcol ="humidity")
mapview(data_sf, zcol = "wind_speed")
mapview(data_sf, zcol ="wind_degree")
mapview(data_sf, zcol = "cloud")
mapview(data_sf, zcol = "ground_level")
mapview(data_sf, zcol = "population")


library(dlookr)
diagnose_outlier(F.Nigeria)
#plot_outlier(F.Nigeria)
plot.ts(F.Nigeria$wind_speed, col ="red")
plot.ts(F.Nigeria$temp, col ="red")
plot.ts(F.Nigeria$pressure, col ="red")
plot.ts(F.Nigeria$humidity, col ="red")
plot.ts(F.Nigeria$ground_level, col ="red")
plot.ts(F.Nigeria$wind_degree, col ="red")
plot.ts(F.Nigeria$cloud, col ="red")
plot.ts(F.Nigeria$population, col ="red")

class(F.Nigeria$date_taken)
F.Nigeria$date_taken <- as.Date(F.Nigeria$date_taken)

library(caret)
library(caTools)
library(purrr)

F.Nigeria$weather_severity <- ifelse(F.Nigeria$temp > 313.15, 'Severe',
      ifelse(F.Nigeria$pressure < 1000, 'Severe',
    ifelse(F.Nigeria$humidity > 90 | F.Nigeria$humidity < 20, 'Severe',
    ifelse(F.Nigeria$wind_speed > 10, 'Severe',
  ifelse(F.Nigeria$cloud > 80 & grepl('storm|thunderstorm|heavy rain', tolower(F.Nigeria$description)), 'Severe', 'Non-Severe')))))


data_sf_sv <- st_as_sf(F.Nigeria, coords = c("longitude", "latitude"), crs = 4326)
mapview(data_sf_sv, zcol = "weather_severity", legend = TRUE)

unique(F.Nigeria$weather_severity)
#dfSummary (F.Nigeria$weather_severity)
F.Nigeria_T <- F.Nigeria %>%
  group_by(weather_severity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(F.Nigeria_T, aes(x = weather_severity, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Weather_severity") +  # Set x-axis label
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14), 
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank()  
  )

#install.packages("ROSE")
library(ROSE)

# Combination of oversampling and undersampling
balanced_data <- ovun.sample(weather_severity ~ ., data = F.Nigeria, method = "both", p = 0.5, N = 22541, seed = 1)

balanced_data <- data.frame(balanced_data$data)
unique(balanced_data$weather_severity)
dfSummary(balanced_data$weather_severity)
F.Nigeria_B <- balanced_data %>%
  group_by(weather_severity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(F.Nigeria_B, aes(x = weather_severity, y = percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Weather_severity") +  # Set x-axis label
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", colour = "black", size = 14), 
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank()  
  )
#MAP OF THE BALANCED DATASET
data_sf_balanced <- st_as_sf(balanced_data, coords = c("longitude", "latitude"), crs = 4326)
mapview(data_sf_balanced, zcol = "weather_severity", legend = TRUE)

# Data Normilization
normalize_min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

balanced_data_N <- balanced_data %>% mutate( population = normalize_min_max(population),   temp = normalize_min_max(temp), pressure = normalize_min_max(pressure),
humidity = normalize_min_max(humidity),
  ground_level = normalize_min_max(ground_level), wind_speed = normalize_min_max(wind_speed), wind_degree = normalize_min_max(wind_degree), cloud = normalize_min_max(wind_degree),
Latitude = normalize_min_max(latitude), Longitude = normalize_min_max(longitude))

balanced_data_N$weather_severity <- as.factor(ifelse(balanced_data_N$weather_severity == "Severe",1,0))
balanced_data_N <- balanced_data_N %>% select(-c(description,
      country,city, region, timezone, date_taken))
s <- sample.split(balanced_data_N$weather_severity, SplitRatio = 0.9)
Train_p <- subset(balanced_data_N, s == T)
Test_p <- subset( balanced_data_N, s== F)

Train <- Train_p%>%select(-c(latitude,longitude))
Test <- Test_p%>%select(-c(latitude,longitude))

md <- glm(weather_severity~., data = Train, family = "binomial")
#Prediction using Train
predictions <- predict(md, newdata = Train, type = "response")
predictions <- ifelse(predictions > 0.5, 1, 0)
log_cm1 <-confusionMatrix(as.factor(predictions), as.factor(Train$weather_severity))
#Prediction using Test
prediction <- predict(md, newdata = Test, type = "response")
prediction <- ifelse(prediction > 0.5, 1, 0)
log_cm2 <-confusionMatrix(as.factor(prediction), as.factor(Test$weather_severity))

#RIGOROUS EVALUATION
pdP <- cbind((as.data.frame(prediction))  , Test_p)
colnames(pdP)[1] <- "weather_severity"
pdP$weather_severity <- ifelse(pdP$weather_severity == 1,"Severe", "Non-Severe")
logistic_Reg <- st_as_sf(pdP, coords = c("longitude", "latitude"), crs = 4326)
mapview(logistic_Reg, zcol = "weather_severity", legend = TRUE)

pdP1 <- cbind((as.data.frame(predictions))  , Train_p)
colnames(pdP1)[1] <- "weather_severity"
pdP1$weather_severity <- ifelse(pdP1$weather_severity == 1,"Severe", "Non-Severe")
logistic_Reg2 <- st_as_sf(pdP1, coords = c("longitude", "latitude"), crs = 4326)
mapview(logistic_Reg2, zcol = "weather_severity", legend = TRUE)


#RIDGE REGRESSION
library(randomForest)
# Train the model
set.seed(123)  # for reproducibility
rf_model <- randomForest(weather_severity ~ ., data = Train, ntree = 500, importance = TRUE)
# Predict on training data
train_pred <- predict(rf_model, newdata = Train)
rf_cm1 <-confusionMatrix(as.factor(train_pred), as.factor(Train$weather_severity))
pdPRd <- cbind((as.data.frame(train_pred))  , Train_p)
colnames(pdPRd)[1] <- "weather_severity"
pdPRd$weather_severity <- ifelse(pdPRd$weather_severity == 1,"Severe", "Non-Severe")
randomForest_1<- st_as_sf(pdPRd, coords = c("longitude", "latitude"), crs = 4326)
mapview(randomForest_1, zcol = "weather_severity", legend = TRUE)

# Predict on test data
test_pred <- predict(rf_model, newdata = Test)
rf_cm2 <-confusionMatrix(as.factor(test_pred), as.factor(Test$weather_severity))
pdP1Rd <- cbind((as.data.frame(test_pred))  , Test_p)
colnames(pdP1Rd)[1] <- "weather_severity"
pdP1Rd$weather_severity <- ifelse(pdP1Rd$weather_severity == 1,"Severe", "Non-Severe")
randomForest_2 <- st_as_sf(pdP1Rd, coords = c("longitude", "latitude"), crs = 4326)
mapview(randomForest_2, zcol = "weather_severity", legend = TRUE)

library(nnet)
nn_model <- nnet(weather_severity ~ ., data = Train, size = 5, decay = 0.01, maxit = 500, trace = FALSE)
train_pred_prob <- predict(nn_model, newdata = Train, type = "class")%>%as.factor()
nn_cm1 <-confusionMatrix(train_pred_prob, Train$weather_severity)
test_pred_prob <- predict(nn_model, newdata = Test, type = "class")%>%as.factor()
nn_cm2 <-confusionMatrix(test_pred_prob, Test$weather_severity)

pdPnn <- cbind((as.data.frame(train_pred_prob))  , Train_p)
colnames(pdPnn)[1] <- "weather_severity"
pdPnn$weather_severity <- ifelse(pdPnn$weather_severity == 1,"Severe", "Non-Severe")
Neural_network_1 <- st_as_sf(pdPnn, coords = c("longitude", "latitude"), crs = 4326)
mapview(Neural_network_1, zcol = "weather_severity", legend = TRUE)

pdP1nn <- cbind((as.data.frame(test_pred_prob))  , Test_p)
colnames(pdP1nn)[1] <- "weather_severity"
pdP1nn$weather_severity <- ifelse(pdP1nn$weather_severity == 1,"Severe", "Non-Severe")
Neural_network_2 <- st_as_sf(pdP1nn, coords = c("longitude", "latitude"), crs = 4326)
mapview(Neural_network_2, zcol = "weather_severity", legend = TRUE)

#ALL 
ALL_pred_prob <- predict(nn_model, newdata = balanced_data_N, type = "class")%>%as.factor()
nn_cm3 <-confusionMatrix(ALL_pred_prob, as.factor(balanced_data_N$weather_severity))
pdPRA <- cbind((as.data.frame(ALL_pred_prob))  , balanced_data_N)
colnames(pdPRA)[1] <- "weather_severity"
pdPRA$weather_severity <- ifelse(pdPRA$weather_severity == 1,"Severe", "Non-Severe")
Neural_network_3 <- st_as_sf(pdPRA, coords = c("longitude", "latitude"), crs = 4326)
mapview(Neural_network_3, zcol = "weather_severity", legend = TRUE)


ALL_pred_R <- predict(rf_model, newdata = balanced_data_N)
rf_cm3 <-confusionMatrix(as.factor(ALL_pred_R), as.factor(balanced_data_N$weather_severity))
pdPRA2 <- cbind((as.data.frame(ALL_pred_R))  , balanced_data_N)
colnames(pdPRA2)[1] <- "weather_severity"
pdPRA2$weather_severity <- ifelse(pdPRA2$weather_severity == 1,"Severe", "Non-Severe")
randomForest_3 <- st_as_sf(pdPRA2, coords = c("longitude", "latitude"), crs = 4326)
mapview(randomForest_3, zcol = "weather_severity", legend = TRUE)

predictionsA_reduced <- predict(md, newdata = balanced_data_N, type = "response")
predictionsA_reduced <- ifelse(predictionsA_reduced > 0.5, 1, 0)
log_cm3 <-confusionMatrix(as.factor(predictionsA_reduced), balanced_data_N$weather_severity)
pdPRA3 <- cbind((as.data.frame(predictionsA_reduced))  , balanced_data_N)
colnames(pdPRA3)[1] <- "weather_severity"
pdPRA3$weather_severity <- ifelse(pdPRA3$weather_severity == 1,"Severe", "Non-Severe")
logistic_Reg3 <- st_as_sf(pdPRA3, coords = c("longitude", "latitude"), crs = 4326)
mapview(logistic_Reg3, zcol = "weather_severity", legend = TRUE)

#PERFORMANCE ANALYSIS
# Function to extract key metrics
extract_metrics <- function(cm, model_name) {
  data.frame(
    Model = model_name,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    Precision = cm$byClass["Pos Pred Value"],
    Recall = cm$byClass["Sensitivity"],
    F1 = 2 * (cm$byClass["Sensitivity"] * cm$byClass["Pos Pred Value"]) /
      (cm$byClass["Sensitivity"] + cm$byClass["Pos Pred Value"])
  )
}

# Combine all
metrics_df1 <- rbind(
  extract_metrics(nn_cm1, "Neural Network"),
  extract_metrics(rf_cm1, "Random Forest"),
  extract_metrics(log_cm1, "Logistic Regression")
)

metrics_df2 <- rbind(
  extract_metrics(nn_cm2, "Neural Network"),
  extract_metrics(rf_cm2, "Random Forest"),
  extract_metrics(log_cm2, "Logistic Regression")
)

metrics_df3 <- rbind(
  extract_metrics(nn_cm3, "Neural Network"),
  extract_metrics(rf_cm3, "Random Forest"),
  extract_metrics(log_cm3, "Logistic Regression")
)

metrics_long1 <- metrics_df1 %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

metrics_long2 <- metrics_df2 %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

metrics_long3 <- metrics_df3 %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")


ggplot(metrics_long1, aes(x = Metric, y = Value, group = Model, color = Model)) +
  geom_line(size = 1.5) +  
  geom_point(size = 4, shape = 21, fill = "white") +  
  geom_text(aes(label = round(Value, 2)), vjust = -0.8, size = 4.2, fontface = "bold") +  
  scale_color_brewer(palette = "Dark2") +  
  labs(
    title = NULL,
    y = NULL,
    x = NULL  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color="black"),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold"),
    legend.position = "bottom",              
    legend.box = "horizontal",               
    axis.title.y = element_text(face = "bold"),
    panel.border = element_blank()
  )


ggplot(metrics_long2, aes(x = Metric, y = Value, group = Model, color = Model)) +
  geom_line(size = 1.5) +  
  geom_point(size = 4, shape = 21, fill = "white") +  
  geom_text(aes(label = round(Value, 2)), vjust = -0.8, size = 4.2, fontface = "bold") +  
  scale_color_brewer(palette = "Dark2") +  
  labs(
    title = NULL,
    y = NULL,
    x = NULL  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color="black"),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold"),
    legend.position = "bottom",              
    legend.box = "horizontal",               
    axis.title.y = element_text(face = "bold"),
    panel.border = element_blank()
  )

ggplot(metrics_long3, aes(x = Metric, y = Value, group = Model, color = Model)) +
  geom_line(size = 1.5) +  
  geom_point(size = 4, shape = 21, fill = "white") +  
  geom_text(aes(label = round(Value, 2)), vjust = -0.8, size = 4.2, fontface = "bold") +  
  scale_color_brewer(palette = "Dark2") + 
  labs(
    title = NULL,
    y = NULL,
    x = NULL  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color="black"),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold"),
    legend.position = "bottom",              
    legend.box = "horizontal",               
    axis.title.y = element_text(face = "bold"),
    panel.border = element_blank()
  )


#Confusion matrix
log_cm1_df <- as.data.frame(log_cm1$table)
colnames(log_cm1_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = log_cm1_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "Logist Confusion Matrix on Train data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

log_cm2_df <- as.data.frame(log_cm2$table)
colnames(log_cm2_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = log_cm2_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "Logist Confusion Matrix on Test data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

log_cm3_df <- as.data.frame(log_cm3$table)
colnames(log_cm3_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = log_cm3_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "Logist Confusion Matrix on All data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))



rf_cm1_df <- as.data.frame(rf_cm1$table)
colnames(rf_cm1_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = rf_cm1_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "RandomF Confusion Matrix on Training data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))


rf_cm2_df <- as.data.frame(rf_cm2$table)
colnames(rf_cm2_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = rf_cm2_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "RandomF Confusion Matrix on Test data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

rf_cm3_df <- as.data.frame(rf_cm3$table)
colnames(rf_cm3_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = rf_cm3_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "RandomF Confusion Matrix on All data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))


nn_cm1_df <- as.data.frame(nn_cm1$table)
colnames(nn_cm1_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = nn_cm1_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "NeuralN Confusion Matrix on Training data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))


nn_cm2_df <- as.data.frame(nn_cm2$table)
colnames(nn_cm2_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = nn_cm2_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "NeuralN Confusion Matrix on Test data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))


nn_cm3_df <- as.data.frame(nn_cm3$table)
colnames(nn_cm3_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = nn_cm3_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  scale_fill_gradient(low = "#d3e5ff", high = "#08306b") +
  labs(title = "NeuralN Confusion Matrix on All data", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))



ALL_pred_prob <- predict(nn_model, newdata = balanced_data_N, type = "class")%>%as.factor()
nn_cm3 <-confusionMatrix(ALL_pred_prob, as.factor(balanced_data_N$weather_severity))
pdPRA <- cbind((as.data.frame(ALL_pred_prob))  , balanced_data_N)
colnames(pdPRA)[1] <- "weather_severity"
pdPRA$weather_severity <- ifelse(pdPRA$weather_severity == 1,"Severe", "Non-Severe")
Neural_network_3 <- st_as_sf(pdPRA, coords = c("longitude", "latitude"), crs = 4326)
mapview(Neural_network_3, zcol = "weather_severity", legend = TRUE)


ALL_pred_R <- predict(rf_model, newdata = balanced_data_N)
rf_cm3 <-confusionMatrix(as.factor(ALL_pred_R), as.factor(balanced_data_N$weather_severity))
pdPRA2 <- cbind((as.data.frame(ALL_pred_R))  , balanced_data_N)
colnames(pdPRA2)[1] <- "weather_severity"
pdPRA2$weather_severity <- ifelse(pdPRA2$weather_severity == 1,"Severe", "Non-Severe")
randomForest_3 <- st_as_sf(pdPRA2, coords = c("longitude", "latitude"), crs = 4326)
mapview(randomForest_3, zcol = "weather_severity", legend = TRUE)

predictionsA_reduced <- predict(md, newdata = balanced_data_N, type = "response")
predictionsA_reduced <- ifelse(predictionsA_reduced > 0.5, 1, 0)
log_cm3 <-confusionMatrix(as.factor(predictionsA_reduced), balanced_data_N$weather_severity)
pdPRA3 <- cbind((as.data.frame(predictionsA_reduced))  , balanced_data_N)
colnames(pdPRA3)[1] <- "weather_severity"
pdPRA3$weather_severity <- ifelse(pdPRA3$weather_severity == 1,"Severe", "Non-Severe")
logistic_Reg3 <- st_as_sf(pdPRA3, coords = c("longitude", "latitude"), crs = 4326)
mapview(logistic_Reg3, zcol = "weather_severity", legend = TRUE)


