install.packages("dplyr")   
library(dplyr)  
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("magrittr")   
library(magrittr)
install.packages("plotly")
library(plotly)


#Check Working Directory
getwd()

#Check Dataset File
list.files()

#Dataset Loading
library(readr)

df <- read_csv("urban_noise_levels.csv")
df

#Basic Structure Check
head(df)
glimpse(df)
summary(df)

#Column Rename
df <- df %>% rename(humidity = `humidity_%`)
colnames(df)

#Missing Value Check
colSums(is.na(df))

#Datetime Duplicate Check
df[duplicated(df$datetime), ]

#Remove Duplicate Row
df <- df[!duplicated(df$datetime), ]

#Check after Removing
sum(duplicated(df$datetime))

# Count Of unique value check 
unique_count <- sapply(df, function(x) length(unique(x)))
unique_count

#Extracting month and day from a datetime column
df <- df %>%
  mutate(
    month = month(datetime),
    day   = day(datetime)
  )
head(df[, c("datetime", "month", "day")])

#Data summary check
summary(df)
str(df)

#Correlation Matrix
numeric_cols <- c("decibel_level","temperature_c","humidity","traffic_density","vehicle_count")
cor(df[, numeric_cols])

#Monthly Noise Trend
p <- df %>%
  group_by(month) %>%
  summarise(avg_noise = mean(decibel_level)) %>%
  ggplot(aes(month, avg_noise)) +
  geom_line() +
  geom_point() +
  labs(title="Average Noise Level by Month")

ggplotly(p)

#Daily Noise Trend
library(dplyr)
daily_noise <- df %>%
  group_by(day) %>%
  summarise(avg_noise = mean(decibel_level))
p <- ggplot(daily_noise, aes(x=day, y=avg_noise)) +
  geom_line(color="blue", size=1) +
  geom_point(color="red") +
  labs(title="Daily Noise Trend",
       x="Day of Month",
       y="Average Decibel Level") +
  theme_minimal()
ggplotly(p)


#Hourly Noise Trend
p <- df %>%
  group_by(hour) %>%
  summarise(avg_noise = mean(decibel_level)) %>%
  ggplot(aes(hour, avg_noise)) +
  geom_line() +
  geom_point() +
  labs(title="Average Noise by Hour")
ggplotly(p)


#Traffic Density VS Noise Level
p <- ggplot(df, aes(traffic_density, decibel_level)) +
  geom_point(alpha=0.6) +
  labs(title="Traffic Density vs Noise Level")

ggplotly(p)


#Vehicle Count vs noise level
library(plotly)

p <- ggplot(df, aes(x=vehicle_count, y=decibel_level, text=paste("Vehicle:", vehicle_count, "<br>Noise:", decibel_level))) +
  geom_point(color="darkgreen", alpha=0.5) +
  geom_smooth(method="lm", se=TRUE, color="red") +
  labs(title="Vehicle Count vs Noise Level",
       x="Vehicle Count",
       y="Decibel Level (dB)") +
  theme_minimal()

ggplotly(p)


#Honking Events vs Noise level
library(plotly)

p <- ggplot(df, aes(x=honking_events, y=decibel_level, 
                    text=paste("Honking:", honking_events, "<br>Noise:", decibel_level))) +
  geom_point(color="purple", alpha=0.5) +
  geom_smooth(method="lm", se=TRUE, color="red") +
  labs(title="Honking Events vs Noise Level",
       x="Honking Events",
       y="Decibel Level (dB)") +
  theme_minimal()

ggplotly(p)


#Weekday vs Weekend Noise Levels
library(plotly)

p <- ggplot(df, aes(x=factor(is_weekend), y=decibel_level, 
                    text=paste("Noise:", decibel_level))) +
  geom_boxplot(aes(fill=factor(is_weekend))) +
  scale_fill_manual(values=c("lightgreen","orange"), labels=c("Weekday","Weekend")) +
  labs(title="Weekday vs Weekend Noise Levels", x="Day Type", y="Decibel Level (dB)") +
  theme_minimal()

ggplotly(p)


#Humidity VS Noise level
library(plotly)

p <- ggplot(df, aes(x=humidity, y=decibel_level, 
                    text=paste("Humidity:", humidity, "<br>Noise:", decibel_level))) +
  geom_point(color="blue", alpha=0.5) +
  geom_smooth(method="lm", se=TRUE, color="red") +
  labs(title="Humidity vs Noise Level", x="Humidity (%)", y="Decibel Level (dB)") +
  theme_minimal()

ggplotly(p)


#Temperature VS Noise Level
library(plotly)

p <- ggplot(df, aes(x=temperature_c, y=decibel_level,
                    text=paste("Temp:", temperature_c, "<br>Noise:", decibel_level))) +
  geom_point(color="orange", alpha=0.5) +
  geom_smooth(method="lm", se=TRUE, color="red") +
  labs(title="Temperature vs Noise Level",
       x="Temperature (°C)",
       y="Decibel Level (dB)") +
  theme_minimal()

ggplotly(p)


















