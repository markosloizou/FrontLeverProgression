pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, rio, rmarkdown,
               shiny, stringr, tidyr)

library(dplyr)
library(plyr)
setwd("~/stats/FrontLever/")

data = read.csv("~/stats/FrontLever/Data/cleanedDataNoComment.csv")
data <-  data[-1] #drop unique id
while(max(data$height) > 1000) {
  data$height[data$height > 1000] <- data$height[data$height > 1000]/10
}

data$weight[data$weight > 110] <- data$height[data$weight > 110]*0.45


data$FLprogression <- as.character(data$FLprogression)
data$FLprogression <- factor(data$FLprogression, levels=c("No tuck lever","Tuck lever", "Advanced Tuck lever", "Straddle Halflay lever", "Straddle lever", "Full lever"))
data$pullup <- as.character(data$pullup)
data$pullup <- factor(data$pullup, levels=c("<10% (so usually couple of pullups with BW)", "10%-30%", "30%-50%", "50%-65%", "65%-80%", "80%-90%",">90%"))
revalue(data$pullup, c("<10% (so usually couple of pullups with BW)" = "<10%")) -> data$pullup

png(filename="./plots/data.png")
plot(data)
dev.off()

ggplot(data, aes(x=FLprogression)) +
  theme_bw() +
  geom_bar() +
  labs(y="Count", title = "Progression")

ggplot(data, aes(x=FLprogression, fill=pullup)) +
  theme_bw() +
  geom_bar() +
  labs(y="Count", title = "Progression")

ggplot(data, aes(x = height, fill=FLprogression)) + 
  theme_bw()+
  geom_histogram(binwidth = 5) +
  labs(y = "Count", x = "Height (binwidth = 5)", title="Progresion by Heigth")

ggplot(data, aes(x = weight, fill=FLprogression)) + 
  theme_bw()+
  geom_histogram(binwidth = 5) +
  labs(y = "Count", x = "Height (binwidth = 5)", title="Progresion by Weight")


ggplot(data, aes(x = weight, fill=FLprogression)) + 
  theme_bw()+
  facet_wrap( ~ pullup)+
  geom_density(alpha = 0.5) +
  labs(y = "Count", x = "Weight", title="Progresion by Max Pullup aand weigth")

ggplot(data, aes(x = height, fill=FLprogression)) + 
  theme_bw()+
  facet_wrap( ~ pullup)+
  geom_density(alpha = 0.5) +
  labs(y = "Percentage", x = "Height", title="Progresion by Max Pullup aand heigth")

ggplot(data, aes(x = height,y=weight, color=FLprogression, lwd=200)) + 
  theme_bw()+
  facet_wrap( ~ pullup)+
  geom_point(shape=1, size=3) +
  labs(y = "Height", x = "Weight", title="Progresion by Max Pullup aand heigth")


png(filename="./plots/height_hist.png")
hist(data$height, xlab="Height (cm)", main="")
dev.off()

png(filename="./plots/weight_hist.png")
hist(data$weight, xlab="Wight (kg)", main="")
dev.off()

png(filename="./plots/bmi_hist.png")
hist(data$weight/(data$height/100)^2, breaks=15, xlab="BMI", main="")
dev.off()

ggplot(data, aes(x=Npullups, fill=FLprogression))  +
  theme_bw() +
  geom_bar()


ggplot(data, aes(x=Npullups, y=FLprogression)) +
  geom_jitter(width=0.5, height=0.5) 

png(filename = "./plots/n_vs_masx.png")
ggplot(data, aes(x=Npullups, y=pullup)) + 
  geom_jitter(width=0.5, height=0.5) +
  labs(x="Number of Pullups", y="Max pullup", caption="Points jittered")
dev.off()

png(filename = "./plots/max_vs_flprog.png")
ggplot(data, aes(x=pullup, y=FLprogression)) +
  geom_jitter(width=0.5, height=0.5) +
  labs(y='',x='Max Pullup',caption='All points are jittered by 0.5 vertically and horizontally')
dev.off()

png(filename = "./plots/pullus_vs_flprog.png")
ggplot(data, aes(x=Npullups, y=FLprogression)) + 
  geom_jitter(width=0.5, height=0.5) +
  labs(y='',x='Number of Pullups',caption='All points are jittered by 0.5 vertically and horizontally')
dev.off()

png(filename = "./plots/NummberOfPullpsVsMax.png")
ggplot(data,aes(x=Npullups, y=pullup)) + 
  facet_wrap(~FLprogression) + 
  geom_jitter(width=0.5, height=0.5) +
  labs(y='', x='Number of Pullups',caption='All points are jittered by 0.5 vertically and horizontally', title="Max Pullup Strength vs Number of Pullups Split by FL progression")
dev.off()

png(filename = "./plots/FLprogressionByNAndMax.png")
ggplot(data,aes(x=Npullups, y=FLprogression)) + 
  facet_wrap(~pullup) + 
  geom_jitter(width=0.5, height=0.5) +
  labs(y='', x='Number of Pullups',caption='All points are jittered by 0.5 vertically and horizontally', title="Front Lever progression vs number of pullups split by max pullup strength")
dev.off()    
