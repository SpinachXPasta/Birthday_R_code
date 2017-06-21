library(ggplot2)
library(EDAWR)
library(dplyr)
library(tidyr)
library(gridExtra)
library(lubridate)
library(reshape2)


calender <- read.csv('calender.csv')
names(calender)
calender <- select(calender, Start)
calender <- separate(calender, Start, c("Start","extra"), sep = " ")
calender2 <-select(calender,Start)
calender3 <- separate(calender2, Start, c("month","day","year"), sep = "/" )

qplot(x = month, data = subset(calender3, !is.na(month)), geom = 'bar')calender4 <- select(calender3,month,day)
calender4 <- calender4 %>% group_by(month)
calender4 <- calender4[1:345,1:2]

calender5 <- matrix(calender4$month,ncol = 1, nrow = 365)
calender5 <- melt(calender5)
calender6 <- select(calender5,value,Var2)
calender7 <- calender6 %>% group_by(value) %>% summarise(Var2 = sum(Var2))


birthday <- read.csv('birthdays2.csv')
birthdayA <- select(birthday, Start,Day.of.week)
birthdayA <- separate(birthdayA, Start, c("date","extra"), sep = " ")
birthdayA <- select(birthdayA,date,Day.of.week)
birthdayA <- separate(birthdayA,date,c("month","day","year"), sep = "/")
names(birthdayA)[4] <- "dayname"
birthdayB <- birthdayA[1:345,1:4]


summary(birthdayB$dayname)


lastquestion <- read.csv('birthdays2.csv')
lastquestion <- select(lastquestion, Start,Day.of.week)
lastquestion <- separate(lastquestion, Start, c("date","extra"), sep = " ")
lastquestion <- select(lastquestion,date,Day.of.week)
lastquestion <- separate(lastquestion,date,c("month","day","year"), sep = "/")
lastquestion <- select(lastquestion, month,day,Day.of.week) 
lastquestion <- unite(lastquestion,"date",month,day,sep = "/")
birthdayshare <- subset(lastquestion, date == "02/10")
summary(birthdayshare)

graph <- read.csv('birthdays2.csv')
graph <- select(graph, Start,Day.of.week)
graph <- separate(graph, Start, c("date","extra"), sep = " ")
graph <- select(graph,date,Day.of.week)
graph <- separate(graph,date,c("month","day","year"), sep = "/")
graph <- select(graph, month,day,Day.of.week) 
graph <- graph[1:345,1:3]
qplot(x = month,data = graph)

graph2 <- read.csv('birthdays2.csv')
graph2 <- select(graph2,Start)
graph2 <- separate(graph2, Start, c("date","extra"), sep = " ")
graph2 <- select(graph2,date)
graph2$date <- factor(graph2$date)
str(graph2)


graphc <- separate(graph2,date,c("mnth","day","yr"),sep = "/")
graphl <- bind_cols(graph2,graphc)
graphl <- graphl[1:345,1:4]

qplot(x = as.numeric(day), data = graphl, binwidth = 0.5) + facet_wrap(~mnth,ncol = 2) + scale_x_continuous(breaks = seq(1,31,1)) 












