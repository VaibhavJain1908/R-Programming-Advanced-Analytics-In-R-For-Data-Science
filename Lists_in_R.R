util <- read.csv("P3-Machine-Utilization.csv")
head(util,12)
str(util)
summary(util)

#Derive utilization column:
util$Utilization <- 1 - util$Percent.Idle
head(util, 12)

#Handling Date-Times in R:
tail(util)
?POSIXct
util$PosixTime <- as.POSIXct(util$Timestamp, format = "%d/%m/%Y %H:%M")
head(util,12)
summary(util)
#TIP: how to rearrange columns in a df:
util$Timestamp <- NULL
head(util, 12)
util <- util[,c(4,1,2,3)]
head(util, 12)

#What is a list?
summary(util)
RL1 <- util[util$Machine == "RL1",]
summary(RL1)
RL1$Machine <- factor(RL1$Machine)
summary(RL1)
#Construct list:
#Character: Machine name
#Vector:    (min, max, mean) Utilization for the month (excluding unknown hours)
#Logical:   Has utilization ever fallen below 90% ? TRUE/FALSE
util_stats_rl1 <- c(min(RL1$Utilization, na.rm=T),
                    mean(RL1$Utilization, na.rm=T),
                    max(RL1$Utilization, na.rm=T))
util_stats_rl1
util_under_90_flag <- length(which(RL1$Utilization < 0.90)) > 0 #as.logical() - alternative to this
util_under_90_flag
list_rl1 <- list("RL1", util_stats_rl1, util_under_90_flag)
list_rl1

#Naming components of a list:
list_rl1
names(list_rl1)
names(list_rl1) <- c("Machine", "Stats", "LowThreshold")
list_rl1
#Another way. Like with dataframes:
rm(list_rl1)
list_rl1
list_rl1 <- list(Machine="RL1", Stats=util_stats_rl1, LowThreshold=util_under_90_flag)
list_rl1

#Extracting components of a list
#three ways:
#[] - will always return a list
#[[]] - will always return the actualobject
#$ - same as [[]] but prettier
list_rl1
list_rl1[1]
list_rl1[[1]]
list_rl1$Machine

list_rl1[2]
typeof(list_rl1[2])
list_rl1[[2]]
typeof(list_rl1[[2]])
list_rl1$Stats
typeof(list_rl1$Stats)

#how would you acces the 3rd element of the vector (max utilization)?
list_rl1
list_rl1[[2]][3]
list_rl1$Stats[3]

#Adding and deleting list components
list_rl1
list_rl1[4] <- "New Information"
list_rl1
#Another way to add a component - via the $
#We will add:
#Vector:  All hours where utilization is unknown (NA's)
list_rl1$UnknownHours <- RL1[is.na(RL1$Utilization), "PosixTime"]
list_rl1
#Remove a component. Use the NULL method:
list_rl1[5] <- NULL
list_rl1
#!Notice: numeration has shifted:
list_rl1[4]
#Add another component:
#DataFrame: For this Mahine
list_rl1$Data <- RL1
list_rl1
summary(list_rl1)
str(list_rl1)

#Subsetting a list:
list_rl1
#Quick challenge:
list_rl1$UnknownHours[1]
list_rl1[[4]][1]
#Let's proceed...
list_rl1
list_rl1[1:3]
list_rl1[c(1,4)]
sublist_rl1 <- list_rl1[c("Machine", "Stats")]
sublist_rl1[[2]][2]
sublist_rl1$Stats[2]
#Double Square Brackets Are NOT For Subsetting:
#list_rl1[[1:3]] #ERROR

#Building a timeseries plot:
library(ggplot2)
p <- ggplot(data = util)
p + geom_line(aes(x = PosixTime, y = Utilization,
                  color = Machine), size = 1.2) +
  facet_grid(Machine~.) +
  geom_hline(yintercept = 0.90,
             color = "Gray", size = 1.2,
             linetype = 3)

myplot <- p + geom_line(aes(x = PosixTime, y = Utilization,
                            color = Machine), size = 1.2) +
  facet_grid(Machine~.) +
  geom_hline(yintercept = 0.90,
             color = "Gray", size = 1.2,
             linetype = 3)

list_rl1$Plot <- myplot
list_rl1
summary(list_rl1)
str(list_rl1)

list_rl1

#----------------------------------------------