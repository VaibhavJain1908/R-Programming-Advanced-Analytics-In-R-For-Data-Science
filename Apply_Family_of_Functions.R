#Read Data:
Chicago <- read.csv("Chicago-F.csv", row.names = 1)
NewYork <- read.csv("NewYork-F.csv", row.names = 1)
Houston <- read.csv("Houston-F.csv", row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-F.csv", row.names = 1)
#Check:
Chicago
NewYork
Houston
SanFrancisco
#these are dataFrames:
is.data.frame(Chicago)
#let's convert to matrices:
Chicago <- as.matrix(Chicago)
NewYork <- as.matrix(NewYork)
SanFrancisco <- as.matrix(SanFrancisco)
Houston <- as.matrix(Houston)
#Let's put all of these into a list:
Weather <- list(Chicago = Chicago, NewYork = NewYork, Houston = Houston, SanFrancisco = SanFrancisco)
Weather
#let's try it out:
Weather[3]
Weather[[3]]
Weather$Houston

#Using apply():
?apply
Chicago
apply(Chicago, 1, mean)
#check:
mean(Chicago["DaysWithPrecip",])
#analyze one city:
Chicago
apply(Chicago, 1, max)
apply(Chicago, 1, min)
#for practice:
apply(Chicago, 2, max) #doesn't make much sense, but good exercise
apply(Chicago, 2, min)
#Compare:
apply(Chicago, 1, mean)
apply(NewYork, 1, mean)
apply(SanFrancisco, 1, mean)
apply(Houston, 1, mean)
                        #<<< (nearly) deliv1 : but there is a faster way


#Recreating the apply function with loops (advanced topic)
Chicago
#find the mean of every row:
#1. via loops
output <- NULL #preparing an empty vector
for (i in 1:5) { #run cycle
  output[i] <- mean(Chicago[i,])
1}
output            #let's see what we have
names(output) <- rownames(Chicago)
output
#2. via apply function
apply(Chicago, 1, mean)

#Using lapply():
?lapply()
Chicago
t(Chicago)
Weather
lapply(Weather, t)  #t(Weather$Chicago), t(Weather$NewYork), t(Weather$Houston),t(Weather$SanFrancisco)
mynewlist <- lapply(Weather, t)
mynewlist
#example 2:
Chicago
rbind(Chicago, NewRow = 1:12)
lapply(Weather, rbind, NewRow = 1:12)
#example 3:
?rowMeans
rowMeans(Chicago) #identical to: apply(Chicago, 1, mean)
lapply(Weather, rowMeans)
                   #<<< (nearly) deliv1 : even better, but willimprove further
#rowMeans
#colMeans
#rowSums
#colSums

#Combininglapply with the [] operator
Weather
Weather[[1]][1,1]     #Weather[[1]][1,1], Weather[[2]][1,1], ...
Weather$Chicago[1,1]  #Weather$Chicago[1,1], Weather$NewYork[1,1], ...
lapply(Weather,"[", 1, 1)
Weather
lapply(Weather, "[", 1,)
Weather
lapply(Weather,"[", , 3)

#Adding your own functions
lapply(Weather, rowMeans)
lapply(Weather, function(x) x[1,])
lapply(Weather, function(x) x[5,])
lapply(Weather, function(x) x[,12])
Weather
lapply(Weather, function(z) z[1,]-z[2,])
lapply(Weather, function(z) round((z[1,]-z[2,])/z[2,],2))
                                       #<<< Deliv2: temp fluctuations.Will improve

#Using sapply()
?sapply
Weather
#AvgHigh_F for July:
lapply(Weather, "[", 1, 7)
sapply(Weather, "[", 1, 7)
#AvgHigh for 4th quarter:
lapply(Weather, "[", 1, 10:12)
sapply(Weather, "[", 1, 10:12)
#Another example:
lapply(Weather, rowMeans)
sapply(Weather, rowMeans)
round(sapply(Weather, rowMeans), 2) #<<< Deliv1 1. Awesome!
#Another example:
lapply(Weather, function(z) round((z[1,]-z[2,])/z[2,],2))
sapply(Weather, function(z) round((z[1,]-z[2,])/z[2,],2))  #<<< Deliv1 2. Awesome!
#By the way:
sapply(Weather, rowMeans, simplify = FALSE, USE.NAMES = FALSE) #same as lapply

#Nesting Apply Functions:
Weather
lapply(Weather, rowMeans)
?rowMeans
Chicago
apply(Chicago, 1, max)
#apply across whole list:
lapply(Weather, apply, 1, max) #preferred approach
lapply(Weather, function(x) apply(x, 1, max))
#tidy up:
sapply(Weather, apply, 1, max) #<<< deliv 3.
sapply(Weather, apply, 1, min) #<<< deliv 4.

#Very advanced tutorial:
#which.max 
?which.max
which.max(Chicago[1,])
names(which.max(Chicago[1,]))
#by the sound of it:
#We will have: apply - to iterate over rows of the matrix
#and we will have: lapply or sapply - to iterate over components of the list
apply(Chicago, 1, function(x) names(which.max(x)))
lapply(Weather, function(y) apply(y, 1, function(x) names(which.max(x))))
sapply(Weather, function(y) apply(y, 1, function(x) names(which.max(x))))

# ------------------------------------------------------- 