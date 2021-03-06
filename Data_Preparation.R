#Basic: fin <- read.csv("P3-Future-500-The-Dataset.csv")
fin <- read.csv("P3-Future-500-The-Dataset.csv", na.strings = c(""))
fin
head(fin)
tail(fin, 10)
str(fin)
summary(fin)

# Changing From Non-Factor to Factor:

fin$ID = factor(fin$ID)
summary(fin)
str(fin)

fin$Inception = factor(fin$Inception)
summary(fin)
str(fin)

# Factor Variable Trap (FVT)
# Converting into Numerics for characters:
a <- c('12','13','14','12','12')
a
typeof(a)
b <- as.numeric(a)
b
typeof(b)
# Converting into Numerics for Factors:
z <- factor(c('12','13','14','12','12'))
z
y <- as.numeric(z)
y
#------- Correct way:
x <- as.numeric(as.character(z))
x
typeof(x)

# FVT Example
head(fin)
str(fin)
#fin$Profit = factor(fin$Profit) #Dangerous

head(fin)
str(fin)

summary(fin)
#fin$Profit <- as.numeric(fin$Profit) #Dangerous
str(fin)
head(fin)

#sub() and gsub()
fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)
head(fin)
str(fin)

fin$Revenue <- gsub("\\$", "", fin$Revenue)
fin$Revenue <- gsub(",", "", fin$Revenue)
head(fin)
str(fin)

fin$Growth <- gsub("%", "", fin$Growth)
head(fin)
str(fin)

fin$Revenue <- as.numeric(fin$Revenue)
fin$Expenses <- as.numeric(fin$Expenses)
fin$Growth <- as.numeric(fin$Growth)
str(fin)
summary(fin)

# Locating the missing data
#Updated import to: fin <- read.csv("P3-Future-500-The-Dataset.csv", na.strings = c(""))

head(fin)
fin[!complete.cases(fin),]
str(fin)

# Filtering: using which() for non-missing data
head(fin)
fin[fin$Revenue == 9746272,]
which(fin$Revenue == 9746272)
?which
fin[which(fin$Revenue == 9746272),]

head(fin)
fin[fin$Employees == 45,]
fin[which(fin$Employees == 45),]

# Filtering: using is.na() for missing data
head(fin, 24)

fin$Expenses == NA
fin[fin$Expenses == NA,]
is.na()

a <- c(1,24,543,NA,76,45,NA)
is.na(a)

is.na(fin$Expenses)
fin[is.na(fin$Expenses),]
fin[is.na(fin$State),]

#Removing records with missing data:
fin_backup <- fin

fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]
fin[is.na(fin$Industry),] #opposite
fin <- fin[!is.na(fin$Industry),]
fin

fin[!complete.cases(fin),]

#Resetting the dataframe index
fin
rownames(fin) <- 1:nrow(fin)
fin

fin
rownames(fin) <- NULL
fin

#Replacing the missing data: Factual analysis
fin[!complete.cases(fin),]

fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City == "New York"]
fin[is.na(fin$State) & fin$City == "New York","State"] <- "NY"
#check:
fin[c(11,377),]

fin[!complete.cases(fin),]

fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City == "San Francisco"]
fin[is.na(fin$State) & fin$City == "San Francisco","State"] <- "CA"
#check:
fin[c(82,265),]

fin[!complete.cases(fin),]

#Replacing the missing data: Median Imputation Method (Part1)
fin[!complete.cases(fin),]

med_empl_retail <- median(fin[fin$Industry == "Retail", "Employees"], na.rm = TRUE)
med_empl_retail

fin[is.na(fin$Employees) & fin$Industry == "Retail",]
fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] <- med_empl_retail
#check:
fin[3,]

fin[!complete.cases(fin),]

med_empl_services <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = TRUE)
med_empl_services

fin[is.na(fin$Employees) & fin$Industry == "Financial Services",]
fin[is.na(fin$Employees) & fin$Industry == "Financial Services", "Employees"] <- med_empl_services
#check:
fin[330,]

fin[!complete.cases(fin),]

#Replacing the missing data: Median Imputation Method (Part2)
fin[!complete.cases(fin),]

med_growth_constr <- median(fin[fin$Industry == "Construction", "Growth"], na.rm = TRUE)
med_growth_constr
fin[is.na(fin$Growth) & fin$Industry == "Construction",]
fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth"] <- med_growth_constr
#check:
fin[8,]

fin[!complete.cases(fin),]

#Replacing the missing data: Median Imputation Method (Part3)
fin[!complete.cases(fin),]
#Revenue:
fin[is.na(fin$Revenue),]
med_revenue_constr <- median(fin[fin$Industry == "Construction", "Revenue"], na.rm = TRUE)
med_revenue_constr

fin[is.na(fin$Revenue) & fin$Industry == "Construction",]
fin[is.na(fin$Revenue) & fin$Industry == "Construction", "Revenue"] <- med_revenue_constr
#check:
fin[c(8,42),]

fin[!complete.cases(fin),]
#Expenses:
# Be careful here. Only for certain ones
# We don't want to replace that one that's by itself (because then that row won't add up)
fin[is.na(fin$Expenses),]
med_expenses_constr <- median(fin[fin$Industry == "Construction", "Expenses"], na.rm = TRUE)
med_expenses_constr

fin[is.na(fin$Expenses) & fin$Industry == "Construction",]
fin[is.na(fin$Expenses) & fin$Industry == "Construction", "Expenses"] <- med_expenses_constr
#check:
fin[c(8,42),]

#Replacing the missing data: Deriving values
# Revenue - Expenses = Profit
# Expenses = Revenue - Profit
fin[!complete.cases(fin),]
fin[is.na(fin$Expenses),]
fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses), "Profit"]
#check:
fin[15,]

fin[!complete.cases(fin),]
fin[is.na(fin$Profit),]
fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit), "Expenses"]
#check:
fin[c(8,42),]

fin[!complete.cases(fin),]

#Visualisation:
# install.packages("ggplot2")
library(ggplot2)
# A scatterplot classified by industry showing revenue, expenses, profit
p <- ggplot(data = fin)
p
p + geom_point(aes(x = Revenue, y = Expenses,
                   color = Industry, size = Profit))
# A scatterplot that includes industry trends for the expenses~revenue relationship
d <- ggplot(data = fin, aes(x = Revenue, y = Expenses,
                   color = Industry))
d + geom_point() +
  geom_smooth(fill = NA, size = 1.2)

# BoxPlots showing growth by industry
f <- ggplot(data = fin, aes(x = Industry, y = Growth,
            color = Industry))
f + geom_boxplot(size = 1)
# Extra:
f + geom_jitter() +
  geom_boxplot(size = 1, alpha = 0.5,
               outlier.color = NA)

# -------------------------------------