                            #Anime Rating Prediction#
#*************************************************************************************************************************#
#Step 1-import the libraries
library(boot) #
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(Hmisc)
library(caTools) #used for splitting the data into test and train
library(ggplot2) #used for data visualization
library(Metrics)
library(psych)
#*************************************************************************************************************************#

#Step 2- Data Extraction or Import
#set directory to extract the data
setwd("E:\\IVY COURSE\\R STUDIO\\Internship")
#use of na.strings in order to convert blank cells to 'NA'
Anime<- read.csv(file ="Anime_Final.csv", header = TRUE, fill = T, na.strings = c("","[]"))
#get the table containing the dataset
View(Anime)
#names of the attributes as well as the overall dataset
names(Anime)
dim(Anime) #check for the dimension of the dataset
nrow(Anime) #no of rows
ncol(Anime) #no of columns
#Result- The dataset is comprised of 7029 rows and 16 columns
#*************************************************************************************************************************#
#Step 3-check for Data Sanity
str(Anime) #check for the overall structure of the data (types of variables)
#Result- From the above mentioned code, we could derive that 
#8 columns are of Character datatype, while rest 7 are integer and one column(Rating) is Numeric
summary(Anime) #check for the summarized output in terms of metrics
describe(Anime) #check for detailed description of the variables

#--------------------------------------------------------------------------------------------------------------------------#
#Convert all of the character datatypes to the factor datatypes in the dataset
Anime$mediaType<-as.factor(Anime$mediaType)
Anime$ongoing<-as.factor(Anime$ongoing)
Anime$sznOfRelease<-as.factor(Anime$sznOfRelease)
Anime$description<-as.factor(Anime$description)
Anime$studios<-as.factor(Anime$studios)
Anime$tags<-as.factor(Anime$tags)
Anime$contentWarn<-as.factor(Anime$contentWarn)
#--------------------------------------------------------------------------------------------------------------------------#
#check for data sanity once again
str(Anime)
#Results - [title(7029 levels),mediaType(9 levels),ongoing(2 levels),
#           sznOfRelease(5 levels),description(3923 levels), studios(532 levels),
#           tags(4869 levels),contentWarn(131 levels)]
summary(Anime)
describe(Anime)
#Nature of Variables-
#Quantitative -eps,duration,watched,watching,wantWatch,dropped,rating,votes
#Categorical- mediaType,ongoing,sznOfRelease,
#Qualitative- title,description,studios,tags,contentWarn
#*************************************************************************************************************************#
#Step 4-Data Pre-processing
#the different steps of data preprocessing includes-
# 1. Missing Value treatment, 2.Duplicate treatment 3.Outlier treatment
#--------------------------------------------------------------------------------------------------------------------------#
#4.1-Missing value treatment
#check for missing values
sapply(Anime, function(x) sum(is.na(x)))
# ##Result- From the output we can witness the missing values,
#                 mediaType(30),duration(1696),sznOfRelease(4916)
#                 description(3083), studios(2239),tags(229)
#                 contentWarn(6242), watched(87)
#-------------------------------------------------------------------------------------------------------------------------#
# check for percentage wise missing values
library(dplyr)
Missing_values <- data.frame(sapply(Anime, function(y) round((sum(length(which(is.na(y))))/nrow(Anime))*100.00,2)))
Missing_values
#Filter the missing values >30%
Missing_values %>% filter(Missing_values > 30.00)
# #Result- Missing values greater than 30% are for the following categories
#         sznOfRelease- 69.94%
#         description - 43.86%
#         studios- 31.85%
#         contentWarn - 88.80%
# for the above cases we will remove them from further calculations
#Now for the categorical variables, we will use mode and for numeric variables we will use median
#[For the above mentioned columns , as we are going to drop them eventually, hence, it's better 
# to explore them a bit before dropping ]
#--------------------------------------------------------------------------------------------------------------------------#
#Exploratory data Analysis
#Importing libraries for data visualization and exploration
library(tidyverse)
library(magrittr)
library(sqldf)
#--------------------------------------------------------------------------------------------------------------------------#
#For the two types of variables , the following graphs or plots are being chosen
#Histograms ~ Continuous Variables()
#Barplot~ Categorical(sznOfRelease)
#Exploration of data using SQL for the Qualitative variables(description,studios,contentWarn)
#--------------------------------------------------------------------------------------------------------------------------#
#1. Barplot of Season Of Release
counts_sznofRelease<- table(Anime$sznOfRelease)
barplot(counts_sznofRelease,
        main = "Barplot of Seasons Of Release" ,
        xlab = "Seasons Of Release" ,
        ylab = "Frequency",
        col = c("blue","green","yellow","orange","red","lightgreen","gray","violet"))
#Results- Spring has the highest no. of Releases followed by Fall
#With respect to sznOfRelease, how was Rating distributed
boxplot(rating~sznOfRelease,
        data = Anime,
        xlab = "Rating",
        ylab = "sznOfRelease",
        main = "Rating vs sznOfRelease",
        horizontal = TRUE)
#Results- For the Spring and Summer, there are presence of Outliers, but mostly skewed towards higher rating
#--------------------------------------------------------------------------------------------------------------------------#
#2. Details on Studios
#2.1- Top 5 studios in terms of maximum nos. of Anime
viz_1 = data.frame(sqldf('select studios,COUNT(studios) from Anime GROUP BY studios ORDER BY COUNT(studios) desc limit (5)'))
viz_1 <- table(viz_1)
barplot(viz_1 ,
        main = "Barplot of Studios" ,
        xlab = "Studios" ,
        ylab = "Frequency",
        col = c("blue","green","yellow","orange","red","lightgreen","gray","violet"))
#Results- Sunrise Studios contributed mostly for the Anime Shows
#--------------------------------------------------------------------------------------------------------------------------#
#3. Details on Top 5 content warnings
sqldf('select contentwarn,COUNT(contentwarn) from Anime GROUP BY contentwarn ORDER BY COUNT(contentwarn) desc limit (5)')
#Results- The above analysis states Violence being used the most as Contents for shows
#--------------------------------------------------------------------------------------------------------------------------#
## Dropping the redundant variables - sznOfRelease,description,studios & contentwarn using subset function
Anime<- subset(Anime, select =-c(sznOfRelease,description,studios,contentWarn))
View(Anime)
names(Anime)

#--------------------------------------------------------------------------------------------------------------------------#
#Replacing the missing values
#First Step- Categorical Variables ~ replacement using Mode Function
#Second Step- Continuous Variables ~ replacement using Median Function
#--------------------------------------------------------------------------------------------------------------------------#
#First Step-Replacement of categorical variables(tags,mediaType)
# Create the function for mode 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Calculate the mode using the user function.
Anime$mediaType[is.na(Anime$mediaType)] <- getmode(Anime$mediaType[!is.na(Anime$mediaType)])                 # Mode imputation
Anime$tags[is.na(Anime$tags)] <- getmode(Anime$tags[!is.na(Anime$tags)])                 # Mode imputation

A <- getmode(Anime$mediaType)
E <- getmode(Anime$tags)

print(A)
print(E)
View(Anime)
#Result- Media type which has 30 missing values, and for tags that has 229 missing values, those are being replaced by the mode function
#-------------------------------------------------------------------------------------------------------------------------#
#Second Step-Replacement of continuous variables (duration,watched) 
#replace missing values with median
for(i in c(4,11)){
  Anime[is.na(Anime[,i]),i]<-round(median(Anime[,i],na.rm = TRUE))
}
Anime$watched[is.na(Anime$watched)]<- round(median(Anime$watched,na.rm=TRUE))
#check the output
print(str(Anime$watched))
summary(Anime)

#Result- Duration which has 1696 missing values, and for watched that has 87 missing values, those are being replaced by the median function
#--------------------------------------------------------------------------------------------------------------------------#
#4.2-Duplicate values treatment

#check for the unique values
lapply(Anime, function(x) length(unique(x)))
#Result- Tags and Title have too many of the unique values, hence, for further ease in modelling, they should be dropped
#Please note that we will backup the data with a different name which we will use to explore the respective attributes present in them

#--------------------------------------------------------------------------------------------------------------------------#
#back up the data
Anime1<-Anime
nrow(Anime1)
#--------------------------------------------------------------------------------------------------------------------------#
#4.3-Outlier treatment
#Boxplot view~ Continuous variables)
#Rating
# identify outliers in r boxplot and save it
boxplot(Anime$rating, main = "boxplot for Rating")
#quantile view
quantile(Anime$rating, seq(0,1, by= 0.005))
#Result- From the Boxplot visualization and also from the quantile processing, it can be confirmed, 
#        that the rating data being continuous, has no outliers present
#--------------------------------------------------------------------------------------------------------------------------#
#1.Eps- 
# identify outliers in r boxplot and save it
boxplot(Anime$eps, main = "Boxplot of Episodes",xlab = "No. of Episodes", horizontal = TRUE)
quantile(Anime$eps, seq(0,1, by= 0.001))
quantile(Anime$eps, c(0.999,0.999301))
#Result- Here, we will be considering the value of 99.9301% which is 399.6171, rounded off to 400, to replace the extreme outliers
#check for details
length(Anime$eps)
summary(Anime$eps)
#setting the bench mark
eps1 <- as.numeric(round(quantile(Anime$eps,0.999301))) 
eps1
#Winsorizing
Anime$eps = (ifelse(Anime$eps > eps1,eps1,Anime$eps))
#Final visualization
summary(Anime$eps)
boxplot(Anime$eps,main = "Boxplot of Episodes",xlab = "No. of Episodes", horizontal = TRUE)
#-------------------------------------------------------------------------------------------------------------------------#
#2.Duration- 
# identify outliers in r boxplot and save it
boxplot(Anime$duration,main = "Boxplot of duration",xlab = "Total Duration in minutes", horizontal = TRUE)
quantile(Anime$duration, seq(0,1, by= 0.001))
quantile(Anime$duration, c(0.999,0.999701))
#Result- Here, we will be considering the value of 99.9701% which is 150.000, to replace the extreme outliers 
#check for details
length(Anime$duration)
summary(Anime$duration)
#setting the bench mark
dur1 <- as.numeric(round(quantile(Anime$duration,0.999701))) 
dur1
#Winsorizing
Anime$duration= (ifelse(Anime$duration > dur1,dur1,Anime$duration))
#Final visualization
summary(Anime$duration)
boxplot(Anime$duration,main = "Boxplot of duration",xlab = "Total Duration in minutes", horizontal = TRUE)
#-------------------------------------------------------------------------------------------------------------------------#
#3.Watched- 
# identify outliers in r boxplot and save it
boxplot(Anime$watched, main = "Boxplot of Watched",xlab = "No. of People Watched",horizontal = TRUE)
quantile(Anime$watched, seq(0,1, by= 0.001))
quantile(Anime$watched,c(0.999,0.999464))
#Result- Here, we will be considering the value of 99.9464% which is 100019.23 , rounded off to 100019, to replace the extreme outliers(which is 4 in this case)
#check for details
length(Anime$watched)
summary(Anime$watched)
#setting the bench mark
wat1 <- as.numeric(round(quantile(Anime$watched,0.999464))) 
wat1
#Winsorizing
Anime$watched = (ifelse(Anime$watched > wat1,wat1,Anime$watched))
#Final visualization
summary(Anime$watched)
boxplot(Anime$watched, main = "Boxplot of Watched",xlab = "No. of People Watched",horizontal = TRUE)
#-------------------------------------------------------------------------------------------------------------------------#
#4.Watching- 
# identify outliers in r boxplot and save it
boxplot(Anime$watching,main = "Boxplot of Watching",xlab = "No. of People Watching", horizontal = TRUE)
quantile(Anime$watching, seq(0,1, by= 0.001))
quantile(Anime$watching,c(0.999,0.9995))
#Result- Here, we will be considering the value of 99.95% which is 20021.98 , rounded off to 20022, to replace the extreme outliers(which is 4 in this case)
#check for details
length(Anime$watching)
summary(Anime$watching)
#setting the bench mark
watchn1 <- as.numeric(round(quantile(Anime$watching,0.9995)))
watchn1
#Winsorizing
Anime$watching = (ifelse(Anime$watching > watchn1,watchn1,Anime$watching))
#Final visualization
summary(Anime$watching)
boxplot(Anime$watching,main = "Boxplot of Watching",xlab = "No. of People Watching", horizontal = TRUE)
#-------------------------------------------------------------------------------------------------------------------------#
#5.Wannawatch- 
# identify outliers in r boxplot and save it
boxplot(Anime$wantWatch,main = "Boxplot of Want to Watch",xlab = "No. of People want to watch", horizontal = TRUE)
quantile(Anime$wantWatch, seq(0,1, by= 0.001))
quantile(Anime$wantWatch,c(0.999,0.99929))
#Result- Here, we will be considering the value of 99.929% which is 20003.17 , rounded off to 20003, to replace the extreme outliers(which is 5 in this case)
#check for details
length(Anime$wantWatch)
summary(Anime$wantWatch)
#setting the bench mark
wantw1 <- as.numeric(round(quantile(Anime$wantWatch,0.99929))) 
wantw1
#Winsorizing
Anime$wantWatch = (ifelse(Anime$wantWatch > wantw1,wantw1,Anime$wantWatch))
#Final visualization
summary(Anime$wantWatch)
boxplot(Anime$wantWatch, main = "Boxplot of Want to Watch",xlab = "No. of People want to watch",horizontal = TRUE)
#-------------------------------------------------------------------------------------------------------------------------#
#6.dropped- 
# identify outliers in r boxplot and save it
boxplot(Anime$dropped, main = "Boxplot of  Dropped",xlab = "No. of People dropped out",horizontal = TRUE)
quantile(Anime$dropped, seq(0,1, by= 0.001))
quantile(Anime$dropped, c(0.999,0.999381))
#Result- Here, we will be considering the value of 99.9381% which is 4998.508 , rounded off to 4999, to replace the extreme outliers(which is 5 in this case)
#check for details
length(Anime$dropped)
summary(Anime$dropped)
#setting the bench mark
drp <- as.numeric(round(quantile(Anime$dropped,0.999381))) 
drp
#Winsorizing
Anime$dropped = (ifelse(Anime$dropped > drp,drp,Anime$dropped))
#Final visualization
summary(Anime$dropped)
boxplot(Anime$dropped, main = "Boxplot of  Dropped",xlab = "No. of People dropped out", horizontal = TRUE)
#-------------------------------------------------------------------------------------------------------------------------#
#7.Voted- 
# identify outliers in r boxplot and save it
boxplot(Anime$votes, main = "Boxplot of  Votes",xlab = "No. of People Voted", horizontal = TRUE)
quantile(Anime$votes, seq(0,1, by= 0.001))
quantile(Anime$votes, c(0.999,0.999454))
#Result- Here, we will be considering the value of 99.9454% which is 80022.01, rounded off to 80022, to replace the extreme outliers(which is 4 in this case)
#check for details
length(Anime$votes)
summary(Anime$votes)
#setting the bench mark
vote <- as.numeric(round(quantile(Anime$votes,0.999454))) 
vote
#Winsorizing
Anime$votes = (ifelse(Anime$votes > vote,vote,Anime$votes))
#Final visualization
summary(Anime$votes)
boxplot(Anime$votes,main = "Boxplot of  Votes",xlab = "No. of People Voted", horizontal = TRUE)

#Note- All the outliers are being treated with respect to their quantiles > 99.9%
## Select a subset of Continuous and Categorical variables
Cont_Var<- subset(Anime1,select = c(eps,duration,watching,watched,wantWatch,dropped,rating,votes))
Cont_Var

Cat_Var<- subset(Anime1,select = c(mediaType,ongoing))
Cat_Var
#*************************************************************************************************************************#
#Step 5- Exploratory Data Analysis(Graphical)

#5.1  Univariate Analysis
#*************************************************************************************************************************#
#Histograms ~ Continuous Variables()
#Barplot~ Categorical(sznOfRelease)
#Exploration of data using SQL for the Qualitative variables(description,studios,contentWarn)
#*************************************************************************************************************************#
##1. Barplot of Media Types
counts_media<- table(Anime1$mediaType)
barplot(counts_media,
        main = "Barplot of MediaTypes" ,
        xlab = "Media types" ,
        ylab = "Frequency",
        col = c("blue","green","yellow","orange","red","lightgreen","gray","violet"))
#Results- TV is the most used medium or platform to watch Anime, while TV special is the least
#-------------------------------------------------------------------------------------------------------------------------#
##2. Histogram of Eps
hist(Anime1$eps,
     xlab = "Episodes_distribution",
     main = "Histogram view of Episodes",
     col = "blue",
     xlim = c(0,150),
     breaks = sqrt(nrow(Anime1)))
#Results- Majority of the shows has episodes between 0-20
#-------------------------------------------------------------------------------------------------------------------------#
##3. Histogram of Duration
hist(Anime1$duration,xlab = "Duration_distribution",col = "green")
hist(Anime1$duration,
     xlab = "Duration_distribution",
     main = "Histogram view of Duration",
     col = "green",
     xlim = c(0,140),
     breaks = sqrt(nrow(Anime1)))
#Results- Majority of the shows has duration between 0-8 minutes
#         The Graph over here is Right skewed as observed
#-------------------------------------------------------------------------------------------------------------------------#
##4. Barplot of Ongoing
counts_ongoing<- table(Anime1$ongoing)
barplot(counts_ongoing,
        main = "Barplot of ongoing shows" ,
        xlab = "Ongoing Shows" ,
        ylab = "Frequency",
        col = c("blue","green"))
#Result- Most of the Shows are not live ones, and hence are not ongoing presently
#-------------------------------------------------------------------------------------------------------------------------#
##5. Details on Tags
#5.1- Top 5 tags in terms of most nos. of Anime
sqldf('select tags,COUNT(tags) from Anime GROUP BY tags ORDER BY COUNT(tags) desc limit (5)')
t <- table(Anime1$tags) # frequency of values in tags
plot(( sort(t, decreasing=TRUE)[1:5] ), type="h")
#Results- Vocaloid is the tag which has being used the most for the Shows
#-------------------------------------------------------------------------------------------------------------------------#
##6. Details on Top 5 shows watched by the no. of Users
sqldf('select title, SUM(watched) from Anime1 GROUP BY title ORDER BY SUM(watched) desc limit (5)')
#Results- Deathnote has been the most watched show

##3. Histogram of watched
hist(Anime1$watched,xlab = "watched_distribution", main = "Histogram view of watched",col = "green")
hist(Anime1$watched,
     xlab = "watched_distributionn",
     main = "Histogram view of watched",
     col = "green",
     xlim = c(0,140),
     breaks = sqrt(nrow(Anime1)))
#-------------------------------------------------------------------------------------------------------------------------#
##7.Details on Top 5 shows currently being watched/watching by the no. of Users
sqldf('select title, SUM(watching) from Anime1 GROUP BY title ORDER BY SUM(watching) desc limit (5)')
#Results- Naruto Shippuden is being the most watched show at present

hist(Anime1$watching,xlab = "watching_distribution", main = "Histogram view of watching shows",col = "blue")

#-------------------------------------------------------------------------------------------------------------------------#
##8.Details on Top 5 shows people are willing to watch
sqldf('select title, SUM(wantWatch) from Anime1 GROUP BY title ORDER BY SUM(wantWatch) desc limit (5)')
#Results- Steins;Gate is the show most people are willing to watch
hist(Anime1$wantWatch,xlab = "wantwatch_distribution", main = "Histogram view of wantwatch shows",col = "red")

#-------------------------------------------------------------------------------------------------------------------------#
##9.Details on Top 5 shows people have dropped before completing it
sqldf('select title, SUM(dropped) from Anime1 GROUP BY title ORDER BY SUM(dropped) desc limit (5)')
#Results- Naruto Shippuden is the show most people have dropped before completion
hist(Anime1$dropped,xlab = "dropped_distribution", main = "Histogram view of dropped shows",col = "red")

#-------------------------------------------------------------------------------------------------------------------------#
##10. Histogram of Ratings
hist(Anime1$rating,
     xlab = "Rating",
     main = "Histogram of Rating",
     breaks = sqrt(nrow(Anime1)),
     lines(density(Anime1$rating, adjust=2), lty="dotted", col="darkgreen", lwd=2))
#Result- The rating has Binomial Distribution
#-------------------------------------------------------------------------------------------------------------------------#
##11.Details on Top 5 shows which has being voted by users the most
sqldf('select title, SUM(votes) from Anime1 GROUP BY title ORDER BY SUM(votes) desc limit (5)')
#Results- Death Note is the most voted show by people
hist(Anime1$votes,xlab = "votes_distribution", main = "Histogram view of votes of shows",col = "yellow")

#*************************************************************************************************************************#
#5.2 Bivariate Analysis: 
#*************************************************************************************************************************#
#Scatterplots -Continuous vs Continuous Variables
#Boxplots - Continuous vs Categorical Variables
#*************************************************************************************************************************#
##1.With respect to mediaType
boxplot(rating~mediaType,
        data = Anime1,
        xlab = "Rating",
        ylab = "mediaType",
        main = "Rating vs mediaType")
#Results- For the Movie ,OVA, TV types presence of outliers can be noticed and they are left skewed
#         More or less, the data has skewness
#-------------------------------------------------------------------------------------------------------------------------#
##2.With respect to eps
plot(x= Anime1$rating, y= Anime1$eps,
     xlab = "Rating",
     ylab = "eps",
     main = "Rating vs eps")
#Results- This graph shows, the shows with episodes between 0-15 have rating between 1.5-3.5 on an average
#-------------------------------------------------------------------------------------------------------------------------#
##3.With respect to duration
plot(x= Anime1$rating, y= Anime1$duration,
     xlab = "Rating",
     ylab = "duration",
     main = "Rating vs duration")
#Results- This graph shows, the shows with less duration have higher ratings , but also many of those with higher durations are rated well.
#         So no clear interpretation can be done
#-------------------------------------------------------------------------------------------------------------------------#
##4.With respect to ongoing
boxplot(rating~ongoing,
        data = Anime1,
        xlab = "Rating",
        ylab = "ongoing",
        main = "Rating vs ongoing")
#Results- For the Ongoing shows, presence of outliers can be noticed and they are left skewed
#         The shows that are not outgoing , have normal distribution
#-------------------------------------------------------------------------------------------------------------------------#
##5.With respect to tags
plot(x= Anime1$rating, y= Anime1$tags,
     xlab = "Rating",
     ylab = "tags",
     main = "Rating vs tags")
#Result- No interpretation can be drawn out of this variable
#-------------------------------------------------------------------------------------------------------------------------#
##6.With respect to watched
plot(x= Anime1$rating, y= Anime1$watched,
     xlab = "Rating",
     ylab = "watched",
     main = "Rating vs watched")
#Result- Most of the high Rated shows are less watched, with few exceptions
#-------------------------------------------------------------------------------------------------------------------------#
##7.With respect to watching
plot(x= Anime1$rating, y= Anime1$watching,
     xlab = "Rating",
     ylab = "watching",
     main = "Rating vs watching")
#Result- Most of the high Rated shows are being currently less watched, with few exceptions
#-------------------------------------------------------------------------------------------------------------------------#
##8.With respect to wantWatch
plot(x= Anime1$rating, y= Anime1$wantWatch,
     xlab = "Rating",
     ylab = "wantWatch",
     main = "Rating vs wantWatch")
#Result- Most of the high Rated shows are in the watchlist of no. of users between 0-10000
#-------------------------------------------------------------------------------------------------------------------------#
##9.With respect to dropped
plot(x= Anime1$rating, y= Anime1$dropped,
     xlab = "Rating",
     ylab = "dropped",
     main = "Rating vs dropped")
#Result- Most of the high Rated shows are less dropped, w.r.t. others
#-------------------------------------------------------------------------------------------------------------------------#
##10.With respect to votes
plot(x= Anime1$rating, y= Anime1$votes,
     xlab = "Rating",
     ylab = "votes",
     main = "Rating vs votes")
#Result- Most of the high Rated shows are less voted,,there is a gradual trend considering an increase in votes for few of the shows
#-------------------------------------------------------------------------------------------------------------------------#
##11.With respect to ongoing
boxplot(rating~ongoing,
        data = Anime1,
        xlab = "Rating",
        ylab = "ongoing",
        main = "Rating vs ongoing")



#*************************************************************************************************************************#
#Step 6- Exploratory Data Analysis(Statistical tests)
#6.1.Correlation tests
#Correlation test is used to evaluate the association between two or more variables(continuous)
#Pearson correlation(r) -It measures a linear dependence between two variables (x and y). 
#                        It's also known as a parametric correlation test because it depends to the distribution of the data. 
#                        It can be used only when x and y are from normal distribution.
#                        If the p-value is < 5%, then the correlation between x and y is significant.
# Correlation coefficient can be computed using the functions cor() or cor.test()

#*************************************************************************************************************************#
##1.With respect to eps
Eps_Rate <- cor.test(Anime$rating, Anime$eps, 
                     method = "pearson")
Eps_Rate
#Result- P value < 2.2e-16, which is less than the significance level alpha = 0.05
# cor of 0.1109282 or 11.09% indicates very weak positive correlation between the two variables
#-------------------------------------------------------------------------------------------------------------------------#
##2.With respect to duration
Dur_Rate <- cor.test(Anime$rating, Anime$duration, 
                     method = "pearson")
Dur_Rate
#Result- P value < 2.2e-16, which is less than the significance level alpha = 0.05, means its significant
# cor of 0.2836987 or 28.36% indicates a weak positive correlation between the two variables
#-------------------------------------------------------------------------------------------------------------------------#
##3.With respect to watched
Wat_Rate <- cor.test(Anime$rating, Anime$watched, 
                     method = "pearson")
Wat_Rate
#Result- P value < 2.2e-16, which is less than the significance level alpha = 0.05, means its significant
# cor of 0.4267195 or 42.67% indicates a moderate positive correlation between the two variables
#-------------------------------------------------------------------------------------------------------------------------#
##4.With respect to watching
Watchn_Rate <- cor.test(Anime$rating, Anime$watching, 
                        method = "pearson")
Watchn_Rate
#Result- P value < 2.2e-16, which is less than the significance level alpha = 0.05, means its significant
# cor of 0.3308784 or 33.08% indicates a weak positive correlation between the two variables
#-------------------------------------------------------------------------------------------------------------------------#
##5.With respect to wantWatch
Want_Watch_Rate <- cor.test(Anime$rating, Anime$wantWatch, 
                            method = "pearson")
Want_Watch_Rate
#Result- P value < 2.2e-16, which is less than the significance level alpha = 0.05, means its significant
# cor of 0.5486213 or 54.86% indicates a moderate positive correlation between the two variables
#-------------------------------------------------------------------------------------------------------------------------#
##6.With respect to dropped
Drop_Rate <- cor.test(Anime$rating, Anime$dropped, 
                      method = "pearson")
Drop_Rate
#Result- P value < 2.2e-16, which is less than the significance level alpha = 0.05, means its significant
# cor of 0.3286063 or 32.86% indicates a weak positive correlation between the two variables
#-------------------------------------------------------------------------------------------------------------------------#
##7.With respect to votes
Vote_Rate <- cor.test(Anime$rating, Anime$votes, 
                      method = "pearson")
Vote_Rate
#Result- P value < 2.2e-16, which is less than the significance level alpha = 0.05, means its significant
# cor of 0.4154198  or 41.54% indicates a moderate positive correlation between the two variables

## Plot a correlation graph
#install.packages("corrplot")
library(corrplot)
Anime_Cor = cor(Anime[,c("eps","duration","watched","watching","wantWatch","dropped","rating","votes")])
corrplot(Anime_Cor, method = "number")

#Considering watched,wantWatch, Votes having correlation >0.40, we need to use log transformation of the same
#Consider dropping of Variables with little or no correlation Ex- eps, duration
Anime<- subset(Anime, select =-c(eps,duration))
View(Anime)
#*************************************************************************************************************************#
#6.2.Anova tests
#ANOVA stands for Analysis of Variance, is a statistical test used to 
#       analyze the difference between the means of more than two groups(Categorical vs continuous)
#1.With respect to MediaTypes,ongoing, tags
Anov <- aov(rating ~ mediaType+ongoing+tags, data = Anime)
summary(Anov)
#Result- P value < 2.2e-16, which is less than the significance level alpha = 0.05, means for both the variables, there is presence of correlation 
#       with respect to the categorical variables as stated
# Since the F statistic, in each case is greater than the critical value, 
#      we conclude that there is a significant batch effect at the 0.05 level of significance.
#*************************************************************************************************************************#
#6.3.Check for Nonlinearity(on the Backed up data)
pairs.panels(Cont_Var,col ="red")
pairs.panels(Cat_Var,col ="red")
View(Anime)
#change the values of all numerical variables except the rating using the logarithmic transformation
Anime_Cor_1 = cor(Anime[,c("watched","wantWatch","watching", "dropped", "rating","votes")])
corrplot(Anime_Cor_1, method = "number")

Anime$watched<-log(Anime$watched)
Anime$wantWatch<-log(Anime$wantWatch)
Anime$votes<-log(Anime$votes)
View(Anime)
hist(Anime$wantWatch)
# Replace inf with 1
Anime$watched[Anime$watched == -Inf]<- 1
Anime$wantWatch[Anime$wantWatch == -Inf]<- 1


Cont1_Var<- subset(Anime,select = c(eps,duration,watching,watched,wantWatch,dropped,rating,votes))
Cont1_Var
pairs.panels(Cont1_Var,col ="red")
#*************************************************************************************************************************#
#                                                    Task 2
#Step 7-Model Development
#create a backup data
data<- Anime

#7.1-Splitting the data set into train and test data
set.seed(123)
library(caTools)
sample<- sample.split(Anime, SplitRatio = 0.70)
#-------------------------------------------------------------------------------------------------------------------------#
#7.2defining train & test data
Anime_train<- subset(Anime, sample == TRUE)
Anime_test<- subset(Anime, sample == FALSE)
#-------------------------------------------------------------------------------------------------------------------------#
#7.3-checking for the no. of rows and columns of the respective datasets
nrow(Anime_train)
nrow(Anime_test)
names(Anime)
#-------------------------------------------------------------------------------------------------------------------------#
#7.4-create linear regression model
Anime_fit<- lm(rating ~., data = Anime_train)
##Step 1
Anime_fit_1 <- lm(rating ~ mediaType + eps + duration + ongoing + 
                    tags + watched+ watching+ wantWatch + 
                    dropped + votes, data = Anime_train)
summary(Anime_fit_1)
#-------------------------------------------------------------------------------------------------------------------------#
##Step 2- removing tags
Anime_fit_1 <- lm(rating ~ mediaType +  ongoing + 
                    watched + wantWatch + watching +
                    dropped + votes, data = Anime_train)
summary(Anime_fit_1)
#-------------------------------------------------------------------------------------------------------------------------#
##Step 3 -removing all other media type except music video,Tv, and Votes
Anime_fit_1 <- lm(rating ~ I(mediaType== "Movie") + I(mediaType== "Music Video") + I(mediaType== "TV Special")+
                  I(ongoing== "Yes") + watched + wantWatch + watching +
                    dropped + votes,
                  data = Anime_train)
summary(Anime_fit_1)
#-------------------------------------------------------------------------------------------------------------------------#
##Step 4 -removing votes
Anime_fit_1 <- lm(rating ~ I(mediaType== "Movie") + I(mediaType== "Music Video") + I(mediaType== "TV Special")+
                    I(ongoing== "Yes") + watched + wantWatch + watching +
                    dropped,
                  data = Anime_train)
summary(Anime_fit_1)
#-------------------------------------------------------------------------------------------------------------------------#
#check Vif values
vif(Anime_fit_1)
#Results- Watched and Wantwatch column have high vif, hence should be dropped from the model
#-------------------------------------------------------------------------------------------------------------------------#
##Step 5 -removing wantwatch  which have high VIF and also correlation is >.40

Anime_fit <- lm(rating ~ I(mediaType== "Movie") + 
                    I(ongoing== "Yes") + watched + watching +
                    dropped,
                  data = Anime_train)
summary(Anime_fit)
#Result- p value of watching is >0.05, hence to be removed
crPlots(Anime_fit)
#-------------------------------------------------------------------------------------------------------------------------#
##Step 6- removing watching
Anime_fit <- lm(rating ~ I(mediaType== "Movie") + I(mediaType== "Music Video") +I(mediaType== "Other") +
                  I(mediaType== "OVA") + I(mediaType== "Web")+
                  eps + duration + I(ongoing== "Yes") + 
                  wantWatch + dropped ,
                  data = Anime_train)
summary(Anime_fit)
abline(Anime_fit,lwd = 3, col = "red")
ggplot(Anime_train, aes(rating, dropped) ) +
  geom_point() +
  stat_smooth()
Anime_fit_2 <- lm(rating ~ I((mediaType== "Movie")^2) + I((mediaType== "Music Video")^2) +I((mediaType== "Other")^2) +
     I((mediaType== "OVA")^2) + I((mediaType== "Web")^2)+
      I(duration^2) + I((ongoing== "Yes")^2) + 
     I(wantWatch^2) + I(dropped^2), data = Anime_train)
summary(Anime_fit_2)








#*************************************************************************************************************************#
#About the results
#*************************************************************************************************************************#
#The above table proves that there is a weak negative relationship between Rating and mediaType(Movie,Music Video,Other,Web),dropped 
# and weak positive relationship between Rating and mediaType(OVA),eps,duration,ongoing(Yes),wantWatch 
#Residual standard error (RSE).
#The RSE (or model sigma), corresponding to the prediction error, represents roughly the average difference between the observed outcome values and the predicted values by the model. 
#The lower the RSE the best the model fits to the data.
#Dividing the RSE by the average value of the outcome variable will give us the prediction error rate, which should be as small as possible.
#Here the RSE = 0.6784, meaning that the observed rating values deviate from the predicted values by approximately 0.5939 units in average.
mean(Anime_train$rating) #3.049757
Error_Rate <-0.5969/mean(Anime_train$rating)
Error_Rate
#This corresponds to an error rate of 0.6784/mean(Anime_train$rating) = 0.5915/3.049757 = 0.1939499 or 19.39%, which is low.
#-------------------------------------------------------------------------------------------------------------------------#
#R-squared
#The R-squared (R2) ranges from 0 to 1 and represents the proportion of variation in the outcome variable that can be explained by the model predictor variables.
#For a simple linear regression, R2 is the square of the Pearson correlation coefficient between 
#      the outcome and the predictor variables.
#In multiple linear regression, the R2 represents the correlation coefficient between the observed outcome values and the predicted values.
#The R2 measures, how well the model fits the data. The higher the R2, the better the model.
#Result- The R square value is 0.5764, which means the moderately okay
#-------------------------------------------------------------------------------------------------------------------------#
#Adjusted R squared
#The adjustment in the Adjusted R Squared value in the summary output is a correction for the number of x variables included in the predictive model.
#So the adjusted R-squared is considered, which is a penalized R2 for a higher number of predictors.
#An (adjusted) R2 that is close to 1 indicates that a large proportion of the variability in the outcome has been explained by the regression model.
#A number near 0 indicates that the regression model did not explain much of the variability in the outcome.
#Result- The Adjusted R square value is 0.5756  , which means the moderately okay
#-------------------------------------------------------------------------------------------------------------------------#
#F-Statistic:
#The F-statistic gives the overall significance of the model. 
#It assess whether at least one predictor variable has a non-zero coefficient.
#The F-statistic becomes more important once we start using multiple predictors as in multiple linear regression.
#A large F-statistic will corresponds to a statistically significant p-value (p < 0.05). 
#Results- In this example, the F-statistic equal 706.9 producing a p-value of < 2.2e-16, which is highly significant.
#**************************************************************************************************************************#
#7.5-estimate the model performance
#A more conventional way to estimate the model performance is to display the residual against different measures.
par(mfrow=c(2,2))
plot(Anime_fit)
#Step 8-check for model validation using correlation plots

#Result- It shows many of the variables are highly correlated, which is affecting the model accuracy
#8.2-Get the predicted or fitted values
fitted(Anime_fit)
Anime_train$pred <- fitted(Anime_fit)
#Plot the values
plot(Anime_train$pred,Anime_train$rating, main= "Actual vs predictive for train data")
abline(a=0, b=1)

Anime_train$pred_1<- fitted(Anime_fit_2)
plot(Anime_train$pred_1,Anime_train$rating, main= "Actual vs predictive for train data")
abline(a=0, b=1)

#Result- For the rating values between range of 1-4, they are highly concentrated towards the predicted values of 3-4

#**************************************************************************************************************************#
#Step 9- Check respective metrics- MAPE, MdAPE, RMSE
#9.1-checking MAPE
#MAPE- One of the most common metrics used to measure the forecasting accuracy of a model is MAPE, 
#                                                  which stands for mean absolute percentage error.
attach(Anime_train)
mape(Anime_train$rating,Anime_train$pred)
#Results - The MAPE value is 0.1857621 or 18.57% which means the average difference between 
#                           the predicted value and the actual value is 18.57%, which is acceptable
Mean_Accuracy_train = (1-mape(Anime_train$rating,Anime_train$pred))*100
 Mean_Accuracy_train
# In other words the mean accuracy thus is (100-MAPE)= (100-18.57)% = 81.42379
#-------------------------------------------------------------------------------------------------------------------------#
#9.2-checking Median APE
#MdAPE- The Median Absolute Percentage Error (MdAPE) is found by ordering the absolute percentage error (APE) from the smallest to the largest, and using its middle value 
#           (or the average of the middle two values if N is an even number) as the median
#MdAPE is more resilient to outliers than MAPE
#MdAPE is less intuitive, for example an MdAPE of 8% does not mean that the average absolute percentage error is 8%. 
#    Instead it means that half of the absolute percentage errors are less than 8% and half are over 8%

median(abs((Anime_train$rating-Anime_train$pred)/Anime_train$rating))
#Results- The MdAPE value is 0.123073 or 12.30%, which means half of the absolute percentage errors are less than 15.38%
#         while the rest is over 12.30%
Median_Accuracy_train = (1-median(abs((Anime_train$rating-Anime_train$pred)/Anime_train$rating)))*100
Median_Accuracy_train
#Thus Median Accuracy is 87.92226

#-------------------------------------------------------------------------------------------------------------------------#
#9.3-checking RMSE
#Root Mean Square Error-It is the standard deviation of the residuals (prediction errors). 
#                       RMSE is a measure of how spread out these residuals are. 
#                       It shows how concentrated the data is around the line of best fit. 

rmse(Anime_train$rating,Anime_train$pred)
#Result- RMSE value is0.5908401,thus >0.5 reflects the poor ability of the model to accurately predict the data, which is observed from this dataset
#-------------------------------------------------------------------------------------------------------------------------#
#9.4-Residuals- A residual is the vertical distance between a data point and the regression line.
#           Each data point has one residual. 
#           They are positive if they are above the regression line and negative if they are below the regression line.

#check for residuals
Anime_train$res<-studres(Anime_fit)
Anime_train$res

install.packages("gvlma")
library(gvlma)
gvmodel<- gvlma(Anime_fit)
summary(gvmodel)

install.packages("olsrr")
library(olsrr)
ols_plot_resid_qq(Anime_fit_2)
ols_plot_resid_fit(Anime_fit_2)
ols_plot_resid_hist(Anime_fit_2)
ols_test_normality(Anime_fit_2)
##################################### Checking of Assumption ############################################
#Step 10- Check for Assumptions
# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value

#10.1-check for multicollinearity
#Multicollinearity- Multicollinearity occurs when independent variables in a regression model are correlated. 
#                   This correlation is a problem because independent variables should be independent.

#VIF- The Variance Inflation Factor (VIF) measures the impact of collinearity among the variables in a regression model.
# As per VIF, 1 = not correlated, Between 1 and 5 = moderately correlated, Greater than 5 = highly correlated.
vif(Anime_fit)
#Conclusion- The vif values being less than 2 for Mediatype,eps,Duration,ongoing, hence we can say, that multicollinearity doesn't exist for them
# But for others , like watching, wantwatch, dropped, it is <4, thus acceptable
#**************************************************************************************************************************#

#10.2-check for autocorrelation
#Autocorrelation- Autocorrelation refers to the degree of correlation between 
#                 the values of the same variables across different observations in the data.
#Ho= the residuals are not correlated, ie. they are independent
#H1= the residuals are correlated
#Durbin Watson statistic- The Durbin Watson Test is a test statistic used in statistics to detect autocorrelation, 
#                                                                      in the residuals from a regression analysis.
# The Durbin-Watson statistic will always have a value between 0 and 4. 
#A value of 2.0 means that there is no autocorrelation detected in the sample. 
#Values from 0 to less than 2 indicate positive autocorrelation and values from  2 to 4 indicate negative autocorrelation.
durbinWatsonTest(Anime_fit)
#Conclusion- The D-W Statistic is 1.062099, which is less than 2 and it means that positive autocorrelation exits 
#**************************************************************************************************************************#
#10.3- check for heteroscedasticity
#heteroscedasticity- Heteroscedasticity is a systematic change in the spread of the residuals over the 
#                    range of measured values and is being caused mainly by the presence of outliers

# Breusch-Pagan test - It is used to test for heteroskedasticity in a linear regression model and assumes that the error terms are normally distributed. 
#                      It tests whether the variance of the errors from a regression is dependent on the values of the independent variables.
#                      If the test statistic has a p-value below an appropriate threshold (e.g. p < 0.05) then  
#                      the null hypothesis of homoskedasticity is rejected and heteroskedasticity assumed.
bptest(Anime_fit)  
#Result - p value from BP test being < 2.2e-16, which is < 0.05 means Heteroscedasticity exists
# Null hypothesis -> error is homogenious (p value should be more than 0.05)
#Hence null hypothesis can be rejected and therefore heterscedasticity exists

#Cook-Weisberg test --- Car package
# hypothesis of constant error variance against the alternative that the error variance changes with the level of the  response 
# p value should be more than 0.05

ncvTest(Anime_fit)
#Result - p value from NCV test being 0.00084771 thus < 0.05 means Heteroscedasticity exists
#**************************************************************************************************************************#
#10.4 -Test for normality
#Normality- A normality test is used to determine whether sample data has been drawn from a normally distributed population (within some tolerance).
#Null hypothesis - The errors should be normally distributed
#                   expected- the p value should be more than 0.05
#Anderson-Darling Test -The Anderson-Darling test is a statistical test of whether a given
#                            sample of data is drawn from a given probability distribution.
#                       The test rejects the hypothesis of normality when the p-value is less than or equal to 0.05. 
#                       Failing the normality test allows you to state with 95% confidence the data does not fit the normal distribution.
Anime_resids <- Anime_fit$residuals

ad.test(Anime_resids)

#Conclusion- P value is 8.232e-11, which is <0.05, hence means that with 95% confidence the data does not fit the normal distribution
#**************************************************************************************************************************#
#Step 11- Check for the Residuals and their Statistics
#distribution of residuals
#11.1-Histogram
hist(Anime_resids)
#Result- The residuals show skewness towards left for the residuals
#-------------------------------------------------------------------------------------------------------------------------#
#11.2-Quantile- Quantile plot~ Q-Q plot, is a graphical tool to help us assess if a set of data plausibly came from some theoretical distribution such as a Normal, exponential or Uniform distribution. 
#                         Also, it helps to determine if two data sets come from populations with a common distribution.
#                         In other words,it is a scatterplot created by plotting two sets of quantiles against one another
qqnorm(Anime_resids)
qqline(Anime_resids)
#Result- If both sets of quantiles came from the same distribution, we should see the points forming a line that's roughly straight.
#        Similarly here as well, the plot is a straight line
# As we can see from this plot the errors follow the straight line well so we can say this assumption is met.

####Testing the model on test data ###
#Test the model on the remaining dataset.
Anime_test$pred<- predict(Anime_fit, Anime_test)
attach(Anime_test)
plot(pred,rating, main= "Actual vs predictive for test data")
abline(a=0, b=1)
#Result - Most of variations can be seen between 2-4 values of the variables
#-------------------------------------------------------------------------------------------------------------------------#
#Check for Mean APE
mape(Anime_test$rating,Anime_test$pred)
mean(abs((rating-pred)/rating))
#Result- The MAPE value is 18.56% which means 
#              the average difference between the predicted value and the actual value is 22.16%.
Mean_Accuracy_test = (1-mape(Anime_test$rating,Anime_test$pred))*100
Mean_Accuracy_test
## The mean accuracy thus is 81.43577
#-------------------------------------------------------------------------------------------------------------------------#
#Check for Median APE
median(abs((Anime_test$rating-Anime_test$pred)/Anime_test$rating))
#Result- The MdAPE value is 0.1204413 or 12.04%, which means half of the absolute percentage errors are less than 15.38%
#         while the rest is over 12.04%
Median_Accuracy_test = (1-median(abs((Anime_test$rating-Anime_test$pred)/Anime_test$rating)))*100
Median_Accuracy_test
#Thus Median Accuracy is 87.95587
#-------------------------------------------------------------------------------------------------------------------------#
#Check for RMSE
rmse(Anime_test$rating,Anime_test$pred)
#Result- RMSE value is 0.5930023,thus =0.5 reflects the poor ability of the model to accurately predict the data, which is observed from this dataset
#**************************************************************************************************************************#
#check for residuals
Anime_test$res<- Anime_test$rating-Anime_test$pred
#-------------------------------------------------------------------------------------------------------------------------#
#Plot the residuals
plot(Anime_test$res)
#Results- Residuals are homogeneously scattered forming a band
#-------------------------------------------------------------------------------------------------------------------------#
hist(Anime_test$res)
#Results- The residuals show normal distribution of the data
#-------------------------------------------------------------------------------------------------------------------------#
qqnorm(Anime_test$res)
qqline(Anime_test$res)
#Results- If both sets of quantiles came from the same distribution, we should see the points forming a line that's roughly straight.
#        Similarly here as well, the plot is a straight line
#As we can see from this plot the errors follow the straight line well so we can say this assumption is met.

#**************************************************************************************************************************#
#Conclusion- Thus from the Metrics values like RSE which is considerably low, but Rsquare and adjusted R square value
# state that the predictor variables are not highly significant as per the model is concerned  and both having value ~ 44%
#The assumptions show the dataset being highly skewed, having multiple correlations and collinearity,and deviates from Normal Distribution as per the oevrall frequency being concerned, 
#                                                                                  even after treatment of outliers, missing values
#The MAPE and MdAPE values are approximately 22% and 15% which means the Accuracy being at a considerable range of 75-80%

# Business Recommendation
#The overall attributes used in the model as we have seen, had weak to moderate correlation with the target variable ie., Rating
#Hence, it is difficult to provide business recommendation from that point of View.
#But from the overall seasons of Release, as we have observed, that Shows with Ratings Greater than 4, had their Release around Fall and Spring
#From the Media Types, Shows with rating >4, are being released on TV the most