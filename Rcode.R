# By Matthew Leichter
# Using Educational Adaptive Math Hours to Predict Grade Improvment

#YOU NEED TO REMOVE THE HASHTAG AND RUN THE INSTALL PACKAGES COMMAND

#install.packages("lme4")
#install.packages("MuMIn")
library(lme4)
library(MuMIn)
library(ggplot2)

#Pull in data - CHANGE THE PATH NAME "C:/Users/Downloads..." ETC. TO THE FOLDER YOUR DATA IS IN!!!!!

initial_data_set1 <- read.csv(file 
                              = "C:/Users/Owner/Downloads/LGL_ADAM_Extract_2016-17_Pre-Post_withMeta-Edge_v2.txt",
                              sep="\t", header=TRUE, stringsAsFactors = FALSE)
initial_data_set2 <- read.csv(file 
                              = "C:/Users/Owner/Downloads/LGL_ADAM_Extract-2015-16-jcboe-prepost_withMeta.txt",
                              sep="\t", header=TRUE, stringsAsFactors = FALSE)

############################################# DATA CLEANUP ########################################

#Reformat column names for joining

colnames(initial_data_set1)[12] <- paste("Hispanic") 
colnames(initial_data_set1)[9] <- paste("Title_I") 
colnames(initial_data_set2)[9] <- paste("Title_I") 
colnames(initial_data_set1)[11] <- paste("Disadvantaged")
colnames(initial_data_set2)[11] <- paste("Disadvantaged")
colnames(initial_data_set2)[5] <- paste("Gender")
colnames(initial_data_set1)[5] <- paste("Gender")
initial_data_set1$Year <- as.character(2017)
initial_data_set2$Year <- as.character(2016)
initial_data_set2[,c("Math_Time")] <- 0

prep_data <- initial_data_set1[,c(1:37, 40:41)] #drop the last 3 columns of the 2017-2016 data set

prep_data$Math_Time <- ifelse(is.na(prep_data$Math_Time) == 1, 0, prep_data$Math_Time)

#eliminate grade levels outside the testing field (3-6)

prep_data <- subset(prep_data, Grade <= 6) 
prep_data <- subset(prep_data, Grade >= 3)

prep_data$EL <- ifelse(prep_data$EL != 'Y', "0", "Y")
prep_data <- rbind(prep_data, initial_data_set2) #join the tables together
prep_data <- subset(prep_data, 
                    Title_I != "#N/A") #eliminate students who have no demographic data
prep_data$SiteId <- as.factor(prep_data$SiteId)
prep_data$Grade <- as.factor(prep_data$Grade)
prep_data$EL <- ifelse(prep_data$EL != 'Y', "N", "Y")
prep_data$SPED <- ifelse(prep_data$SPED != 'Y', "N", "Y")
prep_data$Disadvantaged <- ifelse(prep_data$Disadvantaged != 'Y', "N", "Y")

prep_data$Gender_year <- as.factor(paste(prep_data$Gender, prep_data$Year))
prep_data$EL_year <- as.factor(paste(prep_data$EL, prep_data$Year))
prep_data$Disadvantaged_year <- as.factor(paste(prep_data$Disadvantaged, prep_data$Year))
prep_data$SPED_year <- as.factor(paste(prep_data$SPED, prep_data$Year))
prep_data$SPED_DIS <- as.factor(paste(prep_data$SPED, prep_data$Disadvantaged, prep_data$Year))
prep_data$Ethnicity_year <- as.factor(paste(prep_data$Ethnicity, prep_data$Year))

##################################### Testing - Chpater 3 #######################################

t.test(GradeScore_G ~ Year, data = prep_data) #overall test

ggplot(prep_data,aes(x=GradeScore_G,group=Year,fill=Year))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

gender_test <- aov(GradeScore_G ~ Gender_year, data = prep_data) #Gender test
summary(gender_test)
TukeyHSD(gender_test)

boxplot(prep_data$GradeScore_G ~ prep_data$Gender_year, col=c("red","blue", "yellow", "green"), 
        main = "Gender by Year")

EL_test <- aov(GradeScore_G ~ prep_data$EL_year, data = prep_data) #EL test
summary(EL_test)
TukeyHSD(EL_test)

boxplot(prep_data$GradeScore_G ~ prep_data$EL_year, col=c("red","blue", "yellow", "green"), 
        main = "English Language by Year")

dis_test <- aov(GradeScore_G ~ prep_data$Disadvantaged_year, data = prep_data) #Dis test
summary(dis_test)
TukeyHSD(dis_test)

boxplot(prep_data$GradeScore_G ~ prep_data$Disadvantaged_year, col=c("red","blue", "yellow", "green"), 
        main = "Economically Disadvantaged by Year")

SPED_test <- aov(GradeScore_G ~ prep_data$SPED_year, data = prep_data) #SPED test
summary(SPED_test)
TukeyHSD(SPED_test)

boxplot(prep_data$GradeScore_G ~ prep_data$SPED_year, col=c("red","blue", "yellow", "green"), 
        main = "Special Ed by Year")

Ethn_test <- aov(GradeScore_G ~ prep_data$Ethnicity_year, data = prep_data) #Ethnicity test
summary(Ethn_test)
TukeyHSD(Ethn_test)

boxplot(prep_data$GradeScore_G ~ prep_data$Ethnicity_year, col=c(2:16), 
        main = "Ethnicity by Year")

summary(prep_data) #creates a set of summary statistics in the log counter

################################ Regression Analysis - Chapter 4 ################################

#copy and paste the summary statistics into word or latex for reference
#to reformat copy it into excel

prep_data2 <- subset(prep_data, Year == 2017)
prep_data2 <- prep_data2[, c(1, 5:6, 8, 10:11, 17, 20:25, 38)] #remove unused columns
prep_dataG <- prep_data2[, c(1:8, 14)] #Table for GradeScore Regression

#This is the construction of the linear model to predict change in GradeScore
#lmer means random effect linear model 
#we create and name the model GradeScore, then look at the summary statistics

#Overall Grade Score Model
GradeScoreM <- lmer(GradeScore_G ~ Gender + EL + Disadvantaged + SPED + Ethnicity
                    + Math_Time + (1|SiteId) + (1|Grade), data = prep_dataG)
summary(GradeScoreM)
plot(GradeScoreM, main = "Residual plot for Math_Time model")
r.squaredGLMM(GradeScoreM)

Ethn_test <- aov(Math_Time ~ Ethnicity, data = prep_dataG) #Ethnicity test
summary(Ethn_test)
TukeyHSD(Ethn_test)

boxplot(prep_dataG$Math_Time ~ prep_dataG$Ethnicity, col=c(2:8), 
        main = "Math Time by Ethnicity ")

boxplot(prep_dataG$Math_Time ~ prep_dataG$Disadvantaged, col=c(2:8), 
        main = "Math Time by Disadvantaged")

boxplot(prep_dataG$Math_Time ~ prep_dataG$Gender, col=c(2:8), 
        main = "Math Time by Gender")

boxplot(prep_dataG$Math_Time ~ prep_dataG$SPED, col=c(2:8), 
        main = "Math Time by Special Ed")



