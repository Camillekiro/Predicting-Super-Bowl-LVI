library(readxl) #package used to extract excel file
library(caTools) #package used to split the data into test/train set
library(stats) #package for linear regression model

#the data was filtered in excel using the Team_abbrev variable for CIN & LAR.
#used to predict home_score
defense_data <- read_excel("nfl_dst_raw_data.xls", sheet = "defense_ft")
View(defense_data) 

#renaming the data set to predict vis_score
defense_data_beng <- read_excel("nfl_dst_raw_data.xls", sheet = "defense_ft")

#looking for missing values
sapply(defense_data, function(x) sum(is.na(x)))

#for vis_score
sapply(defense_data_beng, function(x) sum(is.na(x)))

##removing unnecessary columns
defense_data[,1:2]<-list(NULL)
defense_data$Team_abbrev <- NULL
defense_data$Opponent_abbrev <- NULL
defense_data$vis_team <- NULL
defense_data$home_team <- NULL
defense_data$OT<- NULL
defense_data$Roof <- NULL
defense_data$Surface <- NULL
defense_data$Vegas_Favorite<- NULL
defense_data$game_date<- NULL
defense_data$vis_score <- NULL
#removed vis_score due to collinearity with home_score (dependent variable)

##removing unnecessary columns
defense_data_beng[,1:2]<-list(NULL)
defense_data_beng$Team_abbrev <- NULL
defense_data_beng$Opponent_abbrev <- NULL
defense_data_beng$vis_team <- NULL
defense_data_beng$home_team <- NULL
defense_data_beng$OT<- NULL
defense_data_beng$Roof <- NULL
defense_data_beng$Surface <- NULL
defense_data_beng$Vegas_Favorite<- NULL
defense_data_beng$game_date<- NULL
defense_data_beng$home_score<- NULL

defense_data_beng$points_allowed_0 <- NULL
defense_data_beng$points_allowed_1_6 <- NULL
defense_data_beng$points_allowed_7_13 <- NULL
defense_data_beng$points_allowed_14_20 <- NULL
defense_data_beng$points_allowed_21_27 <- NULL
defense_data_beng$points_allowed_28_34 <- NULL
defense_data_beng$points_allowed_35 <- NULL

#Splitting the data into training set and testing set
split2 = sample.split(defense_data$home_score, SplitRatio = 0.8)

train_set = subset(defense_data, split2 == TRUE)
test_set = subset(defense_data, split2 == FALSE)

#linear regression models
Modelrams1 <- lm(home_score ~.,data = train_set)
print(summary(Modelrams1))

#final reduced model only using statirically significant variable
#did not include  fumbles_rec_td due to low mean
Modelrams2 <- lm(home_score ~ def_int_td + fumbles_rec + points_allowed_7_13 + points_allowed_14_20, data = train_set)
print(summary(Modelrams2))
coef(Modelrams2)

#calculation for home_score using the mean values of the variables
24.9 + (8.6 * 0.14) + (2.9*0.36) + (-8.6 *0.17) + (-6.4*0.39)
#results in 23.19 points for rams which is greater than 23.11

#initial model's coefficienct values multiplied by the mean of the variables
25.16 + (6.21 * 0.14) + (2.87*0.36) + (-10 *0.17) + (-5*0.39)

#splitting for vis_score
split = sample.split(defense_data_beng$vis_score, SplitRatio = 0.8)

train_set1 = subset(defense_data_beng, split == TRUE)
test_set1 = subset(defense_data_beng, split == FALSE)

#first model with vis_score as dependent variable and all other variables as independent

Model1 <- lm(vis_score ~.,data = train_set1)
print(summary(Model1))

#Final reduced model only using statirically significant variable
Model2 <- lm(vis_score ~ sacks, data = train_set1)
print(summary(Model2))
coef(Model2)

#calculation for vis_score
22.1 + (0.37*2.75)
#results in 23.11
#initial model resulted in sacks as the only significant variable for vis_sscore
21.53 + (0.57*2.75)
#vis_score result: 23.097



#data source:
#https://www.advancedsportsanalytics.com/nfl-raw-data






