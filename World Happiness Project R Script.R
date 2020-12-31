############################################################################################
#####This is the R Script corresponding to the World Happiness Report Project of the Harvard 
#####EDX Professional Certificate in Data Science Capstone Course
############################################################################################

if(!require(tidyverse)) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr",repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl",repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart",repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot",repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra",repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr",repos = "http://cran.us.r-project.org")
if(!require(rworldmap)) install.packages("rworldmap",repos = "http://cran.us.r-project.org")

library(readxl) # for interpreting excel data
library(tidyverse) # for tidying the data
library(caret) # for training regression models
library(dplyr) # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ggplot2) # data visualization
library(kableExtra) # for table formatting
library(knitr) # for report generating
library(rworldmap) # for glabal mapping

#Set the working directory
setwd("/Users/janefarris/Documents/Academics/Harvard/World Happiness Project/")
#Load the data from the world happiness report 2019
happiness_data <- read_xls('./World-Happiness-Report-Data.xls')

#Select the columns (variables) we are going to analyze
happiness_data <- happiness_data %>% select(1:11)

#Rename the columns to something more readable 
happiness_data <- happiness_data %>% rename(Country="Country name",
                                            Score="Life Ladder",
                                            Log.GDP="Log GDP per capita",
                                            Social.support="Social support",
                                            Healthy.life.expectancy="Healthy life expectancy at birth",
                                            Perceptions.of.corruption="Perceptions of corruption",
                                            Freedom="Freedom to make life choices",
                                            Positive.affect="Positive affect",
                                            Negative.affect="Negative affect")

#Add a column for the region of each country
happiness_data <- happiness_data %>% mutate(Region = NA)
happiness_data <- happiness_data %>% mutate(Region = 
                                              ifelse(Country %in% c("Finland","Norway","Denmark","Iceland",
                                                                    "Switzerland","Netherlands","Sweden","United Kingdom",
                                                                    "Austria","Ireland","Germany","Belgium","Luxembourg",
                                                                    "Malta","France","Spain","Italy","Northern Cyprus",
                                                                    "Cyprus","Portugal","Greece","North Cyprus"), "Western Europe",Region),
                                            Region =
                                              ifelse(Country %in% c("Canada","United States","Mexico"), "North America",Region),
                                            Region =
                                              ifelse(Country %in% c("New Zealand","Australia"),"Australia and New Zealand",Region),
                                            Region =
                                              ifelse(Country %in% c("Costa Rica","Chile","Panama","Brazil","Argentina",
                                                                    "Guatemala","Uruguay","Colombia","Trinidad & Tobago",
                                                                    "El Salvador","Nicaragua","Ecuador","Belize","Jamaica",
                                                                    "Bolivia","Paraguay","Peru","Dominican Republic",
                                                                    "Venezuela","Suriname","Honduras","Haiti","Puerto Rico",
                                                                    "Trinidad and Tobago","Cuba","Guyana"),
                                                     "Latin America and Caribbean",Region),
                                            Region =
                                              ifelse(Country %in% c("Czech Republic","Slovakia","Poland","Uzbekistan",
                                                                    "Lithuania","Slovenia","Romania","Latvia","Russia",
                                                                    "Kazakhstan","Estonia","Kosovo","Moldova","Turkmenistan",
                                                                    "Hungary","Serbia","Croatia","Montenegro","Azerbaijan",
                                                                    "Belarus","Kyrgyzstan","Macedonia","Albania",
                                                                    "Bosnia and Herzegovina","Tajikistan","Ukraine",
                                                                    "Armenia","Georgia","Bulgaria"),
                                                     "Central and Eastern Europe",Region),
                                            Region =
                                              ifelse(Country %in% c("Mauritius","Nigeria","Zambia","Somaliland region",
                                                                    "Mozambique","Lesotho","Swaziland","South Africa",
                                                                    "Ghana","Zimbabwe","Liberia","Sudan","Congo (Kinshasa)",
                                                                    "Ethiopia","Sierra Leone","Mauritania","Kenya",
                                                                    "Djibouti","Botswana","Malawi","Cameroon","Angola",
                                                                    "Mali","Congo (Brazzaville)","Comoros","Uganda",
                                                                    "Senegal","Gabon","Niger","Tanzania","Madagascar",
                                                                    "Central African Republic","Chad","Guinea",
                                                                    "Ivory Coast","Burkina Faso","Rwanda","Benin","Somalia",
                                                                    "Namibia","South Sudan","Gambia"),"Sub-Saharan Africa",Region),
                                            Region =
                                              ifelse(Country %in% c("Israel","United Arab Emirates","Qatar","Saudi Arabia",
                                                                    "Bahrain","Kuwait","Libya","Turkey","Lebanon","Algeria",
                                                                    "Morocco","Oman","Jordan","Tunisia",
                                                                    "Palestinian Territories","Iran","Iraq","Egypt",
                                                                    "Yemen","Syria","Burundi","Togo"),
                                                     "Middle East and Northern Africa",Region),
                                            Region =
                                              ifelse(Country %in% c("Singapore","Malaysia","Thailand","Philippines",
                                                                    "Indonesia","Vietnam","Laos","Myanmar","Cambodia"),
                                                     "Southeastern Asia",Region),
                                            Region =
                                              ifelse(Country %in% c("Pakistan","Bhutan","Bangladesh","India","Nepal",
                                                                    "Sri Lanka","Afghanistan"),"Southern Asia",Region),
                                            Region =
                                              ifelse(Country %in% c("Taiwan","Japan","South Korea","Hong Kong",
                                                                    "China","Mongolia","Hong Kong S.A.R.", "Hong Kong S.A.R. of China","China",
                                                                    "Taiwan Province of China"),
                                                     "Eastern Asia",Region))

#Check to make sure every country is categorized in a region
sum(is.na(happiness_data$Region))

#Dataset size
dim(happiness_data)

#How many countries are in our dataset
length(unique(happiness_data$Country))
#How many regions are in our dataset
length(unique(happiness_data$Region))

#Range of world happiness reports
min(happiness_data$Year)
max(happiness_data$Year)

#Average World Happiness
happiness_data %>% group_by(Year) %>% summarise("Average Happiness Score" = mean(Score)) %>% knitr::kable() %>%
  kable_styling(position="center")


#Distribution of Countries in Each Region
#Unique country data
unique_happiness <- happiness_data %>% group_by(Region,Country) %>% summarise(count=n())

#Disribution of Regions
unique_happiness %>% 
  select(Region, Country) %>%
  group_by(Region) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(reorder(Region, count),count)) +
  geom_bar(stat="identity",color="dark grey",fill="sky blue") +
  coord_flip(y=c(0, 45)) +
  geom_text(aes(label= count),hjust=-0.2, size=2) +
  labs(x="Region",y="Count",title="Distribution of Countries in Each Region") +
  theme_minimal()

#Happiest Countries
#Happiest Countries in each of the years in the 14 year period
Happiest_2005 <- happiness_data %>% filter(Year==2005) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2006 <- happiness_data %>% filter(Year==2006) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2007 <- happiness_data %>% filter(Year==2007) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2008 <- happiness_data %>% filter(Year==2008) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2009 <- happiness_data %>% filter(Year==2009) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2010 <- happiness_data %>% filter(Year==2010) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2011 <- happiness_data %>% filter(Year==2011) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2012 <- happiness_data %>% filter(Year==2012) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2013 <- happiness_data %>% filter(Year==2013) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2014 <- happiness_data %>% filter(Year==2014) %>% arrange(desc(Score)) %>% select(Country)
Happiest_2015 <- happiness_data %>% filter(Year==2015) %>% arrange(desc(Score)) %>% select(Country) 
Happiest_2016 <- happiness_data %>% filter(Year==2016) %>% arrange(desc(Score)) %>% select(Country) 
Happiest_2017 <- happiness_data %>% filter(Year==2017) %>% arrange(desc(Score)) %>% select(Country) 
Happiest_2018 <- happiness_data %>% filter(Year==2018) %>% arrange(desc(Score)) %>% select(Country) 

tab1 <- cbind(1:10,Happiest_2005[1:10,],Happiest_2006[1:10,],Happiest_2007[1:10,],Happiest_2008[1:10,],
Happiest_2009[1:10,],Happiest_2010[1:10,],Happiest_2011[1:10,]) %>%
knitr::kable(col.names = c("Rank","2005","2006","2007","2008","2009","2010","2011")) %>%
kable_styling(font_size=7)
tab2 <- cbind(Happiest_2012[1:10,],Happiest_2013[1:10,],Happiest_2014[1:10,],Happiest_2015[1:10,],
Happiest_2016[1:10,],Happiest_2017[1:10,],Happiest_2018[1:10,]) %>%
knitr::kable(col.names = c("2012","2013","2014","2015","2016","2017","2018")) %>% kable_styling(font_size=7)
tab1
tab2

#Least Happy Countries
#Least Happy Countries in each of the years in the 14 year period
Bottom_2005 <- happiness_data %>% filter(Year==2005) %>% arrange(Score) %>% select(Country)
Bottom_2006 <- happiness_data %>% filter(Year==2006) %>% arrange(Score) %>% select(Country)
Bottom_2007 <- happiness_data %>% filter(Year==2007) %>% arrange(Score) %>% select(Country)
Bottom_2008 <- happiness_data %>% filter(Year==2008) %>% arrange(Score) %>% select(Country)
Bottom_2009 <- happiness_data %>% filter(Year==2009) %>% arrange(Score) %>% select(Country)
Bottom_2010 <- happiness_data %>% filter(Year==2010) %>% arrange(Score) %>% select(Country)
Bottom_2011 <- happiness_data %>% filter(Year==2011) %>% arrange(Score) %>% select(Country)
Bottom_2012 <- happiness_data %>% filter(Year==2012) %>% arrange(Score) %>% select(Country)
Bottom_2013 <- happiness_data %>% filter(Year==2013) %>% arrange(Score) %>% select(Country)
Bottom_2014 <- happiness_data %>% filter(Year==2014) %>% arrange(Score) %>% select(Country)
Bottom_2015 <- happiness_data %>% filter(Year==2015) %>% arrange(Score) %>% select(Country)
Bottom_2016 <- happiness_data %>% filter(Year==2016) %>% arrange(Score) %>% select(Country)
Bottom_2017 <- happiness_data %>% filter(Year==2017) %>% arrange(Score) %>% select(Country)
Bottom_2018 <- happiness_data %>% filter(Year==2018) %>% arrange(Score) %>% select(Country)

tab3 <- cbind(1:10,Bottom_2005[1:10,],Bottom_2006[1:10,],Bottom_2007[1:10,],Bottom_2008[1:10,],
Bottom_2009[1:10,],Bottom_2010[1:10,],Bottom_2011[1:10,]) %>%
knitr::kable(col.names = c("Rank","2005","2006","2007","2008","2009","2010","2011")) %>%
kable_styling(font_size=5.5)
tab4 <- cbind(Bottom_2012[1:10,],Bottom_2013[1:10,],Bottom_2014[1:10,],Bottom_2015[1:10,],Bottom_2016[1:10,],
Bottom_2017[1:10,],Bottom_2018[1:10,]) %>%
knitr::kable(col.names = c("2012","2013","2014","2015","2016","2017","2018")) %>% kable_styling(font_size=7)
tab3
tab4

##Data Visualiztion
#Happiness By Region
#Average happiness score across the world between 2005-2018
avg_score <- mean(happiness_data$Score)

#Average Happiness Score By Region
happiness_data %>%
group_by(Region) %>%
summarise(Average_Score = mean(Score)) %>%
ggplot(aes(reorder(Region,Average_Score),Average_Score)) +
geom_bar(stat="identity",color="dark grey",fill="sky blue") +
coord_flip(y=c(0, 8)) +
geom_text(aes(label= round(Average_Score,3)),hjust=-0.1, size=3) +
labs(x="Region",y="Average Score",title="Average Happiness Score By Region") +
theme_minimal()

#Find the average score across the world in each of the 14 years segmented by region
scores_2005 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2005) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2005)
scores_2006 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2006) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2006)
scores_2007 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2007) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2007)
scores_2008 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2008) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2008)
scores_2009 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2009) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2009)
scores_2010 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2010) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2010)
scores_2011 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2011) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2011)
scores_2012 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2012) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2012)
scores_2013 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2013) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2013)
scores_2014 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2014) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2014)
scores_2015 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2015) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2015)
scores_2016 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2016) %>%
summarise(Average_Score=mean(Score)) %>% 
mutate(Year=2016)
scores_2017 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2017) %>%
summarise(Average_Score=mean(Score)) %>% 
mutate(Year=2017)
scores_2018 <- happiness_data %>% 
group_by(Region) %>% 
filter(Year==2018) %>%
summarise(Average_Score=mean(Score)) %>%
mutate(Year=2018)

tab <- rbind(scores_2005,scores_2006,scores_2007,scores_2008,scores_2009,scores_2010,scores_2011,scores_2012,scores_2013,scores_2014,scores_2015,scores_2016,scores_2017,scores_2018)
tab %>%
group_by(Year) %>%
ggplot(aes(Year, Average_Score,color=Region)) +
geom_line(stat="identity") +
geom_point() +
labs(x="Year of Report",y="Happiness Score",title="Happiness Scores from 2005-2018") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))

#Happiness Across the Globe
#Create a country Map
#Create a subset of our data to include only 2018 scores
data2018 <- happiness_data %>% filter(Year==2018)
myCountries <- unique(data2018$Country)

#data frame with the country names plus the score variavle to merge with the map data
DF <- data.frame(country = myCountries,Score = data2018$Score)

Map <- joinCountryData2Map(DF, joinCode = "NAME",nameJoinColumn = "country")
#Join malDF data frame to the country map data to plot

mapCountryData(Map, nameColumnToPlot="Score", catMethod = c(2:8),
missingCountryCol = gray(.8), mapTitle = "Happiness Score Across the World in 2018",
colourPalette = c("yellow","orange","red"),addLegend = TRUE)

happiness_data %>%
  group_by(Region) %>%
  ggplot(aes(Log.GDP,Score,color=Region)) +
  geom_point() +
  labs(x="Economy (GDP Per Capita)",y="Happiness Score",title="Log GDP Per Capita vs. Happiness Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Healthy Life Expectancy vs. Happiness
happiness_data %>%
group_by(Region) %>%
ggplot(aes(Healthy.life.expectancy,Score,col=Region)) +
geom_point() +
labs(x="Healthy Life Expectancy",y="Happiness Score",title="Healthy Life Expectancy vs. Happiness Score") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))

#Social Support vs. Happiness
happiness_data %>%
group_by(Region) %>%
ggplot(aes(Social.support,Score,col=Region)) +
geom_point() +
labs(x="Social Support",y="Happiness Score",title="Social Support vs. Happiness Score") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))

#Freedom vs. Happiness
happiness_data %>%
group_by(Region) %>%
ggplot(aes(Freedom,Score,col=Region)) +
geom_point() +
labs(x="Freedom Level",y="Happiness Score",title="Freedom Level vs. Happiness Score") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))

#Perceptions of Corruption vs. Happiness
happiness_data %>%
group_by(Region) %>%
ggplot(aes(as.numeric(Perceptions.of.corruption),Score,col=Region)) +
geom_point() +
labs(x="Perceptions of Corruption",y="Happiness Score",title="Perceptions of Corruption vs. Happiness Score") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))

#Generosity vs. Happiness
happiness_data %>%
group_by(Region) %>%
ggplot(aes(Generosity,Score,col=Region)) +
geom_point() +
labs(x="Generosity Level",y="Happiness Score",title="Generosity vs. Happiness Score") +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))

##Modeling
#Defining RMSE
#Train & Test Sets
test_index <- createDataPartition(happiness_data$Score, times = 1, p = 0.25, list = FALSE)
test_set <- happiness_data[test_index, ]
train_set <- happiness_data[-test_index, ]

#Replace test set NAs with column means from our training set
col_means <- lapply(train_set[,2:11], mean, na.rm = TRUE)
test_set <- replace_na(test_set, col_means)

#Create Root Mean Squared Error (RMSE) function
rmse <- function(true_scores, predicted_scores){
sqrt(mean((true_scores - predicted_scores)^2))}

###Baseline Model
#Calculate the average happiness score of all countries in the training set
avg_score <- mean(train_set$Score)

#Calculate RMSE using the average score as the predicted value for each country 
#and the scores from the final hold out validation set as the observed values
rmse(test_set$Score,avg_score)

###Model 1: Linear Model

#Define train control for 10 fold cross validation
set.seed(1)
train_control <- trainControl(method="cv", number=10)

#Train a linear model using 10-fold cross validation 
#and make predictions on each of the k subsets
model1 <- train(Score~Log.GDP+Social.support+Healthy.life.expectancy+Freedom+
                  Perceptions.of.corruption+Generosity+Year+Region,
                na.action=na.exclude,
                data=happiness_data,
                trControl=train_control, #Generate training and test sets, with cross validation
                method="lm")
#Compute the prediction error RMSE
print(model1)

###Model 2: Decision (Regression) Tree

#Fully grown regression tree
model2 <- rpart(Score ~ Log.GDP+Social.support+Healthy.life.expectancy+Freedom+
                  Perceptions.of.corruption+Generosity+Year+Region, 
                method = "anova",
                data = train_set)
#visualize the splits 
rpart.plot(model2)
#Get score predictions 
predicted_scores <- predict(model2, test_set)
#Compute the prediction error RMSE
rmse(test_set$Score,predicted_scores)

#The most important factors in determining score (according to the features in our model)
model2$variable.importance
#Find the optimal cost complexity parameter that was used in the decision tree
plotcp(model2)

###Model 3: Random Forest

#Train multiple regression trees and average their errors
#use tuneGrid and cross validation to optimize the tuning parameter 
model3 <- train(Score~Log.GDP+Social.support+Healthy.life.expectancy+Freedom+
Perceptions.of.corruption+Generosity+Year+Region, 
method = "rf",
tuneGrid = data.frame(mtry = c(3,5,7,9)),
trControl=train_control, #10-fold cross validation to generate different trees
importance=TRUE, #allows to inspect variable importance
data = train_set, 
na.action=na.exclude)

#Plot the tuning parameter vs. RMSE it achieves
plot(model3)
#Find the opitmal tuning parameter mtry 
#mtry is the number of variables randomly sampled as candidates at each split in the tree
model3$bestTune
#Final model developed
model3$finalModel

#Make predictions on the test data
predicted_scores <- predict(model3, test_set)
#Compute the prediction error RMSE
rmse(test_set$Score,predicted_scores)


###Model 4: K Nearest Neighbors (KNN)

#First we must split up our training set to create a separate label training set
#Isolate the dependent variable
train_y <- train_set %>% na.exclude() %>% select(Score)
#Remove the non-numeric factors and NA values
train_x <- train_set %>% select(-Score,-Year,-Country,-Region) %>% na.exclude()

model4 <- train(train_x, train_y$Score,
                method="knn", 
                trControl = trainControl("cv", number = 10),
                preProcess = c("center","scale"),
                tuneLength = 10)
#Find the optimal value of the tuning parameter (how many neighbors to use)
model4$bestTune
#Make predictions on the test data
predicted_scores <- predict(model4, test_set)
#Compute the prediction error RMSE
rmse(test_set$Score,predicted_scores)

#Results
#Set the number of significant figures for the RMSE
options(pillar.sigfig = 7)
#Create a result table
results <- tibble(Method = "Baseline Model", RMSE = 1.11129)
results <- results %>% 
  add_row(Method="Linear Model", RMSE = 0.5267627) %>%
  add_row(Method="Decision (Regression) Tree", RMSE = 0.5568067) %>% 
  add_row(Method="Random Forest", RMSE = 0.4107367) %>% 
  add_row(Method="K Nearest Neighbors", RMSE = 0.4791718)

results %>% knitr::kable() %>% kable_styling(position="center")