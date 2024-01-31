#TO 567: Final Project
#Prepared by: Group 11

#===================================================================================================================
#Part 1: Import data into R

#Import CSV file into R as data frame
tele <- read.csv("tele.csv", stringsAsFactors=TRUE)

#View structure and summary
str(tele)
summary(tele)

#===================================================================================================================
#Part 2: Prepare and  lean data for visualization and clustering

#Create histogram of age brackets
hist(tele$age, xlab="Age Bracket", ylab="# of Calls",
     main = "Histogram of Age Bracket", breaks=seq(0,100,by=10))

#Create new column to indicate age bracket of clients who were called
tele$age_bracket <- NA   #Default to NA
tele$age_bracket[tele$age >= 70] <- "70+"
tele$age_bracket[tele$age < 70 & tele$age >= 60] <- "60-70"
tele$age_bracket[tele$age < 60 & tele$age >= 50] <- "50-60"
tele$age_bracket[tele$age < 50 & tele$age >= 40] <- "40-50"
tele$age_bracket[tele$age < 40 & tele$age >= 30] <- "30-40"
tele$age_bracket[tele$age < 30 & tele$age >= 20] <- "20-30"
tele$age_bracket[tele$age < 20] <- "<20"
tele$age_bracket <- as.factor(tele$age_bracket)   #Convert to factor
summary(tele$age_bracket)   #View summary

#Create histogram of Number of days that passed by after the client was last contacted from a previous campaign
hist(tele[tele$pdays < 999, ]$pdays,
     xlab="# of days passed since client was last contacted",
     ylab="# of Calls", main = "Histogram of Days Passed")

#Create new "pdays_range" column to better represent "pdays" and adjust for 999 values
tele$pdays_range <- "unknown"   
tele$pdays_range[tele$pdays < 999 & tele$pdays  >= 28] <- "> 28 days"  #Over 4 weeks
tele$pdays_range[tele$pdays < 28 & tele$pdays  >= 21] <- "21-28 days"  #3-4 weeks
tele$pdays_range[tele$pdays < 21 & tele$pdays  >= 14] <- "14-21 days"  #2-3 weeks
tele$pdays_range[tele$pdays < 14 & tele$pdays  >= 7] <- "7-14 days"    #1-2 weeks
tele$pdays_range[tele$pdays < 7] <- "< 7 days"                         #Less than 1 week    
tele$pdays_range <- as.factor(tele$pdays_range)                        #Convert to factor
summary(tele$pdays_range)   #View summary

#Add column to "tele" dataframe; convert "y" field (factor) to binary numbers
tele$outcome <- NA
tele$outcome[tele$y == "yes"] <- 1
tele$outcome[tele$y == "no"] <- 0

#===================================================================================================================
#Part 3: Data exploration and exploratory analysis

#Load dplyr
library("dplyr")

#Load ggplot2
library(ggplot2)

#-------------------------------------------------------------------------------------------------------------------
#Analyze success rate by occupation type

#Calculate success rate by occupation type
success_rate_by_occupation_type <- tele %>%
  group_by(job) %>%
  select(job, outcome) %>%
  summarise(
    avg_success_rate = round(mean(outcome, na.rm = TRUE),3)
  )

#Plot success rate by occupation type in bar chart; sort bars in descending order
ggplot(data = success_rate_by_occupation_type, aes(x= reorder(job, -avg_success_rate), y= avg_success_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Occupation Type") +
  ylab("Average Success Rate") + 
  ggtitle("Sales Success Rate by Occupation Type") + 
  geom_text(aes(label = scales::percent(avg_success_rate), vjust=-0.25))

#Count number of observations by occupation type
count_by_occupation_type <- tele %>%
  group_by(job) %>%
  summarise(n = n())

#Merge "success rate by occupation type" and "count by occupation type" data frames together
occupation_type_stats <- merge(x= success_rate_by_occupation_type, y = count_by_occupation_type, by="job")
#Sort new data frame in descending order based on success rate
occupation_type_stats <- arrange(occupation_type_stats, desc(avg_success_rate)) 
#Add new column to data frame to represent count of occupation type as % of all observations
occupation_type_stats$pct_total <- round(occupation_type_stats$n / nrow(tele),3)
#Print summary table
occupation_type_stats

#-------------------------------------------------------------------------------------------------------------------
#Analyze success rate by age range

#Calculate success rate by age range
success_rate_by_age_range <- tele %>%
  group_by(age_bracket) %>%
  select(age_bracket, outcome) %>%
  summarise(
    avg_success_rate = round(mean(outcome, na.rm = TRUE),3)
  )

#Plot success rate by age range in bar chart
ggplot(data = success_rate_by_age_range, aes(x= age_bracket, y= avg_success_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Age Range") +
  ylab("Average Success Rate") + 
  ggtitle("Sales Success Rate by Age Range") + 
  geom_text(aes(label = scales::percent(avg_success_rate), vjust=-0.25))

#Count number of observations by age range
count_by_age_range <- tele %>%
  group_by(age_bracket) %>%
  summarise(n = n())

#Merge "success rate by age range" and "count by age range" data frames together
age_range_stats <- merge(x= success_rate_by_age_range, y = count_by_age_range, by="age_bracket")
#Add new column to data frame to represent count of age range as % of all observations
age_range_stats$pct_total <- round(age_range_stats$n / nrow(tele),3)
#Print summary table
age_range_stats

#-------------------------------------------------------------------------------------------------------------------
#Analyze success rate by education level

#Calculate success rate by education level
success_rate_by_education_level <- tele %>%
  group_by(education) %>%
  select(education, outcome) %>%
  summarise(
    avg_success_rate = round(mean(outcome, na.rm = TRUE),3)
  )

#Plot success rate by education level in bar chart; sort bars in descending order
ggplot(data = success_rate_by_education_level, aes(x= reorder(education, -avg_success_rate), y= avg_success_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Education Level") +
  ylab("Average Success Rate") + 
  ggtitle("Sales Success Rate by Education level") + 
  geom_text(aes(label = scales::percent(avg_success_rate), vjust=-0.25))


#Count number of observations by education level
count_by_education_level <- tele %>%
  group_by(education) %>%
  summarise(n = n())

#Merge "success rate by education level" and "count by education level" data frames together
education_level_stats <- merge(x= success_rate_by_education_level, y = count_by_education_level, by="education")
#Sort new data frame in descending order based on success rate
education_level_stats <- arrange(education_level_stats, desc(avg_success_rate)) 
#Add new column to data frame to represent count of education level as % of all observations
education_level_stats$pct_total <- round(education_level_stats$n / nrow(tele),3)
#Print summary table
education_level_stats

#-------------------------------------------------------------------------------------------------------------------
#Analyze success rate by marital status

#Calculate success rate by marital status
success_rate_by_marital_status <- tele %>%
  group_by(marital) %>%
  select(marital, outcome) %>%
  summarise(
    avg_success_rate = round(mean(outcome, na.rm = TRUE),3)
  )

#Plot success rate by marital status in bar chart; sort bars in descending order
ggplot(data = success_rate_by_marital_status, aes(x= reorder(marital, -avg_success_rate), y= avg_success_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Marital Status") +
  ylab("Average Success Rate") + 
  ggtitle("Sales Success Rate by Marital Status") + 
  geom_text(aes(label = scales::percent(avg_success_rate), vjust=-0.25))

#Count number of observations by marital status
count_by_marital_status <- tele %>%
  group_by(marital) %>%
  summarise(n = n())

#Merge "success rate by marital status" and "count by marital status" data frames together
marital_status_stats <- merge(x= success_rate_by_marital_status, y = count_by_marital_status, by="marital")
#Sort new data frame in descending order based on success rate
marital_status_stats <- arrange(marital_status_stats, desc(avg_success_rate)) 
#Add new column to data frame to represent count of marital status as % of all observations
marital_status_stats$pct_total <- round(marital_status_stats$n / nrow(tele),3)
#Print summary table
marital_status_stats

#-------------------------------------------------------------------------------------------------------------------
#Analyze success rate by credit default status
success_rate_by_credit_default_status <- tele %>%
  group_by(default) %>%
  select(default, outcome) %>%
  summarise(
    avg_success_rate = round(mean(outcome, na.rm = TRUE),3)
  )

#Calculate success rate as function of default status
success_rate_by_credit_default_status

#Calculate each credit default status option as % of all observations
round(prop.table(table(tele$default)),3)

#---

#Analyze success rate by housing loan status
success_rate_by_housing_loan_status <- tele %>%
  group_by(housing) %>%
  select(housing, outcome) %>%
  summarise(
    avg_success_rate = round(mean(outcome, na.rm = TRUE),3)
  )

#Calculate success rate as function of housing loan status
success_rate_by_housing_loan_status

#Calculate each housing loan status option as % of all observations
round(prop.table(table(tele$housing)),3)

#---

#Analyze success rate by personal loan status
success_rate_by_personal_loan_status <- tele %>%
  group_by(loan) %>%
  select(loan, outcome) %>%
  summarise(
    avg_success_rate = round(mean(outcome, na.rm = TRUE),3)
  )

#Calculate success rate as function of personal loan status
success_rate_by_personal_loan_status

#Calculate each personal loan status option as % of all observations
round(prop.table(table(tele$loan)),3)

#===================================================================================================================
#Part 4: Prepare data for k-means clustering analysis 

#Retrieve fields most relevant for clustering clients into segments; store selected fields in "abridged" data frame;
#ignore all other data. Rationale: if we receive a new list of clients we have never called before, these are the 
#key factors in deciding who to call
abridged_tele_data <- tele[,c("age_bracket","job", "marital", "education", "default", "housing", "loan")]
str(abridged_tele_data)

#Convert factors to dummy variables; do not eliminate any fields yet for the "base case" 
tele_data_with_dummy_var <- as.data.frame(model.matrix(~ ., data = abridged_tele_data, contrasts.arg = lapply(abridged_tele_data[,], contrasts, contrasts=FALSE)))
str(tele_data_with_dummy_var)

#Retrieve fields needed for k-means analysis; the base case for the "age bracket" field is "<20" while the base case for
#all other fields is "unknown" (this includes job type, education level, martial status, credit default status, 
#housing loan status , and personal loan status)
abridged_tele_data_with_dummy_var <- tele_data_with_dummy_var[,c(3:19, 21:23, 25:31, 33, 35:36, 38:39, 41)]
str(abridged_tele_data_with_dummy_var)

#Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
tele_data_with_dummy_var_normalized <- as.data.frame(lapply(abridged_tele_data_with_dummy_var, normalize))
str(tele_data_with_dummy_var_normalized)



#===================================================================================================================
#Part 5: Determine ideal number of clusters for k-means algorithm

#Load factoextra
library(factoextra)

#Determine ideal number of clusters

#Since factoextra is unable to run data set with ~41k observation, reduce data set to a 33% random sample
set.seed(12345)
normalized_33_percent_random_sample <- tele_data_with_dummy_var_normalized[
  sample(1:nrow(tele_data_with_dummy_var_normalized), round(.33 * nrow(tele_data_with_dummy_var_normalized))),]

#Count size of sample
nrow(normalized_33_percent_random_sample)


#Use WSS method to determine optimal "k" for algorithm
set.seed(12345)
fviz_nbclust(normalized_33_percent_random_sample, kmeans, method = "wss")  #WSS method: use 4 clusters

#Use gap statistic method to determine optimal "k" for algorithm
set.seed(12345)
#fviz_nbclust(normalized_33_percent_random_sample, kmeans, method = "gap_stat") #Gap Stat method: does not converge

#Use silhouette method to determine optimal "k" for algorithm
set.seed(12345)
fviz_nbclust(normalized_33_percent_random_sample, kmeans, method = "silhouette") #Silhouette method: use 2 clusters


#===================================================================================================================
#Part 6: Perform k-means clustering analysis

#Set seed
set.seed(12345)

#Run k-means clustering algorithm with 8 clusters
tele_data_clusters <- kmeans(tele_data_with_dummy_var_normalized, 18)  

#View cluster sizes
tele_data_clusters$size   

#View cluster centers
tele_data_clusters$centers

#Export cluster centers to CSV file
write.csv(tele_data_clusters$centers, "Cluster_Centers_Output.csv")


#Segment customers in original "tele" data frame by cluster according to k-means algorithm
tele$cluster <- tele_data_clusters$cluster

#Validate work
head(tele)
summary(tele)

#===================================================================================================================
#Part 7: Draw insights from clustering analysis

#Summary table for success rate
round(prop.table(table(tele$cluster, tele$y),1),3)
round(prop.table(table(tele$y)),3)

#Summary table for number of successes and failures
table(tele$cluster, tele$y)
table(tele$y)


#===================================================================================================================
#Part 8: Visualize clusters

#Plot clusters without observations but with center; ellipse
fviz_cluster(tele_data_clusters, data = tele_data_with_dummy_var_normalized, geom = "point", shape = 20,
             ellipse.type = "norm", show.clust.cent = TRUE, alpha =0)

#Plot clusters without observations but with center; no ellipse
fviz_cluster(tele_data_clusters, data = tele_data_with_dummy_var_normalized, geom = "point", shape = 20,
             show.clust.cent = TRUE, alpha =0)

#===================================================================================================================
#Part 9: Prepare data for kNN analysis

#Add column of outcomes (y) to normalized tele data
tele_data_for_knn <- tele_data_with_dummy_var_normalized
tele_data_for_knn$outcome <- tele$y
str(tele_data_for_knn)

#Re-arrange all ~41K observations in random order 
set.seed(1234)
all_observations_in_random_order <- tele_data_for_knn[sample(1:nrow(tele_data_for_knn), nrow(tele_data_for_knn)),]
str(all_observations_in_random_order)

#Set starting and ending boundaries for test and train data
train_start <- 1
train_end <- round(nrow(all_observations_in_random_order) * .7,0)
test_start <- train_end + 1
test_end <- round(nrow(all_observations_in_random_order) * 1,0)

#Create training and testing data for x-variables
tele_train <- all_observations_in_random_order[train_start:train_end,1:33 ]
tele_test <- all_observations_in_random_order[test_start:test_end,1:33 ]

#Create training and testing data for y-variable
tele_train_labels <- all_observations_in_random_order[train_start:train_end,34]
tele_test_labels <- all_observations_in_random_order[test_start:test_end,34]

#===================================================================================================================
#Part 10: Perform kNN analysis

#Load class package
library(class)

#Prelim sample size
round(sqrt(nrow(tele_train)),0)

#Run KNN model
set.seed(12345)
tele_test_prediction <- knn(train = tele_train, test = tele_test, cl = tele_train_labels, k=11)

#Load gmodels package
library(gmodels)

#Create confusion matrix
CrossTable(x = tele_test_labels, y = tele_test_prediction, prop.chisq=FALSE)

#===================================================================================================================
