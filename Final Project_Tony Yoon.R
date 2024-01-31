#TO 567: Final individual Project

#===================================================================================================================
#Part 1: Import data into R

#Import CSV file into R as data frame
hr <- read.csv("hrdata.csv", stringsAsFactors=TRUE)

#View structure and summary
str(hr)
summary(hr)

#===================================================================================================================
#Part 2: Data exploration

#Load dplyr
library("dplyr")

#Load ggplot2
library(ggplot2)

#-------------------------------------------------------------------------------------------------------------------
#Analyze turnover rate by department

#Calculate turnover rate by department
turnover_rate_by_department <- hr %>%
  group_by(sales) %>%
  select(sales, left) %>%
  summarise(
    avg_turnover_rate = round(mean(left, na.rm = TRUE),3)
  )

#Plot turnover rate by department in bar chart; sort bars in descending order
ggplot(data = turnover_rate_by_department, aes(x= reorder(sales, -avg_turnover_rate), y= avg_turnover_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Department") +
  ylab("Average Turnover Rate") + 
  ggtitle("Turnover Rate by Department") + 
  geom_text(aes(label = scales::percent(avg_turnover_rate), vjust=-0.25))

#Count number of observations by department
count_by_department <- hr %>%
  group_by(sales) %>%
  summarise(n = n())

#-------------------------------------------------------------------------------------------------------------------
#Analyze turnover rate by salary level

#Calculate turnover rate by salary level
turnover_rate_by_salary <- hr %>%
  group_by(salary) %>%
  select(salary, left) %>%
  summarise(
    avg_turnover_rate = round(mean(left, na.rm = TRUE),3)
  )

#Plot turnover rate by department in bar chart; sort bars in descending order
ggplot(data = turnover_rate_by_salary, aes(x= reorder(salary, -avg_turnover_rate), y= avg_turnover_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Salary Level") +
  ylab("Average Turnover Rate") + 
  ggtitle("Turnover Rate by Salary Level") + 
  geom_text(aes(label = scales::percent(avg_turnover_rate), vjust=-0.25))

#Count number of observations by department
count_by_salary <- hr %>%
  group_by(salary) %>%
  summarise(n = n())

#-------------------------------------------------------------------------------------------------------------------
#Analyze turnover rate by time spend

#Calculate turnover rate by time spend
turnover_rate_by_time <- hr %>%
  group_by(time_spend_company) %>%
  select(time_spend_company, left) %>%
  summarise(
    avg_turnover_rate = round(mean(left, na.rm = TRUE),3)
  )

#Plot turnover rate by time spend in bar chart; sort bars in descending order
ggplot(data = turnover_rate_by_time, aes(x= reorder(time_spend_company, -avg_turnover_rate), y= avg_turnover_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Time Spend") +
  ylab("Average Turnover Rate") + 
  ggtitle("Turnover Rate by Time Spend") + 
  geom_text(aes(label = scales::percent(avg_turnover_rate), vjust=-0.25))

#Count number of observations by department
count_by_time_spend <- hr %>%
  group_by(time_spend_company) %>%
  summarise(n = n())

#-------------------------------------------------------------------------------------------------------------------
#Analyze turnover rate by promotion in last 5 years

#Calculate turnover rate by promotion
turnover_rate_by_promotion <- hr %>%
  group_by(promotion_last_5years) %>%
  select(promotion_last_5years, left) %>%
  summarise(
    avg_turnover_rate = round(mean(left, na.rm = TRUE),3)
  )

#Plot turnover rate by promotion in bar chart; sort bars in descending order
ggplot(data = turnover_rate_by_promotion, aes(x= reorder(promotion_last_5years, -avg_turnover_rate), y= avg_turnover_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Promotion") +
  ylab("Average Turnover Rate") + 
  ggtitle("Turnover Rate by Promotion in 5 years") + 
  geom_text(aes(label = scales::percent(avg_turnover_rate), vjust=-0.25))

#Count number of observations by promotion
count_by_promotion <- hr %>%
  group_by(promotion_last_5years) %>%
  summarise(n = n())

#-------------------------------------------------------------------------------------------------------------------
#Analyze turnover rate by satisfaction level

#Create new column to indicate satisfaction level bracket 
hr$bracket <- NA   #Default to NA
hr$bracket[hr$satisfaction_level >= 0.8] <- "0.8+"
hr$bracket[hr$satisfaction_level < 0.8 & hr$satisfaction_level >=0.7] <- "0.7-0.8"
hr$bracket[hr$satisfaction_level < 0.7 & hr$satisfaction_level >=0.6] <- "0.6-0.7"
hr$bracket[hr$satisfaction_level < 0.6 & hr$satisfaction_level >=0.5] <- "0.5-0.6"
hr$bracket[hr$satisfaction_level < 0.5 & hr$satisfaction_level >=0.4] <- "0.4-0.5"
hr$bracket[hr$satisfaction_level < 0.4 & hr$satisfaction_level >=0.3] <- "0.3-0.4"
hr$bracket[hr$satisfaction_level < 0.3 & hr$satisfaction_level >=0.2] <- "0.2-0.3"
hr$bracket[hr$satisfaction_level < 0.2 ] <- "0.0-0.2"
hr$bracket <- as.factor(hr$bracket)   #Convert to factor
summary(hr$bracket)   #View summary

#Calculate turnover rate by satisfaction level
turnover_rate_by_satisfaction <- hr %>%
  group_by(bracket) %>%
  select(bracket, left) %>%
  summarise(
    avg_turnover_rate = round(mean(left, na.rm = TRUE),3)
  )

ggplot(data = turnover_rate_by_satisfaction, aes(x= bracket, y= avg_turnover_rate)) + 
  geom_bar(stat = "identity", fill = "#1D9A78") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Satisfaction level") +
  ylab("Average Turnover Rate") + 
  ggtitle("Turnover Rate by Satisfaction Level") + 
  geom_text(aes(label = scales::percent(avg_turnover_rate), vjust=-0.25))

#Create histogram
hist(hr$satisfaction_level, xlab="Satisfaction Level", ylab="# of Employees",
     main = "Histogram of Satisfaction Level", breaks=seq(0,1,by=0.1))

#===================================================================================================================
#Part 3: Prepare Clustering Analysis 

#Retrieve fields most relevant for clustering into segments; store selected fields in "abridged" data frame;
#key factors in deciding who will quit
abridged_hr_data <- hr[,c("satisfaction_level","time_spend_company", "promotion_last_5years", "sales", "salary")]
str(abridged_hr_data)

#Change to dummy variables
hr_dummy <- as.data.frame(model.matrix(~ . -1, data = abridged_hr_data))
str(hr_dummy)
summary(hr_dummy)

#Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

hr_data_with_dummy_normalized <- as.data.frame(lapply(hr_dummy, normalize))
str(hr_data_with_dummy_normalized)


#Load factoextra
library(factoextra)

#Reduce data set to a 33% random sample
set.seed(12345)
normalized_33_percent_random_sample <- hr_data_with_dummy_normalized[
  sample(1:nrow(hr_data_with_dummy_normalized), round(.33 * nrow(hr_data_with_dummy_normalized))),]

#Count size of sample
nrow(normalized_33_percent_random_sample)

#Choosing K by Silhouette Score
set.seed(12345)
fviz_nbclust(normalized_33_percent_random_sample, kmeans, method = "silhouette") 

#===================================================================================================================
#Part 4: Perform clustering analysis

#Set seed
set.seed(12345)

#Run k-means clustering algorithm with 5 clusters
hr_clusters <- kmeans(hr_data_with_dummy_normalized, 5)  

#View cluster sizes
hr_clusters$size   

#View cluster centers
hr_clusters$centers

#Export cluster centers to CSV file
write.csv(hr_clusters$centers, "Cluster_Centers_Output.csv")


#Segment customers in original "hr" data frame by cluster according to k-means algorithm
hr$cluster <- hr_clusters$cluster

head(hr)
summary(hr)

#===================================================================================================================
#Part 5: Draw insights from clustering analysis

#Summary table for turnover rate
round(prop.table(table(hr$cluster, hr$left),1),3)
round(prop.table(table(hr$left)),3)

#Summary table for number of turnover
table(hr$cluster, hr$left)
table(hr$left)

#===================================================================================================================
#Part 6: Visualize clusters

#Plot clusters without observations but with center; ellipse
fviz_cluster(hr_clusters, data = hr_data_with_dummy_normalized, geom = "point", shape = 20,
             ellipse.type = "norm", show.clust.cent = TRUE, alpha =0)

#Plot clusters without observations but with center; no ellipse
fviz_cluster(hr_clusters, data = hr_data_with_dummy_normalized, geom = "point", shape = 20,
             show.clust.cent = TRUE, alpha =0)

#===================================================================================================================
#Part 7: Prepare data for knn analysis of all observations

#Add column of left to normalized data
hr_data_for_knn <- hr_data_with_dummy_normalized
hr_data_for_knn$outcome <- hr$left
str(hr_data_for_knn)

#Re-arrange all observations in random order 
set.seed(1234)
all_observations_in_random_order <- hr_data_for_knn[sample(1:nrow(hr_data_for_knn), nrow(hr_data_for_knn)),]
str(all_observations_in_random_order)

#Set starting and ending boundaries for test and train data
train_start <- 1
train_end <- round(nrow(all_observations_in_random_order) * .7 * 1,0)
test_start <- train_end + 1
test_end <- round(nrow(all_observations_in_random_order) * 1 * 1,0)

#Create training and testing data for x-variables
hr_train <- all_observations_in_random_order[train_start:train_end,1:15 ]
hr_test <- all_observations_in_random_order[test_start:test_end,1:15 ]

#Create training and testing data for y-variable
hr_train_labels <- all_observations_in_random_order[train_start:train_end,16]
hr_test_labels <- all_observations_in_random_order[test_start:test_end,16]

#===================================================================================================================
#Part 10: Perform kNN analysis

#Load class package
library(class)

#Prelim sample size
round(sqrt(nrow(hr_train)),0)

#Run KNN model
set.seed(12345)
hr_test_prediction <- knn(train = hr_train, test = hr_test, cl = hr_train_labels, k=1)

#Load gmodels package
library(gmodels)

#Create confusion matrix
CrossTable(x = hr_test_labels, y = hr_test_prediction, prop.chisq=FALSE)

#===================================================================================================================
