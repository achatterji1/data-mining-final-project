# Random Forest Code 

# Import Packages
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample) 
library(randomForest)
library(lubridate)
library(modelr)
library(caret)
library(readr)
library(lattice)
library(caTools)
library(ggplot2)
library(dplyr)
library(data.table)
library(onehot)



# #Import Data
# startup_data_2 <-read.csv(file.path("startup data 2.csv"))
# 
# 
# startup_data_2$status <- as.factor(startup_data_2$status)



view(startup_data_2)

# Create buckets for category_code

# internet bucket
startup_data_2$category_bucket = 'null'
startup_data_2$category_bucket[startup_data_2$category_code=='web'] = 'internet'
startup_data_2$category_bucket[startup_data_2$category_code=='search'] = 'internet'
startup_data_2$category_bucket[startup_data_2$category_code=='network_hosting'] = 'internet'
startup_data_2$category_bucket[startup_data_2$category_code=='enterprise'] = 'internet'
startup_data_2$category_bucket[startup_data_2$category_code=='ecommerce'] = 'internet'
startup_data_2$category_bucket[startup_data_2$category_code=='security'] = 'internet'

# Entertainment bucket
#startup_data_2$category_bucket = 'null'
startup_data_2$category_bucket[startup_data_2$category_code=='social'] = 'entertainment'
startup_data_2$category_bucket[startup_data_2$category_code=='photo_video'] = 'entertainment'
startup_data_2$category_bucket[startup_data_2$category_code=='games_video'] = 'entertainment'
startup_data_2$category_bucket[startup_data_2$category_code=='music'] = 'entertainment'
startup_data_2$category_bucket[startup_data_2$category_code=='messaging'] = 'entertainment'
startup_data_2$category_bucket[startup_data_2$category_code=='travel'] = 'entertainment'
startup_data_2$category_bucket[startup_data_2$category_code=='sports'] = 'entertainment'

# knowledge_service bucket
#startup_data_2$category_bucket = 'null'
startup_data_2$category_bucket[startup_data_2$category_code=='public_relations'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='education'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='consulting'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='analytics'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='news'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='finance'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='advertising'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='hospitality'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='fashion'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='real_estate'] = 'knowledge_service'
startup_data_2$category_bucket[startup_data_2$category_code=='transportation'] = 'knowledge_service'


# Science bucket
#startup_data_2$category_bucket = 'null'
startup_data_2$category_bucket[startup_data_2$category_code=='software'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='semiconductor'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='mobile'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='medical'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='health'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='hardware'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='cleantech'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='biotech'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='automotive'] = 'science'
startup_data_2$category_bucket[startup_data_2$category_code=='manufacturing'] = 'science'

# Other bucket
startup_data_2$category_bucket[startup_data_2$category_code=='other'] = 'other'                             

table(startup_data_2$category_bucket)

startup_data_2$category_bucket

#startup_data_2$category_bucket1 <- as.factor(startup_data_2$category_bucket)


# Convert buckets into one hot encoding 
#startup_data_2$category_bucket
startup_data_2$is_internet = 0
startup_data_2$is_internet[startup_data_2$category_bucket=='internet'] = 1
view(startup_data_2)


#startup_data_2$category_bucket
startup_data_2$is_entertainment = 0
startup_data_2$is_entertainment[startup_data_2$category_bucket=='entertainment'] = 1

#startup_data_2$category_bucket
startup_data_2$is_knowledge_service = 0
startup_data_2$is_knowledge_service[startup_data_2$category_bucket=='knowledge_service'] = 1

#startup_data_2$category_bucket
startup_data_2$is_science = 0
startup_data_2$is_science[startup_data_2$category_bucket=='science'] = 1

#startup_data_2$category_bucket
startup_data_2$is_other = 0
startup_data_2$is_other[startup_data_2$category_bucket=='other'] = 1

# startup_data_2$is_other[startup_data_2$category_bucket==1] = 0

#print(startup_data_2)

view(startup_data_2)

# Encode WA

#startup_data_2$state_code.1
startup_data_2$is_WA = 0
startup_data_2$is_WA[startup_data_2$state_code.1=='WA'] = 1

startup_data_2$is_otherstate[startup_data_2$is_WA==1] = 0

view(startup_data_2)

head(startup_data_2)

# Let's split our data into training and testing
startup_split =  initial_split(startup_data_2, prop=0.8)
startup_train = training(startup_split)
startup_test  = testing(startup_split)

#Make status a factor



status ~ has_VC + has_angel + is_top500 + funding_total_usd + is_internet + is_entertainment + is_knowledge_service + is_science + is_other + is_CA + is_MA + is_NY + is_TX + is_WA + milestones


# Let's fit a single tree
startup.tree = rpart(status ~ has_VC + has_angel + is_top500 + funding_total_usd + is_internet + is_entertainment
                     + is_knowledge_service + is_science + is_other + is_CA + is_MA + is_NY + is_TX + is_WA + milestones, data=startup_train, control = rpart.control(cp = 0.00001))

# Fit a forest
startup.forest = randomForest(status ~ has_VC + has_angel + is_top500 + funding_total_usd + is_internet + is_entertainment
                              + is_knowledge_service + is_science + is_other + is_CA + is_MA + is_NY + is_TX + is_WA + milestones,
                              data=startup_train, importance = TRUE)

# Confusion Matrix 
conf <- startup.forest$confusion
conf
##        acquired closed class.error
##acquired      439     43  0.08921162
##closed        132    124  0.51562500


## Corresponds to roughly 76.28% accuracy


# shows out-of-bag MSE as a function of the number of trees used
plot(startup.forest)

# let's compare RMSE on the test set
modelr::rmse(startup.tree, startup_test)
##[1] 0.4197978
modelr::rmse(startup.forest, startup_test)  # a lot lower!
##[1] 0.4011664

# variable importance measures
# how much does mean-squared error increase when we ignore a variable?
viforest<-varImpPlot(startup.forest)

view(viforest)

# partial dependence plots
# these are trying to isolate the partial effect of specific features
# on the outcome

pd_funding<-partialPlot(startup.forest, startup_test, 'funding_total_usd', las=1)

pd_milestones<-partialPlot(startup.forest, startup_test, 'milestones', las=1)

pd_top500<-partialPlot(startup.forest, startup_test, 'is_top500', las=1)

pd_CA<-partialPlot(startup.forest, startup_test, 'is_CA', las=1)
