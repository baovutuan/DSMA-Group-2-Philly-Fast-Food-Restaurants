# PART 1: DATA PROCESSING
## This part's codes originated from Jasper Stuerwald, were modified to suitable to the analysis

rm(list = ls()) #clear workspace
#setwd("~/dsma_project/Term paper") #set workspace


library(gender)
#remotes::install_github("ropensci/gender-data-pkg")
library(genderdata)
library(dplyr)

#Load the dataset
#Data is extracted by using the Philadelphia_fastfoodreview.sql query.
Philly_FastFood = read.csv("Fastfood_raw_data.csv")

#Sentiment score analysis by using AFINN Lexicon
library(tidytext)

CustomerSentiment<-Philly_FastFood %>%
  unnest_tokens(word, review) %>%  ##Split a review column into words/tokens
  anti_join(stop_words)%>%  #remove stopwords
  inner_join(get_sentiments("afinn"), by = "word") %>% # join with the AFINN lexicon which has 2 columns titled word and value 
  
  group_by(review_id) %>%
  summarize(sentiment = mean(value),words = n()) 

#Join in the full data
Philly_FastFood <- left_join(Philly_FastFood, CustomerSentiment, by = c("review_id" = "review_id"))

#If any review does not have any words in AFINN Lexicon, the number of words and the sentiment score will be zero
Philly_FastFood$sentiment[is.na(Philly_FastFood$sentiment)] <- 0
Philly_FastFood$words[is.na(Philly_FastFood$words)] <- 0

#Review by Elite User
Philly_FastFood$elite_status <- ifelse(!is.na(Philly_FastFood$elite_user), 1, 0)

#Total number of friends of the user that wrote the review
Philly_FastFood$friends <- ifelse(is.na(Philly_FastFood$friends) | Philly_FastFood$friends == "None", NA, Philly_FastFood$friends)
Philly_FastFood$friends_count <- ifelse(!is.na(Philly_FastFood$friends),stringr::str_count(Philly_FastFood$friends, ",") + 1,0)

#Gender of the user
gender_vector = gender(Philly_FastFood$name)
unique_rows  = !duplicated(gender_vector)
gender_vector = gender_vector[unique_rows,][c(1,4)]
Philly_FastFood = merge(Philly_FastFood, gender_vector, by = "name", all.x = TRUE)


#Now some transform for date 
Philly_FastFood$date = as.Date(Philly_FastFood$date)

#First divide the dataframe into everything up to the start date of the final dataframe and everything after that date to the end date of the dataframe

start_date <- as.Date("2018-01-01")  # Start date of the range
end_date <- as.Date("2018-12-31")    # End date of the range

date_range <- seq(start_date, end_date, by = "day")  # Create a sequence of dates

results <- data.frame()  # Create an empty dataframe to store the final results later

for (i in seq_along(date_range)) {
  date <- date_range[i]
  
  Philly_FastFood_filtered <- Philly_FastFood[Philly_FastFood$date < date,]
  
  Philly_FastFood_grouped <- Philly_FastFood_filtered %>%
    group_by(business_id) %>%
    summarize(
      sum_elite_status = sum(elite_status),
      max_friends_count = max(friends_count),
      num_male = sum(gender == "male", na.rm = TRUE),
      num_female = sum(gender == "female", na.rm = TRUE),
      sum_fans = sum(fans, na.rm = TRUE),
      avg_stars = mean(stars, na.rm = TRUE),
      review_count = n(),
      sum_user_review = sum(user_review_count),
      avg_sentiment_score_review = mean(sentiment)
    ) %>%
    mutate(date = as.Date(date))
  
  Philly_FastFood_grouped <- mutate(Philly_FastFood_grouped, date = date)
  
  results <- bind_rows(results, Philly_FastFood_grouped)  # Append the results to the dataframe
}

#Now find whether in a specific date a restaurant has check in or not

#Load the check in data by using the Philadelphia_checkin.sql query.
checkin_fastfood <- read.csv("checkin_fastfood.csv")
#Only choose distinct observations (for the upcoming left join)
checkin_fastfood <- distinct(checkin_fastfood, .keep_all = TRUE)

#Create id for left join tables
results <- results %>%
  mutate(id = paste(results$business_id, gsub("-", "", results$date), sep = "_"))

checkin_fastfood <- checkin_fastfood %>%
  mutate(id = paste(checkin_fastfood$business_id, gsub("-", "", checkin_fastfood$chn_date), sep = "_"))

#Do the left join
results <- left_join(results, checkin_fastfood, by = c("id" = "id"))

#Find the check in date of a restaurant
results$ch_in <- ifelse(!is.na(results$chn_date),1,0)

#Delete the joined table for better looking
results <- results %>% select(-id,-business_id.y,-chn_date)
results <- results %>% rename(business_id = business_id.x)

#Save the outcome before go to the next part
write.csv(results, file = "Fastfood_analyzed_data.csv", row.names = FALSE)





# PART 2: DATA CLEANING & PREPARATION

#Load part 1 data
results <- read.csv("Fastfood_analyzed_data.csv")

#Load Fast food business data by using Philadelphia_Fastfood_business.sql query
fastFoodBusiness <- read.csv("Fastfood_business.csv")

#First convert the logical value as character (true, false, yes, no) to actual logical values
fastFoodBusiness <- mutate_all(fastFoodBusiness, ~ifelse(. %in% c("NULL", "None"), NA, .))
fastFoodBusiness <- mutate_all(fastFoodBusiness, ~ifelse(. %in% c("True", "true","Free","Paid"), 1, .))
fastFoodBusiness <- mutate_all(fastFoodBusiness, ~ifelse(. %in% c("False","false","No"), 0, .))

fastFoodBusiness_subset <- subset(fastFoodBusiness,select = -c(business_id, business_name)) # removed this because MICE does not like imputing factors with more than 50 levels

#change it to numeric data for the imputing missing variables
fastFoodBusiness_subset <- sapply(fastFoodBusiness_subset, as.numeric)

library(mice)

#inspect pattern of missings
md.business=md.pattern(fastFoodBusiness_subset)
#as we can see, these 5 variables "business_price","business_wifi","business_takeout","business_creditcards","business_delivery" have missing values

if(md.business[length(md.business)]!=0){ # do the imputation only if there are missings (but in this case there are missing values)
  #Below, the predictorMatrix is specified.
  #It is a square matrix with a size equal to the number of columns in the data (ncol(data)), comprising 0s and 1s, indicating the selection of predictors for each target column. 
  #The rows align with target variables, specifically the variables requiring imputation, following the order in which they are presented in the dataset. 
  #A '1' indicates that the column variable serves as a predictor for the corresponding target variable in the rows.
  #The diagonal of predictorMatrix must be zero.
  predictorMatrix <- matrix(0,nrow = ncol(fastFoodBusiness_subset), ncol = ncol(fastFoodBusiness_subset)) # Make a matrix of zeros
  colnames(predictorMatrix)=colnames(fastFoodBusiness_subset)
  row.names(predictorMatrix)=colnames(fastFoodBusiness_subset)
  
  predictorMatrix[c("business_price","business_wifi","business_takeout","business_creditcards","business_delivery"),] <- 1 
  #these variables can be explained by all other variables
  
  diag(predictorMatrix) <- 0 #diagonal must be zero
  
  #impute data
  fastFoodBusiness_subset_data_imputed <- mice(fastFoodBusiness_subset, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)
  
  summary(fastFoodBusiness_subset_data_imputed)
  
  #get one of the complete data sets (2nd out of 5)
  fastFoodBusiness_complete_data <- complete(fastFoodBusiness_subset_data_imputed,2)
  
  #bring back the business_id and business name
  fastFoodBusiness_complete_data=cbind(business_id=fastFoodBusiness$business_id,business_name = fastFoodBusiness$business_name,fastFoodBusiness_complete_data)
  
}else{
  fastFoodBusiness_complete_data = fastFoodBusiness
}

#merge with the result before
results <- left_join(results, fastFoodBusiness_complete_data, by = c("business_id" = "business_id"))

#save the data for the next part
write.csv(results, file = "Fastfood_complete_data.csv", row.names = FALSE)




# PART 3: EXTRACTING WEATHER DATA


rm(list = ls()) #clear workspace

library(dplyr)
library(doParallel)
library(caret)
library(smotefamily)

source("Fastfood_analysis_functions.r")
# ----
# This loop extracts the weather data, and add it to the yelp dataset.
if(1){
  #load data
  Fastfood_complete_data <- read.csv("Fastfood_complete_data.csv",header=TRUE,skipNul = T) #read csv file
  Fastfood_complete_data$date <- as.Date(Fastfood_complete_data$date)
  Fastfood_complete_data$X=NULL
  
  #---- read the temperature data
  # you may need to increase the range of lat and long:
  # extend the range by rangeX 
  rangeX=.01
  latrange=range(Fastfood_complete_data$business_lat)*c(1-rangeX,1+rangeX) 
  longrange=range(Fastfood_complete_data$business_long)*c(1+rangeX,1-rangeX)
  
  cl <- makeCluster(detectCores(),outfile="log1.txt")
  registerDoParallel(cl)
  
  wear=extractweather(Fastfood_complete_data,latrange=latrange,longrange=longrange,resol=.25, cl=cl)
  
  nocl=FALSE
  tryCatch(stopCluster(cl),error=function(e) {nocl<<-TRUE})
  if(!nocl){
    stopCluster(cl)
  }
  
  # take the averages across stations for each coordinate
  weather=weardailyavg(wear)
  
  
  
  dates=sort(unique(Fastfood_complete_data$date))
  weatherstations=as.data.frame(t(sapply(weather,function(x){colMeans(x$range)})))
  
  # adding weather data to yelp_data
  if(1){
    stations_by=t(apply(Fastfood_complete_data[,c("business_lat","business_long")],1,
                        function(x){a=sort((x[1]-weatherstations$rangelat)^2+
                                             (x[2]-weatherstations$rangelong)^2,index.return=T)
                        return(a$ix[1:50])})) # finding the 50 closest stations
    
    # add for example, temperature forecasts to the weather data
    for(i in 1:length(weather)){
      if(nrow(weather[[i]]$data)==0)
        next
      store_weather=weather[[i]]$data
      store_weather$TOBS_1=c(store_weather$TOBS[2:nrow(store_weather)],NA)
      store_weather$TOBS_2=c(store_weather$TOBS[3:nrow(store_weather)],NA,NA)
      store_weather$TOBS_3=c(store_weather$TOBS[4:nrow(store_weather)],NA,NA,NA)
      store_weather$TOBS_4=c(store_weather$TOBS[5:nrow(store_weather)],NA,NA,NA,NA)
      weather[[i]]$data=store_weather
    }
    weatherinf=colnames(store_weather)[-1] # which weather variables are available?
    
    Fastfood_complete_data_weather=NULL
    for(i in 1:length(weather)){
      k=1 # start with the closest station
      stores_in=stations_by[,k]==i
      if(sum(stores_in)==0)
        next
      store_weather=weather[[i]]$data
      
      temp=Fastfood_complete_data[stores_in,]
      temp=merge(temp,store_weather,by.x="date",by.y="DATE",all.x=T)
      Fastfood_complete_data_weather=rbind(Fastfood_complete_data_weather,temp)
      print(i)
    }
    
    # now deal with the missings, by going to the next possible station
    temp_indx=is.na(Fastfood_complete_data_weather[,"TOBS"])|is.na(Fastfood_complete_data_weather[,"PRCP"])
    k_changed=NULL
    for(i in which(temp_indx)){
      temp_date=Fastfood_complete_data_weather[i,]$date
      for(k in 2:ncol(stations_by)){
        temp=weather[[stations_by[i,k]]]$data
        if(!is.na(as.numeric(temp[temp$DATE==temp_date,"TOBS"]))&!is.na(as.numeric(temp[temp$DATE==temp_date,"PRCP"])))
          break
      }
      k_changed=c(k_changed,k)
      
      Fastfood_complete_data_weather[i,weatherinf]=temp[temp$DATE==temp_date,-1]
      #print(i)
    }
    
    # add weekends and quarters
    temp=weekdays(Fastfood_complete_data_weather$date,abbreviate = T)
    Fastfood_complete_data_weather$WE=temp=="Sat"|temp=="Sun"
    
    Fastfood_complete_data_weather$Quarter=as.factor(quarters(Fastfood_complete_data_weather$date))
    
    #save(file="yelp_data_weather.RData",list=c("yelp_data_weather"))
    write.csv(Fastfood_complete_data_weather,file="Fastfood_complete_data_weather.csv", row.names = FALSE)
    
  }
  
}

# PART 4: MACHINE LEARNING MODELS
# Importing and adjusting the yelp-data + weather data

rm(list = ls()) #clear workspace

library(dplyr)
library(doParallel)
library(caret)
library(smotefamily)

source("Fastfood_analysis_functions.r")

yelp_data_weather=read.csv(file="Fastfood_complete_data_weather.csv")

# some adjustments to the imported data
yelp_data=yelp_data_weather

yelp_data$date = as.Date(yelp_data$date)
yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)
yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="Noch_in") # since the performance evaluations are mainly made
# to check for the minority class - in our case ch_in


yelp_data$business_park=as.factor(yelp_data$business_park)
yelp_data$business_wifi=as.factor(yelp_data$business_wifi)
yelp_data$business_takeout=as.factor(yelp_data$business_takeout)
yelp_data$business_delivery=as.factor(yelp_data$business_delivery)
yelp_data$business_creditcards=as.factor(yelp_data$business_creditcards)
yelp_data$WE=as.factor(yelp_data$WE)
yelp_data$Quarter=as.factor(yelp_data$Quarter)

# A simple regression analysis ----
# m1=glm(ch_in~sum_elite_status+max_friends_count+sum_fans+sum_user_review+avg_stars+avg_sentiment_score_review+I((num_male+1)/(num_female+1))+business_price+business_park+business_wifi+business_takeout+business_creditcards+business_delivery+PRCP+SNOW+SNWD+TMAX+TMIN+TOBS_3+Quarter+WE, data = yelp_data, family = "binomial")
# car::vif(m1)
# summary(m1)


# predictive models
# Split randomly
set.seed(66)
yelp_data_na=yelp_data
# list of variables in your model
varsin=c("ch_in_string","ch_in","WE","Quarter","business_price","business_park",
         "business_wifi","business_takeout","business_creditcards","business_delivery",
         "TOBS","PRCP","SNOW","SNWD",
         "num_female","num_male","sum_elite_status","max_friends_count","sum_fans",
         "sum_user_review","avg_stars","avg_sentiment_score_review")
yelp_data=subset(yelp_data,select=varsin)
datasetsize=nrow(yelp_data)/1 # would you like to work only  on a subset of your data? 
x <- yelp_data[sample(1:nrow(yelp_data), datasetsize, replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]

BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))


# create dummies (required for SMOTE)
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))

# class imbalance check.
temp=table(x.train[,"ch_in_string"])
print(temp)

# there is imbalance between check in and not checkin, so do SMOTE:
if(1){
  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  x.traindum=x.traindum_smote
  rm(x.traindum_smote)
}
temp=table(x.traindum[,"ch_in_string"])
print(temp)


############ Data for Heuristic machine learning methods
# normalize data (very important for ML techniques, but not for logistic regression)
x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)

# adjust Base formula to the dummy version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}

# redo the releveling:
x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")


BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))

# set threshold probability: usually .5, but better is to set it to the portion of 1's. 
probthres=mean(x.traindum$ch_in)

# ----
# the analyses
############ SVM
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()

# fast trainer
library(kernlab)

x.modelSVM <- ksvm(BaseFormula_dum, data = x.trainnorm, kernel = "rbfdot", kpar = "automatic", 
                   type = "C-bsvc", prob.model = TRUE)

x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, type="probabilities")


x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))


# Extract the class probabilities.
x.evaluate$predictionSVM <- x.evaluate$predictionSVM[,'ch_in']

SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")

TimeAux <- proc.time() - ptm 
SVMOutput$TimeElapsed <- TimeAux[3]
SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)




######### LOGIT
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "checkin"

summary(x.modelLogit)

x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "ch_in"
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "Noch_in"

x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")

TimeAux <- proc.time() - ptm 
#LogitOutput$summary=summary(x.modelLogit)
LogitOutput$TimeElapsed <- TimeAux[3]
LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
rm(TimeAux)




############ Naive Bayes
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelNB <- train(BaseFormula_dum, data = x.trainnorm, method="naive_bayes")

x.evaluate$predictionNB <- predict(x.modelNB, newdata=x.evaluatenorm,type="prob")


x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctNB <- x.evaluate$predictionNBclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNB)))

# the variable importance
print(varImp(x.modelNB))

# Extract the class probabilities.
x.evaluate$predictionNB <- x.evaluate$predictionNB[,'ch_in']

NBOutput <- makeLiftPlot(x.evaluate$predictionNB,x.evaluate,"NB")

TimeAux <- proc.time() - ptm 
NBOutput$TimeElapsed <- TimeAux[3]
NBOutput$PercCorrect <- mean(x.evaluate$correctNB)*100
NBconfmatrix <- table(x.evaluate$predictionNBclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)




############ KNN
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelKNN <- train(BaseFormula_dum, data = x.trainnorm, method="knn")

x.evaluate$predictionKNN <- predict(x.modelKNN, newdata=x.evaluatenorm,type="prob")


x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctKNN <- x.evaluate$predictionKNNclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctKNN)))

# the variable importance
print(varImp(x.modelKNN))

# Extract the class probabilities.
x.evaluate$predictionKNN <- x.evaluate$predictionKNN[,'ch_in']

KNNOutput <- makeLiftPlot(x.evaluate$predictionKNN,x.evaluate,"KNN")

TimeAux <- proc.time() - ptm 
KNNOutput$TimeElapsed <- TimeAux[3]
KNNOutput$PercCorrect <- mean(x.evaluate$correctKNN)*100
KNNconfmatrix <- table(x.evaluate$predictionKNNclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)




########### Neural Networks 
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(NeuralNetTools) # required for plotting
# fast trainer using parallel computations
ptm <- proc.time()
mlp_grid = expand.grid(layer1 = 5,
                       layer2 = 5,
                       layer3 = 0)
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid) 

x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")

x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))

print(varImp(x.modelNNet))
# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet$finalModel)
}
x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"ch_in"]

NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")

TimeAux <- proc.time() - ptm 
#NNetOutput$summary=varImp(x.modelNNet)
NNetOutput$TimeElapsed <- TimeAux[3]
NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)




########## TREE
# fast model using parallel computation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()

x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree') 


x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")

x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("Noch_in","ch_in"))

x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))

x.evaluate$predictionTree <- x.evaluate$predictionTree[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelTree))

# plot tree, if desired 
if(0){
  plot(x.modelTree$finalModel)
}

TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")

TimeAux <- proc.time() - ptm 
#TreeOutput$summary <- varImp(x.modelTree)
TreeOutput$TimeElapsed <- TimeAux[3]
TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)





############ Bagging
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# fast training using parallel computation
x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)

# Use the model to predict the evaluation.
x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")

x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))

# Extract the class probabilities.
x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBagging))

BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")

TimeAux <- proc.time() - ptm
#BaggingOutput$summary <- varImp(x.modelBagging)
BaggingOutput$TimeElapsed <- TimeAux[3]
BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)




############ Boosting
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'blackboost')#,  method = 'bstTree')

# Use the model to predict the evaluation.
x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")

x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))

# Extract the class probabilities.
x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBoosting))

# Make a lift curve
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")

TimeAux <- proc.time() - ptm 
#BoostingOutput$summary <- varImp(x.modelBoosting)
BoostingOutput$TimeElapsed <- TimeAux[3]
BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)




############ RANDOM FOREST
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF") 

# Use the model to predict the evaluation.
x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")

x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))

# Extract the class probabilities.
x.evaluate$predictionRF <- x.evaluate$predictionRF[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelRF))

RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")

TimeAux <- proc.time() - ptm 
#RFOutput$summary <- varImp(x.modelRF)
RFOutput$TimeElapsed <- TimeAux[3]
RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)



# SOME Summarizing plots:

OverallTDL <- c(LogitOutput$TDL,KNNOutput$TDL,NBOutput$TDL,SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL)
OverallGINI <- c(LogitOutput$GINI,KNNOutput$GINI,NBOutput$GINI,SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI)

ForGraph <- data.frame(OverallTDL,OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)

op <- par(mar = c(5,4,4,4) + 0.1)

barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), beside = TRUE, yaxt = "n", names.arg = c("Logit","N. Neighbour", "Naive Bayes","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), 
        ylab =	"Top Decile Lift", legend = c("TDL","GINI"),
        main="Performance of the Machine Learning Algorithms")

axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))

mtext(c(paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(KNNOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NBOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))
mtext(c(paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(KNNOutput$PercCorrect,digits=0),"%"),
        paste(round(NBOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))

mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)

# drawing the TDL/Gini Graphs for each output
par(mfrow=c(2,2))
LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")
KNNOutput <- makeLiftPlot(x.evaluate$predictionKNN,x.evaluate,"Nearest Neighbour")
NBOutput <- makeLiftPlot(x.evaluate$predictionNB,x.evaluate,"Naive Bayes")
SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")
windows()
par(mfrow=c(3,2))
TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")
BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")
RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")
NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")

# drawing all lift curves in a plot
lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+
                predictionKNN+predictionNB+predictionlogit+predictionSVM+predictionRF,data=x.evaluate,class="ch_in")

ggplot(lift_obj)