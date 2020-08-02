
###################################Supporting Library install and load#######################################

install.packages("package_name") #The function install.packages() is used to install a package from CRAN
install.packages("devtools")#Install before data.table
remove.packages("data.table")
install.packages("data.table")
#remove.packages("read.table")
#install.packages("read.table")
install.packages(c("Rcpp", "readr"))
install.packages("sqldf")
install.packages("moments")
install.packages("reshape2") 
install.packages("ggplot2")
install.packages("openintro") # states data
install.packages("ggmap")
install.packages("readxl")
install.packages("gdata") # to reformat some data sets, such as using cbindX function
install.packages("zipcode")
install.packages("gsubfn")
install.packages("e1071", dep = TRUE) 
install.packages("gridExtra")
install.packages("RH2")#need to extract year from date field

library(devtools)
library(data.table)
#library(read.table)
library(Rcpp)
library(readr)
library(sqldf)
library(moments)
library(reshape2)
library(ggplot2)
library(openintro) 
library(ggmap)
library(readxl)
library(gdata)
library(zipcode)
library(gsubfn) 
library(e1071)
library(gridExtra)
library(RH2)
library(Metrics)

packages=c("arulesViz", "kernlab","caret", "arules")
#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#mapping factor to character
library(purrr)
library(dplyr)

#Date format
install.packages("tidyverse")
install.packages("lubridate")
install.packages("nycflights13")
library(tidyverse)
library(lubridate)
library(nycflights13)

#verify they are loaded (library)
search()

###################################FUnction declearation#######################################

#function readGoogleSalesData to read data from CSV
#@param verctor for file path and file name
readGoogleSalesData <- function(csvFile)
{
  csvFile
  setwd(csvFile[1])
  googleSalesData <- read.csv(csvFile[2])
  googleSalesDataCleaned <- dataCleanUpProcess(googleSalesData)
  return(googleSalesDataCleaned)
}

#function dataCleanUpProcess to clean up data
#@param verctor for file path and file name
dataCleanUpProcess <- function(salesdfCleaneUp){
  columnNames <- colnames(salesdfCleaneUp)
  for (i in c(1:length(columnNames))) { 
    print(paste(columnNames[i]))
    
    if(columnNames[i] == "Rating"){
      salesdfCleaneUp <- salesdfCleaneUp[salesdfCleaneUp$Rating<=5,] #remove data if rating is more than 5
      salesdfCleaneUp$Rating[is.nan(salesdfCleaneUp$Rating)] <- mean(salesdfCleaneUp$Rating, nan.rm=TRUE) #replace NaN with mean of Rating
    }
    else if(columnNames[i] == "Reviews"){
      salesdfCleaneUp$Reviews <- as.numeric(salesdfCleaneUp$Reviews) #Convert Reviews column to as numeric
      }
    else if(columnNames[i] == "Size"){
      salesdfCleaneUp <- salesdfCleaneUp[complete.cases(salesdfCleaneUp), ] #Remove all Null/Na/NaN from the data set as it's creating forblem for data manipulation.
      salesdfCleaneUp$Size <- gsub("Varies with device","0",salesdfCleaneUp$Size) # Replace "Varies with device" with "0"
      options(scipen=999) #
      salesdfCleaneUp$Size <- as.double(sub('\\D$', '', salesdfCleaneUp$Size))*c(1e9, 1e6, 1e3)[match( sub('\\d*\\.*\\d*', '', salesdfCleaneUp$Size), c('B', 'M', 'k'))] #Change download size 1.2M or 1.8k to double format.
    }
    else if(columnNames[i] == "Installs"){
      salesdfCleaneUp$Installs <- gsub("\\D|\\s","",salesdfCleaneUp$Installs) #Remove all characters and space from Installs
      salesdfCleaneUp$Installs <- as.numeric(salesdfCleaneUp$Installs) # convert factor to numeric data type
    }
    else if(columnNames[i] == "Price"){
      salesdfCleaneUp$Price <- as.numeric(gsub("[\\$,]", "", salesdfCleaneUp$Price)) #Remove '#' and ',' from Price and convert to numeric from Factor data type
    }
    else if(columnNames[i] == "Last.Updated"){
      #unlist(strsplit(salesdfCleaneUp$Last.Updated, ",|\\s"))
      salesdfCleaneUp$Last.Updated <- mdy(salesdfCleaneUp$Last.Updated) #Convert date format from 'January 07, 2018' to '2018-01-07' as Date type
     # salesdfCleaneUp$Last.Updated <- as.character.Date(salesdfCleaneUp$Last.Updated,"%m-%d-%Y")
    }
      #salesdfCleaneUp <- data.frame(lapply(salesdfCleaneUp, as.character), stringsAsFactors=FALSE)
      salesdfCleaneUp %>% map_if(is.factor, as.character) %>% as_data_frame -> salesdfCleaneUp #Convert all Factors to Character type
      salesdfCleaneUp[, c("Category", "Type", "Content.Rating", "Genres", "Current.Ver", "Android.Ver")] <- lapply(salesdfCleaneUp[, c("Category", "Type", "Content.Rating", "Genres", "Current.Ver", "Android.Ver")], factor) #COnverted some specific column back to Factor from Character type
  }
  return(salesdfCleaneUp)
}


# create a function to do sampling
  printVecInfo <- function(v, x){
    samp <- sample(v,x,replace=TRUE)
    samp <- samp[!is.na(samp)]
    meanNum <- sum(samp)/length(samp)
    return(meanNum)
  }
  
  
  ###################################Main Scope of the execution#######################################
  
  csvFile <- c("C:\\Users\\dchatterjee\\Global Interim\\Syracuse\\IST 687\\Week9\\", "googleplaystore.csv") #Prepare vector to read csv file
  googlePlayStoreData<-readGoogleSalesData(csvFile) #Call function readGoogleSalesData to read csv file and load data into data frame. 
  rownames(googlePlayStoreData) <- NULL #Reset the row index for dataset googlePlayStoreData
  str(googlePlayStoreData) #display the structure of the dataset googlePlayStoreData
  summary(googlePlayStoreData) #display the summery of the dataset googlePlayStoreData

  D <- googlePlayStoreData
  #@histogram---------------------------------Start------------------------------
  hist(D$Installs)
  plot(D$Last.Updated,D$Installs, type ="h")
  plot(D$Last.Updated, D$Rating, type ="b")
  quantileInstallData <- quantile(D$Installs, c(0.90, 0.99))
  quantileInstallData
  skewnessPriceData <- skewness(D$Price)
  skewnessPriceData
  #@histogram---------------------------------END--------------------------------
  
  #@Sampling---------------------------------Start-------------------------------
  
  Price <- replicate(20,mean(replicate(10,printVecInfo(D$Price, 10))))
  hist(Price)
  Size <- replicate(20,mean(replicate(10,printVecInfo(D$Size, 10))))
  hist(Size)
  Installs <- replicate(20,mean(replicate(10,printVecInfo(D$Installs, 10))))
  hist(Installs)
  Reviews <- replicate(20,mean(replicate(10,printVecInfo(D$Reviews, 10))))
  hist(Reviews)
  Rating <- replicate(20,mean(replicate(10,printVecInfo(D$Rating, 10))))
  hist(Rating)
  #@Sampling---------------------------------END---------------------------------
  
  #@Plot and GGPlot---------------------------------Start------------------------
  
  namesOfColumns <-  c("App","Category","Rating","Reviews","DownloadSize","Installs","Type","Price","AgeGroup","Genres","LastUpdated","CurrentVer","AndroidVer")
  colnames(D) <- namesOfColumns
  #Extracting year from Last.update and adding to the data set
  #D <- D[,-14]#Used it during denug and clean drop the column
  UpdateYear <- sqldf("select year(LastUpdated) UpdateYear from D")
  D <- cbind(D, UpdateYear)
  #download based on type, Category, Year
  appDownloadByTypeCategory <- sqldf("select sum(DownloadSize) TotalDownload, Type, Category, UpdateYear from D group by Type, Category, UpdateYear")
  #install based on type, Category, Year
  appInstalledByTypeCategory <- sqldf("select sum(Installs) SumInstalled, Type, Category, UpdateYear from D group by Type, Category, UpdateYear")
  #App(x) download(BLOCk PLOT) based on Price(Y)
  appDownloadByPrice <- sqldf("select App, sum(DownloadSize) TotalDownload, Price from D group by App, Price")
  #App(x) installed(BLOCk PLOT) based on Price(Y)
  appInstalledByPrice <- sqldf("select App, sum(Installs) SumInstalled, Price from D group by App, Price")
  #App(x) download(BLOCk PLOT) based on Category(Y)
  appDownloadByCategory <- sqldf("select App, sum(DownloadSize) TotalDownload, Category from D group by App, Category")
  #App(x) installed(BLOCk PLOT) based on Category(Y)
  appInstalledByCategory <- sqldf("select App, sum(Installs) SumInstalled, Category from D group by App, Category")
  
  #Top rating five application download rate over the year. 

  GoogleData<- googlePlayStoreData[,2:9]
  colnames(GoogleData)[colSums(is.na(GoogleData)) > 0]
  GoogleData$Size[is.na(GoogleData$Size)] <- mean(GoogleData$Size, na.rm=TRUE) # find the NAs in column "Ozone" and replace them by the mean value of this column
  GoogleData$Installs<-as.factor(GoogleData$Installs)
  #create a random index for test data
  randIndex <- sample(1:dim(GoogleData)[1])
  #create a cutpoint for 2/3 test data
  cutpoint2_3<- floor(2*dim(GoogleData)[1]/3)
  #create a dataframe for the training data
  trainData <- GoogleData[randIndex[1:cutpoint2_3],]  
  dim(trainData)  
  head(trainData)
  #create a dataframe for the test data
  testData<-GoogleData[randIndex[(cutpoint2_3+1):dim(GoogleData)[1]],]
  
  svmOutput <- ksvm(Installs~., #Set Installs as the target predicting variables
               data = trainData,
               kernel = "rbfdot", 
               C = 100,
               cross =10,
               prob.model = TRUE
              )
  svmPred <- predict(svmOutput,
                     testData
                     )  

  compTable <- data.frame(testData[,5],svmPred)  
  colnames(compTable) <- c("test", "prediction")
  predacc <- length(which(compTable$test==compTable$prediction))/dim(compTable)[1]
  predacc
  #converting from factors to numeric, have to use character otherwise converts to assigned factor number
  compTable$test <-as.character(compTable$test)
  compTable$test <-as.numeric(compTable$test)
  compTable$prediction <-as.character(compTable$prediction)
  compTable$prediction <-as.numeric(compTable$prediction)
  testData$Installs <-as.character(testData$Installs)
  testData$Installs <-as.numeric(testData$Installs)  
  
  testData$prediction <- compTable$prediction
  #inserting the error into the testdata
  testData$errors<- abs(compTable$test-compTable$prediction)
  testData$errora<-with(testData, pmax(Installs, prediction))
  testData$errorp<-testData$errors/testData$errora
  #plotting error
  
  OrdTest<-testData[order(-testData$errorp),]
  
  svmPlot <- ggplot(OrdTest) + geom_point(aes(x=Category, y=Installs, color=errorp, size=errorp))
  svmPlot
  
  #converting installs back to factors for plotting the heat map
  testData$Installs <-as.factor(testData$Installs) 
  
  OutputHeatMapCategory <- ggplot(data = testData, aes(x = Installs, y = Category)) + geom_tile(aes(fill = errorp)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  OutputHeatMapCategory

#  While this model is overall only about 35% accuratye, we do notice that it is very good at predicting apps that will hit one million downloads.
#  we also notice that it is pretty good at predicting sports, social, shopping, games, communications, and books and reference categories.
  
  testData$Rating<-as.factor(testData$Rating)
  OutputHeatMapRating <- ggplot(data = testData, aes(x = Installs, y = Rating)) + geom_tile(aes(fill = errorp)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  OutputHeatMapRating
  
  OutputHeatMapContent <- ggplot(data = testData, aes(x = Installs, y = Content.Rating)) + geom_tile(aes(fill = errorp)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  OutputHeatMapContent

  OutputHeatMapType <- ggplot(data = testData, aes(x = Installs, y = Type)) + geom_tile(aes(fill = errorp)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  OutputHeatMapType  

  OutputHeatMapCType <- ggplot(data = testData, aes(x = Category, y = Type)) + geom_tile(aes(fill = errorp)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  OutputHeatMapCType  
# From these heatmaps we can start to have an idea of catebories to focus our efforts on in order to hit 1000000 downloads  
  
  #@Plot and GGPlot--------------------------------ENd--------------------------------
  
  #@Hypothesis Test---------------------------------Start-----------------------------
  #@Test 1 (Check if DownloadSize or Installation does have any effect on Price)
  #Priced based on DownloadSize and Installation
  hypothesisD <- sqldf("select sum(Price) Price,DownloadSize, Installs  from D group by DownloadSize, Installs")
  hypothesisD <- hypothesisD[complete.cases(hypothesisD), ]
  #salesdfCleaneUp <- salesdfCleaneUp[complete.cases(salesdfCleaneUp), ] 
  modelDownload <- lm(formula = hypothesisD$Price ~ hypothesisD$DownloadSize, data = hypothesisD)
  #D <- sqldf("select App, Installs, Category, Price from D group by Category, Price")
  modelInstalled <- lm(formula = hypothesisD$Price ~ hypothesisD$Installs, data = hypothesisD)
  #Hypothesis test if Price has any effect on download and 
  anova(modelInstalled,modelDownload)
  
  #@Conclude :
    #The result shows that model 1 and 2 does not provide a significantly better fit to the data compared to model. 
    #It means both of them Download Size and Installation does not have any significance on price.
  
  #@Test 2
  #@Test 1 (Check if Price has any effect on TotalDownloa or Installation)  
  modelDownload <- lm(D$DownloadSize ~ D$Price, data = D)
  plot(D$DownloadSize ~ D$Price, D)
  abline(modelDownload)
  g <- ggplot(D, aes(x = D$Price, y = D$Category)) + geom_point(aes(size = D$DownloadSize, color = D$DownloadSize))
  g <- g + stat_smooth(method = "lm")
  g
  summary(modelDownload)
  #abline(coef = coef(modelDownload))//Same as above line
  sum.model <- summary(modelDownload)
  sum.model$adj.r.squared
  

  
  modelInstalled <- lm(formula = D$Installs ~ D$Price, D)
  plot(D$Installs ~ D$Price, D)
  abline(modelInstalled)
  g <- ggplot(D, aes(x = D$Price, y = D$Category)) + geom_point(aes(size = D$Installs, color = D$Installs))
  g <- g + stat_smooth(method = "lm")
  g
  summary(modelInstalled)
  sum.model <- summary(modelInstalled)
  sum.model$adj.r.squared
  
  #@Conclude :
    # In first test P is 0.0211 (P < Alphs(0.05)). P is low Home must go. It means Price does not have any effect on Total download.
    # In second test P is 0.2499 (P > Alphs(0.05)). In this case we could not reject Home. It means Price has effect on Installation.

  #@Hypothesis Test---------------------------------ENd-------------------------------
  
  #@Linear regression module---------------------------------Start--------------------
  #Find which category of application people like to download most (top five) and show the comparison between paid and free.
  #Predict best selling app in Linear model for 2019 of total worth.

#Find which category of application people like to download most (top five) and show the comparison between paid and free.
#Top 5 overall categories of installed apps for all years
appInstalledByCategory <- sqldf("select Category, count(Category) Category_Ct from D group by Category order by Category_Ct DESC")
appInstalledByCategory[1:5,]
# Top overall downloaded free apps by category
appFreeInstalledByCategory <- sqldf("select Category, count(Category) Category_Ct from D WHERE Price = 0 group by Category order by Category_Ct DESC")
appFreeInstalledByCategory[1:5,]
# Top overall downloaded paid apps Category Type
appFreeInstalledByCategory <- sqldf("select Category, count(Category) Category_Ct from D WHERE Price > 0 group by Category order by Category_Ct DESC")
appFreeInstalledByCategory[1:5,]
#
# Copy the data frame to allow analysis and changes without impacting other code
D9 <- D
#
colnames(D9) <- c("App","Category","Rating", "Reviews", "Size", "Installs", "Type", "Price", "ContentRating", "Genres", "LastUpdated", "CurrentVer", "AndroidVer", "TotalWorth","CategoryCode","GenresCode")
#Determine the distinct categories
DistinctCategory <- sqldf("select Distinct(Category), 0 int from D9 order by Category DESC")
colnames(DistinctCategory) <- c("Category","CategoryCode")
DistinctCategory
DistinctCategory$CategoryCode <- as.integer(rownames(DistinctCategory))
DistinctCategory
#Add a ColumnCategory Code to D which is the numeric equivalent of the Category
D9$CategoryCode <- match(D9$Category,DistinctCategory$Category,nomatch=0)
#
#Determine the distinct Genres
DistinctGenres <- sqldf("select Distinct(Genres) from D9 order by Genres DESC")
#Assign the Row number as the numeric equivalent of the Genre
DistinctGenres$GenresCode <- as.integer(rownames(DistinctGenres))
DistinctGenres
#Add a Column Category Genres to D which is the numeric equivalent of the Genres
D9$GenresCode <- match(D9$Genres,DistinctGenres$Genres,nomatch=0)
#
#Determine the distinct Content.Rating
DistinctCR <- sqldf("select Distinct(ContentRating) from D9 order by ContentRating DESC")
#Assign the Row number as the numeric equivalent of the Content Rating
DistinctCR$CRCode <- as.integer(rownames(DistinctCR))
DistinctCR
#Add a Column Category Genres to D which is the numeric equivalent of the Genres
D9$CRCode <- match(D9$ContentRating,DistinctCR$ContentRating,nomatch=0)
#Predict best selling app in Linear model for 2019 of total worth.
# Use estimated value of $1.20/user as per Quora
D9$TotalWorth = (D9$Installs * D9$Price) + 1.20 * D9$Installs
#D9$TotalWorth = (D9$Installs * D9$Price)
# Use ggplot to plot Fawn Count vs. Adult Count
#g <- ggplot(df, aes(x = AdultCt, y = FawnCt)) + geom_point() + stat_smooth(method = "lm", col="red")
#g
# Model 1: Predict Total Worth for 2019 Using Category
m1 <- lm(formula = D9$TotalWorth ~ CategoryCode, data=D9)
# Display the regression analysis summary for Model 1:
summary(m1)
# Plot the regression line
plot(D9$CategoryCode,D9$TotalWorth)
abline(m1)
# Model 2: Predict Total Worth for 2019 Using Rating
m2 <- lm(formula = D9$TotalWorth ~ Rating, data=D9)
# Display the regression analysis summary for Model 2:
summary(m2)
# Plot the regression line
plot(D9$Rating,D9$TotalWorth)
abline(m2)
# Model 3: Predict Total Worth for 2019 Using Genres
m3 <- lm(formula = D9$TotalWorth ~ GenresCode, data=D9)
# Display the regression analysis summary for Model 3:
summary(m3)
# Plot the regression line
plot(D9$GenresCode,D9$TotalWorth)
abline(m3)
#
# Model 4: Predict Total Worth for 2019 Using Reviews
m4 <- lm(formula = D9$TotalWorth ~ Reviews, data=D9)
# Display the regression analysis summary for Model 4:
summary(m4)
# Plot the regression line
plot(D9$Reviews,D9$TotalWorth)
abline(m4)
# Model 5: Predict Total Worth for 2019 Using Size
m5 <- lm(formula = D9$TotalWorth ~ Size, data=D9)
# Display the regression analysis summary for Model 5:
summary(m5)
# Plot the regression line
plot(D9$Size,D9$TotalWorth)
abline(m5)
#
# Model 6: Predict Total Worth for 2019 Using ContentRating
m6 <- lm(formula = D9$TotalWorth ~ CRCode, data=D9)
# Display the regression analysis summary for Model 6:
summary(m6)
# Plot the regression line
plot(D9$CRCode,D9$TotalWorth)
abline(m6)
#
# Model 7: Predict Total Worth for 2019 Using Type
m7 <- lm(formula = D9$TotalWorth ~ Type, data=D9)
# Display the regression analysis summary for Model 7:
summary(m7)
# Plot the regression line
plot(D9$Type,D9$TotalWorth)
abline(m7)
#
# Model 8: Predict Total Worth for 2019 Using Genres, Category, Rating, Size
m8 <- lm(formula = D9$TotalWorth ~ GenresCode + CategoryCode + Rating + Size + CRCode, data=D9)
# Display the regression analysis summary for Model 8:
summary(m8)
#
#
  #@Linear regression module---------------------------------ENd----------------------