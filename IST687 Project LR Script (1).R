#@Linear regression module---------------------------------Start--------------------
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
#@Linear regression module---------------------------------ENd-------------------------------------