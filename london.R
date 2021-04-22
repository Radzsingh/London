
# Analysis of Londom Crime dataset

# Question 1
# Reading the dataset using read.csv
london_crime = read.csv("london-crime-data.csv", na = "")

# displaying the structure of data
str(london_crime)

# displaying first 15 values of the dataset
head(london_crime, 15)

# append the seprated dates into one column
# paste method to concatinate columns to one value
# we are using date as 1 of every mentioned month as we were missing the variable
converted_date <- paste(01, london_crime$month, london_crime$year, sep= "/")
converted_date

# now we can convert this date into Date variable
#insert it into dataset
london_crime$Date <- converted_date

# New column Date is added with formatted date in the dataset
head(london_crime, 15)
str(london_crime)
# Question 2
# Include borough, Major_Category, Minor_Category, value and Date attributes
# remove unwanted attributes
# using names, one can select only required variables from the dataset
include_list <- names(london_crime) %in% c("borough", "major_category", "minor_category", "value", "Date")
include_list

# assigning required variables from dataset to new_london_crime
new_london_crime  <- london_crime[(include_list)]

# display the contents of new dataset to verify it has the required variables
head(new_london_crime, 15)

# Next is to rename the variables
# make a vector of the new names in the same order as we have in our dataset
column_names <- c("Borough", "MajorCategory", "SubCategory", "Value", "CrimeDate")

# Add column names to the new_london_crime dataframe
colnames(new_london_crime) <- column_names
head(new_london_crime, 15)

# Question 3
# Convert the CrimeDate into Date type variables
new_london_crime$CrimeDate <- as.Date(new_london_crime$CrimeDate, "%d/%m/%Y")
# Displaying the structure and contents of the dataset after conversion
str(new_london_crime)
head(new_london_crime, 15)


# QUESTION 4
#display the summary
summary(new_london_crime)

# make factors of the Variable Borough
new_london_crime$Borough <- factor(new_london_crime$Borough)
# check the structure 
str(new_london_crime)


plot(summary(new_london_crime$Borough), 
             main="Number of Borough Crimes",
             xlab="Boroughs",
             ylab="Number of Crimes")

# plotting graph of Borough crime
plot(new_london_crime$Borough, 
     main="Number of Borough Crimes",
     xlab="Boroughs",
     ylab="Number of Crimes",
     col='#5dbaa9')

# The borough has the highest number of crime is Croydon: 5226
max(summary(new_london_crime$Borough))
# The borough has the lowest number of crime is City of London: 86
min(summary(new_london_crime$Borough))

summary(new_london_crime$Borough)

# Question 5
#Factorizing the Major Category
new_london_crime$MajorCategory <- factor(new_london_crime$MajorCategory)
#summarizing the crime categories
summary(new_london_crime$MajorCategory)
#Plotting the Pie chart for the summary data
pie(summary(new_london_crime$MajorCategory), main="Major Crime Categories")

# Category with highest number of crimes is Theft and Handling with 33759
max(summary(new_london_crime$MajorCategory))

# Category with lowest number of crimes is Fraud and Forgery with 917
min(summary(new_london_crime$MajorCategory))

# Question 6
# Create a new column and  generalise the locations
new_london_crime$Region[new_london_crime$Borough == 'Barking and Dagenham'] <- "East"
new_london_crime$Region[new_london_crime$Borough == 'Barnet'] <- "North"
new_london_crime$Region[new_london_crime$Borough == 'Bexley'] <- "East"
new_london_crime$Region[new_london_crime$Borough == 'Brent'] <- "West"
new_london_crime$Region[new_london_crime$Borough == 'Bromley'] <- "South"
new_london_crime$Region[new_london_crime$Borough == 'Camden'] <- "North"
new_london_crime$Region[new_london_crime$Borough == 'Croydon'] <- "South"
new_london_crime$Region[new_london_crime$Borough == 'Ealing'] <- "West"
new_london_crime$Region[new_london_crime$Borough == 'Enfield'] <- "North"
new_london_crime$Region[new_london_crime$Borough == 'Greenwich'] <- "East"
new_london_crime$Region[new_london_crime$Borough == 'Hackney'] <- "North"
new_london_crime$Region[new_london_crime$Borough == 'Hammersmith and Fulham'] <- "West"
new_london_crime$Region[new_london_crime$Borough == 'Haringey'] <- "North"
new_london_crime$Region[new_london_crime$Borough == 'Harrow'] <- "West"
new_london_crime$Region[new_london_crime$Borough == 'Havering'] <- "East"
new_london_crime$Region[new_london_crime$Borough == 'Hillingdon'] <- "West"
new_london_crime$Region[new_london_crime$Borough == 'Hounslow'] <- "West"
new_london_crime$Region[new_london_crime$Borough == 'Islington'] <- "Central"
new_london_crime$Region[new_london_crime$Borough == 'Kensington and Chelsea'] <- "Central"
new_london_crime$Region[new_london_crime$Borough == 'Kingston upon Thames'] <- "East"
new_london_crime$Region[new_london_crime$Borough == 'Lambeth'] <- 'Central'
new_london_crime$Region[new_london_crime$Borough == 'Lewisham'] <- 'Central'
new_london_crime$Region[new_london_crime$Borough == 'Merton'] <- 'South'
new_london_crime$Region[new_london_crime$Borough == 'Newham'] <- 'East' 
new_london_crime$Region[new_london_crime$Borough == 'Redbridge'] <- 'East'
new_london_crime$Region[new_london_crime$Borough == 'Richmond upon Thames'] <- 'West'
new_london_crime$Region[new_london_crime$Borough == 'Southwark'] <- 'Central'
new_london_crime$Region[new_london_crime$Borough == 'Sutton'] <-  'South'
new_london_crime$Region[new_london_crime$Borough == 'Tower Hamlets'] <- 'Central'
new_london_crime$Region[new_london_crime$Borough == 'Waltham Forest'] <- 'Central'
new_london_crime$Region[new_london_crime$Borough == 'Wandsworth'] <- 'East'
new_london_crime$Region[new_london_crime$Borough == 'Westminster'] <- 'Central'

# for any NA data, either we could check the records in the map or we can assign central to the rest of the provinces
# central is no on any extreme and share bounderies with all the directions
new_london_crime$Region[is.na(new_london_crime$Region)] <- 'Central'
#Question 7

# make factors of the Variable Region
new_london_crime$Region <- factor(new_london_crime$Region)
# check the structure 
str(new_london_crime)
# we can see 5 levels


# plotting graph of Borough crime
plot(new_london_crime$Region, 
     main="Number of Borough Crimes",
     xlab="London",
     ylab="Number of Crimes",
     col='#5dbaa9')
#summary of regions
summary(new_london_crime$Region)
#We can clearly see that Central has the highest number of crimes  28591
# We can clearly see the South part of london has less number of crimes which are 15487

#Ques 8
attach(new_london_crime)
#subsetting the region "central" with high crime rate
high_crime_data <- subset( new_london_crime, Region == "Central")
high_crime_data

#subsetting the region "south" with low crime rate
low_crime_data <- subset( new_london_crime, Region == "South")
low_crime_data
str(low_crime_data)
detach(new_london_crime)
# Ques 9
# plotting graphs for high and low crime rates
plot.new()
plot(high_crime_data$Borough, 
     main="Number of Borough Crimes",
     xlab="Boroughs",
     ylab="Number of Crimes",
     col='#5dbaa9')
# to plot new graph on the same graph
par(new=TRUE)
plot(low_crime_data$Borough, 
     main="Number of Borough Crimes",
     xlab="Boroughs",
     ylab="Number of Crimes",
     col='#fdbaa9')
box()


