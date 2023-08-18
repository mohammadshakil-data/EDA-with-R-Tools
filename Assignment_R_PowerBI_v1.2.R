# Analysis

#Load data file local disc by using read.csv function
#Please note, forward slash '/' must be used in the path, else R will throw an error if backslash '\' used

file_dir <- "C:/Users/SSNQ/Documents/Skills Data Technician BootCamp/WK11 Learning R/"
file_name <- "HollywoodsMostProfitableStories.csv"
full_file_path <- file.path(file_dir,file_name)

df <- read.csv(full_file_path)

View(df)

#Load library:
  
install.packages("tidyverse")

#Import library

library(tidyverse)
library(dplyr)

# Check data types:

str(df)
dim(df) 
summary(df)

# Check for missing values:
# na() function is used to check for NA in the fields
# colsums() function calculate the total number of NA values in each column

colSums(is.na(df))


# Drop rows with missing values
data_cleaned <- df %>% drop_na()
dim(data_cleaned) #check to see the output data

#Rerun check for missing values
colSums(is.na(data_cleaned))
head(data_cleaned, n=40)
tail(data_cleaned, n=40)


#Check for duplicates by using duplicated() function
# table() function is used to calculate the total number of TRUE/FALSE values
# This is not a mandatory step but can be used while analysing large dataset
duplicated_rows <- duplicated(data_cleaned)
duplicated_count <- table(duplicated_rows)

# Check duplicated data by using print() function
# Please note, print must not be used with large amount of dataset, else system will freeze
# Use head() or tail() functions as above
print(duplicated_rows)

# check the count of duplicated data
print(duplicated_count)

# Remove duplicated rows based on a given column
# There are no duplicates, but below statement will help you remove duplicates, if present
duplicated_removed <-data_cleaned %>% distinct(Film, .keep_all = TRUE)

# This is to see the output after removing the duplicates
dim(duplicated_removed)

#round off values to 2 places
# mutate function allows us to store the output into new column then assign it back to df_rounded
df_rounded <- duplicated_removed %>%
  mutate(across(c(Profitability, Worldwide.Gross), ~round(., digits = 2)))

head(df_rounded)

#Check for outliers using a boxplot

# Import ggplot2 library into memory
library(ggplot2)

######## Outlier Removal Process ###############
#remove outlier from Profitability Column

#Create a boxplot that labels the axes and outliers displaed in red
# geom_boxplot() function will mark the outliers in red and shape as "19 = solid circle"
#scale_x_continuous() is used to format the label formatting by adding "comma" as the 1000 seperator
# scales::comma - It means, we want to use comma function from package 'scales' directly, 
# instead of specifically loading entire scales package
# coord_cartesian(ylim = c(0, 1000)) - This is very useful function, whereby you can specify your focus on a
# particular range of data within large dataset, i.e., Y axis is limited to 0-1000, any values beyond this range will be ignored.

ggplot(df_rounded, aes(x=Profitability, y=Worldwide.Gross)) + 
  geom_boxplot(outlier.colour = "red", 
               outlier.shape = 19)+ 
  scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

dim(df_rounded)

#Remove outliers in 'Profitability' columns
# Q1 represent the lower quartile; Q1 = (n+1)*0.25th value
Q1 <- quantile(df_rounded$Profitability, .25)

# Q3 is the upper quartile; Q3 = (n+1)*0.75th value
Q3 <- quantile(df_rounded$Profitability, .75)

# IQR represent the inter quartile, i.e., median value ; IQR = Q3 - Q1 
#inter quartile range
IQR_P <- IQR(df_rounded$Profitability)

# subset() function in R is used to extract the rows from dataframe as per the condition
# Every single value in Profitability column is checked again whisker's range
# Only those rows are stored in no_outliers dataframe variable which satisfies the condition.
# In a nutshell, we have setup a rule for the outlier, in order to get a realistic output
no_outliers <- subset(df_rounded, 
                      df_rounded$Profitability > 
                        (Q1 - 1.5*IQR_P) & df_rounded$Profitability 
                      < (Q3 + 1.5*IQR_P))


# check how many rows were filtered out after removal of outliers
dim(no_outliers)

#Draw graph to check if outliers have been removed
ggplot(no_outliers, aes(x=Profitability, y=Worldwide.Gross)) + 
  geom_boxplot(outlier.colour = "red", 
               outlier.shape = 19)+ 
  scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))


#Summary Before
summary(df_rounded)

#Summary After 
summary(no_outliers)


#Remove outliers in Worldwide.Gross columns
Q1_W <- quantile(no_outliers$Worldwide.Gross, .25)
Q3_W <- quantile(no_outliers$Worldwide.Gross, .75)

#Inter quartile range
IQR_W <- IQR(df_rounded$Worldwide.Gross)

no_outliers_W <- subset(no_outliers, no_outliers$Worldwide.Gross > 
                          (Q1_W - 1.5*IQR_W) & no_outliers$Worldwide.Gross < 
                          (Q3 + 1.5*IQR_W))


#Draw graph to check if outliers have been removed from Worldwide table
ggplot(no_outliers_W, 
       aes(x=Profitability, y=Worldwide.Gross)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 19) + 
  scale_x_continuous(labels = scales::comma) + 
  coord_cartesian(ylim = c(0, 1000))


dim(no_outliers_W)


########  Uni-Variate Analysis
#Summary Before
summary(no_outliers)

#Summary After 
summary(no_outliers_W)

# Histogram graph shows the distribution of data 
# break=10 - you are breaking down the data into 10 equal parts
# which are represented in bars
# The graph represents the concentration of data points fall between 1-3
hist(no_outliers_W$Profitability, breaks=10)


#ggplot(no_outliers, aes(x=Profitability)) +
#  geom_histogram(binwidth = 50, fill = "blue", color = "black")

# geom_density() - this function is used to see the distribution of the data points and point out the possible 
# exceptions or outliers. The profitability data points along x-axis is drawn against the probability between 0-1
# The nice bell shaped curve with a possible skewness towards bottom right hands shows the possible outliers
ggplot(no_outliers, aes(x=Profitability)) + geom_density()



########## Bi-Variate ANALYSIS ##########
########## Audience Score vs. Profitability #######

#Step1 - Check outlier in Audience score only - BEFORE REMOVAL OF OUTLIER FROM BOTH COLUMNS
ggplot(no_outliers_W, 
       aes(x=Audience..score.., y=Profitability)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 19) + 
  scale_x_continuous(labels = scales::comma) + 
  coord_cartesian(ylim = c(0, 10))

#Step2 - Remove outliers, if identified by using the below command
Q1_Ad <- quantile(no_outliers_W$Audience..score.., .25)
Q3_Ad <- quantile(no_outliers_W$Audience..score.., .75)
IQR_Ad <- IQR(no_outliers_W$Audience..score..)

# Use of subset() filter funciton to extract only those rows where audience..score is between the calculated range
no_outliers_Ad <- subset(no_outliers_W, no_outliers_W$Audience..score.. > 
                           (Q1_Ad - 1.5*IQR_Ad) & no_outliers_W$Audience..score..< 
                           (Q3_Ad + 1.5*IQR_Ad))

## Check if outliers have been removed
ggplot(no_outliers_Ad, 
       aes(x=Audience..score.., y=Profitability)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 19) + 
  scale_x_continuous(labels = scales::comma) + 
  coord_cartesian(ylim = c(0, 10))

# dim() function shows the number of rows and column, known as dimentions in R
dim(no_outliers_Ad)

# summary() function is very useful, provides the mean, median, min and max 
summary(no_outliers_Ad)

### Removal of outliers from Profitability, in comparision with Audience Score
Q1_Pft <- quantile(no_outliers_Ad$Profitability, .25)
Q3_Pft <- quantile(no_outliers_Ad$Profitability, .75)
IQR_Pft <- IQR(no_outliers_Ad$Profitability)

no_outliers_Bi <- subset(no_outliers_Ad, no_outliers_Ad$Profitability > 
                           (Q1_Pft - 1.5*IQR_Pft) & no_outliers_Ad$Profitability < 
                           (Q3_Pft + 1.5*IQR_Pft))


# checking the output after removal of outliers from profitability column.
# You will see the grap has been improved but not 100% outliers removed
ggplot(no_outliers_Bi, aes(x=Audience..score.., y=Profitability)) + 
  geom_boxplot(outlier.colour = "red", 
               outlier.shape = 19)+ 
  scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 10))

# Now, we will draw the scatter boxplot to check the co-relation between two columns
# It could either be positive or negative co-relation. 
# The scatter plot shows a positive but weak correlation between two variables.

ggplot(no_outliers_Bi, aes(x= Audience..score.., y= Profitability)) + 
  geom_point() + scale_y_continuous(labels=scales::comma) + 
  coord_cartesian(ylim = c(0,10)) + 
  theme(axis.text.x.bottom = element_text(angle=90))

#scatterplot for Lead.Studio vs Rotten. Tomatoes..
#Not sure if this was mandatory but adding it, just incase
#The scatter plot clearly shows that "Independent" studio received several points 
# Rotten Tomatoes points, meaning producting flop movies
ggplot(no_outliers_Bi, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + 
  geom_point() + scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+
  theme(axis.text.x = element_text(angle = 90))


## Calculate correlation coefficent
# The positive value close to 1 shows strong correlation between Audience Score vs Profitability
# In our case, the correlation value of "[1] 0.2723181" shows a relatively moderate positive correlation

correlation <- cor(no_outliers_Bi$Audience..score.., no_outliers_Bi$Profitability)
print(correlation)

# Perform linear regression
# Linear model function can be used to assess the relation between 
# Independant and depandent variables, i.e., Audience..Score = Independent & Profitability = Depandant 
lm_model <- lm(Profitability ~ Audience..score.., data = no_outliers_Bi)
summary(lm_model)




#Visualise data
barplot(no_outliers_Bi$Profitability, 
        main="Bar Chart - Profitability Distributed Data", 
        labels=names(no_outliers_Bi$Profitability), 
        col=c("red", "green", "blue"))

# Bar Plot
ggplot(no_outliers_Bi, aes(x=Audience..score..)) + 
  geom_bar() + coord_cartesian(ylim = c(0,5))

# Line Plot
ggplot(no_outliers_Bi, aes(x=Audience..score.., y=Profitability)) + 
  geom_line() + coord_cartesian(ylim = c(0,10))


#Export clean data
# write.csv() function accepts two arguments, first is the data frame
# 2nd is the outpout file name

outpout_file_name <- "no_outliers_Bi_HollywoodsMostProfitableStories.csv"
output_file_path <- file.path(file_dir, outpout_file_name)

write.csv(no_outliers_Bi, file = output_file_path)





