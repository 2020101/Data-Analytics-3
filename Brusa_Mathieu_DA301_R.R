## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################


# Import libraries - this might take a minute or so.
# The whole tidyverse package.
library(tidyverse)
# Create insightful summaries of the data set.
library(skimr)
# Create insightful reports on the data set.
library(DataExplorer)

# Import a CSV file.
df <- read.csv(file.choose(), header=T)
# Print the data frame.
head(df,2)
str(df)  # info()
summary(df) # describe()
dim(df) # shape = (2000, 11)

# Performance EDA - foucs customer behavior specifically how loyalty points are
# accumulated
# Components - tables and visualization


# Questions worth exploring
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate

# Approach: Scatterplot to identify which numeric columns has the highest correlation coefficient
# Step1) Identify largest correlation coefficient - which is the strongest relationship with loyalty_points
# Step2) Once identified compare categorical columns - which subgroup has the strongest correlation with loyalty_points
# Step3) Explore subgroup to answer customer behavior
# This can include profile of most common within the data set - distribution, proportion of representation within the data set


# Step1) Setup; 
# (a) create a subset of data with only numeric columns
# (b) plot basic scatter plot and identify visually which is the most pronounce relationship
# (c) if it not clear calculate the pearson correlation coefficient
# Step2) Group with strongest relationship to loyalty points;
# (a) use original data set and identify which categorical columns fit; gender and education
# (b) plot more complex scatter plot that utilizes the categorical column and identify which in each category has the strongest 
# relationship with loyalty points
# (c) Calculate correlation of demo graphical groups
# Step3) Explore the distribution, proportion representation using table; count and sum

###################################################################################################
# Prep: Clean duplicates, rename columns for easier subsetting, look for outlier
# Duplicates exist?
duplicates <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
sum(duplicates)  # none exist

# Null exist?
sum(is.na(df_num))  # none exist

# rename columns 
colnames(df)[colnames(df) == "remuneration..k.."] <- "renumeration"
colnames(df)[colnames(df) == "spending_score..1.100."] <- "spending_score"

# education values inconsistent string format -> convert to lower case


# summary
summary(df)
####################################################################################################
# Step1) Setup; 
# (a) create a subset of data with only numeric columns
df_num <- df %>% keep(is.numeric)

# Outlier exist 
boxplot(df_num) # yes, loyalty_points

# Subset Outlier using upper wisker as condition 
# - to find out what percentage of the entire data set do they represent
# Equation for box plot upper wisker maximum (Q3 + 1.5*IQR)
upper_hinge <- quantile(df$loyalty_points, 0.75)
iqr <- IQR(df$loyalty_points)
upper_whisker <- upper_hinge + 1.5 * iqr

# Filtering rows based on the upper whisker
loyal_pts_outlier_df <- df[df$loyalty_points <= upper_whisker, ]
as_tibble(loyal_pts_outlier_df)

# Difference in number of rows
nrow(df)  # 2000 rows
nrow(loyal_pts_outlier_df)  # outliers equate to 1734 out of 2000 = 87%

# Filtering rows excluding the upper whisker
loyal_pts_nonoutlier_df <- df[!df$loyalty_points <= upper_whisker, ]
nrow(loyal_pts_nonoutlier_df)

# Visualize distribution comparing outlier and nonoutlier in loyalty_points
# Set up a side-by-side layout
par(mfrow = c(1, 2))

# Create the first histogram  - observation: closer to normal distribution
hist(loyal_pts_outlier_df$loyalty_points, main = "Outlier", xlab = "Value", ylab = "Frequency")

# Create the second histogram - observation: right skewed (positively skewed)
hist(loyal_pts_nonoutlier_df$loyalty_points, main = "Non-Outlier", xlab = "Value", ylab = "Frequency")

########### Observation and Approach#######################################################
# Keep outliers for initial exploration - processed with initial place 
# 3 basic strategies at this stage; Removal, Transformation(log, square root), and imputation(replacement with estimated values; median, mean, regression)
# # deleting outlier at is not an option since it outlier make up of 70% of data

# At ML linear regression stage the best approach will be determined by accuracy of model 
## Require creation 3 df each with a different normalization technique 
# column = loyalty_points
# techniques tested; Min-Max Scaling, Standardization (z-score normalization), and Log scaling normalization

########################################################################################


# (b) plot basic scatterplot and identify visually which is the most pronounce relationship
# View numeric columns to compare
colnames(df_num)

# Scatterplot to indicate relation between loyalty_points and age
ggplot(data = df_num,
       mapping = aes(x = loyalty_points, y = age)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  # Add the line of best fit to the plot.
  geom_smooth(method = 'lm', se=FALSE)

# Scatterplot to indicate relation between loyalty_points and renumeration
ggplot(data = df_num,
       mapping = aes(x = loyalty_points, y = renumeration)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  # Add the line of best fit to the plot.
  geom_smooth(method = 'lm', se=FALSE)

# Scatterplot to indicate relation between loyalty_points and spending_score
ggplot(data = df_num,
       mapping = aes(x = loyalty_points, y = spending_score)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  # Add the line of best fit to the plot.
  geom_smooth(method = 'lm', se=FALSE)

# Scatterplot to indicate relation between loyalty_points and product
ggplot(data = df_num,
       mapping = aes(x = loyalty_points, y = product)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  # Add the line of best fit to the plot.
  geom_smooth(method = 'lm', se=FALSE)

# (c) if it not clear calculate the pearson correlation coefficient
# strongest relationship are spending_score and renumeration - need to calculate which is strongest
round(cor(df_num$loyalty_points,df_num$renumeration),2)  # 2nd strongest 0.62
round(cor(df_num$loyalty_points,df_num$spending_score),2)  # Strongest relationship 0.67
###################################################################################################
# Step2) Group with strongest relationship to loyalty points; focus on spending_score

# (a) use original data set and identify which categorical columns fit; gender and education
# Categorical columns check original df; will utilize gender and education
colnames(df) 
summary(df)
# How determined which character
df_cate <- df %>% keep(is.character)
summary(df_cate)

# Columns sequence
colnames(df_cate)
# Identify number of unique values per columns
unique_values_per_column <- sapply(df_cate, function(x) length(unique(x)))
as_tibble(unique_values_per_column)

# Insight:
# Conclusion of exploration will use only gender and education for approach in next scatterplot section

# (b) plot more complex scatterplot that utilizes the categorical column and identify which in each category has the strongest 
# relationship with loyalty points
# Scatterplot to indicate relation between loyalty_points and spending_score: GENDER
ggplot(data = df,
       mapping = aes(x = loyalty_points, y = spending_score,
                     color=gender)) +
  geom_point(alpha = 0.5, size = 1.5) +
  # Add the line of best fit to the plot.
  geom_smooth(method = 'lm', se=FALSE)

# Scatterplot to indicate relation between loyalty_points and spending_score: EDUCATION
ggplot(data = df,
       mapping = aes(x = loyalty_points, y = spending_score,
                     color=education)) +
  geom_point(alpha = 0.5, size = 1.5) +
  # Add the line of best fit to the plot.
  geom_smooth(method = 'lm', se=FALSE)

# (c) Calculate correlation of demographies; gender
# female_data <- subset(data, gender == "female")
# Selecting necessary columns
gender_data <- select(df, c('gender', 'loyalty_points', 'spending_score'))

# Check 
head(gender_data)
colnames(gender_data)

# Filtering for gender: Female
female_df <- gender_data[gender_data$gender == 'Female', ]
# check
head(female_df)

# Filtering for gender: Male
male_df <- gender_data[gender_data$gender == 'Male', ]
# Check
head(male_df)

# Calculate correlation using gender specific df
cor(female_df$loyalty_points, female_df$spending_score)  # cor = 0.64 obs= 1120
cor(male_df$loyalty_points, male_df$spending_score)  # cor = 0.70 obs = 880

# Calculate correlation of demographies; education
# Selecting necessary columns
education_data <- select(df, c('education', 'loyalty_points', 'spending_score'))
head(education_data)

# Filtering per education
basic_df <- education_data[education_data$education == 'Basic',]
diploma_df <- education_data[education_data$education == 'diploma', ]
graduate_df <- education_data[education_data$education == 'graduate', ]
phd_df <- education_data[education_data$education == 'PhD', ]
postgraduate_df <- education_data[education_data$education == 'postgraduate', ]

# Calculate corr and identify strongest corr; spending_score per education
cor(basic_df$loyalty_points, basic_df$spending_score)  # cor 0.91 obs 50 
cor(diploma_df$loyalty_points, diploma_df$spending_score) # cor 0.78 obs 190 
cor(graduate_df$loyalty_points, graduate_df$spending_score) # cor 0.680 obs 900
cor(phd_df$loyalty_points, phd_df$spending_score) # cor 0.58 obs 460 
cor(postgraduate_df$loyalty_points, postgraduate_df$spending_score) # cor 0.684 obs 400 


# Insight: 
# Focus on graduate and post_graduates (weighted center) = 900 + 400 = 1300 out 2000 = 65%
# Graduate and post_graduates 0.68 corr positive relationship 
# Ranking by highest positive correlation; 1st 0.91, 2nd 0.78, 3rd 0.683, 4th 0.680
# 5th 0.58
# Count per education = obs details can be found above

# Approach: sequential step
# Deep dive of customer behavior = Female Graduate + Female Post_graduate 
# Group age range - sales responsible 
##############################################################################################
#Step3) How much does customer behavior from Female Graduate + Female Post_graduate
# generate in loyalty_points in comparison to total

total_loyalty_points <- sum(df$loyalty_points)
total_loyalty_points

# Subset demographic; Female and grad or postgrad
fem_grad <- df[df$gender == "Female" & df$education == "graduate", ]
fem_postgrad <- df[df$gender == "Female" & df$education == "postgraduate", ]

# Subset demographic; Male and grad or postgrad (context/compare)
male_grad <- df[df$gender == "Male" & df$education == "graduate", ]
male_postgrad <- df[df$gender == "Male" & df$education == "postgraduate", ]

# Verification - reminder of obs (can use dim())
head(fem_grad) # obs 530
head(fem_postgrad) # obs 240
head(male_grad) # 370
head(male_postgrad) # 160 

#How they behave ?
# Total loyalty points per sub-demographic group
sum(fem_grad$loyalty_points)  # 866257 obs 530 MOST
sum(male_grad$loyalty_points) # 633195 obs 370
sum(fem_postgrad$loyalty_points) # 383118 obs 240
sum(male_postgrad$loyalty_points) # 216513 obs 160

# Total loyalty points
total_loyalty_points  # 3156064

# Postgrad = spending_score
sum(fem_grad$spending_score) # 28569 obs 530  # MOST
sum(male_grad$spending_score) # 19241 obs 370
sum(fem_postgrad$spending_score) # 12595 obs 240
sum(male_postgrad$spending_score) # 7696 obs 160

# Total spending_score
total_spending_score <- sum(df$spending_score)
total_spending_score  # 100,000

# Insight: 
# total obs; Female 1120 ,Male 880
# total obs education; grad 900, postgrad 400
# total obs subgroup; Fem grad 530, Fem postgrad 240, Male grad 370, Male postgrad 160
# Female grad 530 + Female postgrad 240 = 770 out 1300 = represent 59% 
# 1300 out 2000 = 65% of total observation of data points available in data

# Conclusion the proportionally Female grad and Female postgrad represent
# the largest and influencial subgroup using spending_score and loyalty_points
# with the moderate positively strong correlation 0.68
# Female grad is responsible for generating 866257 out 3156064= 27% out all loyalty points
# Female postgrad generates 383118 out 3156064 = 12%
# Female grad + postgrad = 39% loyalty_points

# Further research:
# Check graduate exclude gender
# Check also customer with education level basic (extremely strong positive correlation loyalty program)
# Check also product that attract that generate the largest loyalty points
# Base on scatterplot loyalty_points vs spending_score - deep dive into spending_score 30-60 and 1000 to 2000 loyalty_points
# create profile based on most common/densely populated area (product)
# current state attract them understand what product is most attractive and how much loyalty does it generate

summary(fem_grad$loyalty_points)
summary(fem_grad$spending_score)


summary(fem_postgrad$loyalty_points)
summary(fem_grad$spending_score)

# Most popular product
#install.packages('dplyr')
#library(dplyr)
head(table(fem_grad$product))
table(fem_postgrad$product))

# Create a table
fem_pg_freq_table <- table(fem_postgrad$product)
# Convert table to data frame
fem_pg_freq_df <- as.data.frame(fem_pg_freq_table)
# Sort by counts in descending order
fem_pg_freq_df <- fem_pg_freq_df[order(-df$Freq), ]

fem_pg_freq_df

summary(fem_grad$product)

summary(fem_postgrad$product)

###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Focus on sales: from perspective of product and how it generates loyalty

## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.



summary(fem_grad$loyalty_points)
summary(fem_grad$spending_score)


summary(fem_postgrad$loyalty_points)
summary(fem_grad$spending_score)

# Most popular product
#install.packages('dplyr')
#library(dplyr)
head(table(fem_grad$product))
table(fem_postgrad$product))

# Create a table
fem_pg_freq_table <- table(fem_postgrad$product)
# Convert table to data frame
fem_pg_freq_df <- as.data.frame(fem_pg_freq_table)
# Sort by counts in descending order
fem_pg_freq_df <- fem_pg_freq_df[order(-df$Freq), ]

fem_pg_freq_df

summary(fem_grad$product)

summary(fem_postgrad$product)



###############################################################################
###############################################################################




