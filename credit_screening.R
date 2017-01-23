# Chad R. Bhatti's Week 3 Class Notes
# credit_screening.R

# UCI Credit Screening Data Set
# https://archive.ics.uci.edu/ml/datasets/Credit+Approval

my.path <- '~/Dropbox/NU/ADVANCED_MODELING/';
my.file <- paste(my.path,'credit_screening.csv',sep='');
my.data <- 	read.csv(my.file, header=FALSE);
head(my.data)

col.names <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16');

colnames(my.data) <- col.names;
head(my.data)

# Let's take a look at the structure of the data frame;
str(my.data)

# Notice that the string data has been read in as factors;
# Notice that the data has missing values denoted by '?';
# '?' is not a proper missing value in R;
# Notice that A2 should be numeric, but since there is '?' as a value
# R treats all values as a character values and reads it in as a factor;

table(my.data$A1)


# Let's look at some options for read.csv();
help(read.csv)

# Use read.csv() options to read in the text data as strings;
string.data <- read.csv(my.file,header=FALSE,as.is=TRUE,na.strings=c('?'));
colnames(string.data) <- col.names;

# Display the structure;
str(string.data)

# Note that when we use the table() function we need to account for NA values;
table(string.data$A1)
table(string.data$A1,useNA=c('always'))

# Remove any observations (rows) with missing values;
complete.df <- na.omit(string.data);
head(complete.df)

# Check the difference in sizes;
dim(string.data)
dim(complete.df)

# Rows with NAs

not.na <- seq(1,dim(string.data)[1],1)*complete.cases(string.data);
na.data <- string.data[-not.na,];
dim(na.data)


# See how R handles missing values.  These missing values were defined by the na.strings
# argument in read.csv();

# In this example we will not use observations with missing values since they are 
# relatively few; If you have a lot of missing values, then you need to consider whether 
# they are 'uncertainty' (or unknown) or whether they are 'signal'.  If they are signal, 
# then you would need to code them as valid values;


#################################################################################
#################################################################################
# Part 2
#################################################################################
#################################################################################
# How do we analyze and model this data?
#################################################################################
#################################################################################
# We need to turn (or transform) this character/string data into variables that
# can be used in a regression model;
# Remember, A2, A3, A8, A11, A14, A15 are continuous;
# A16 is the response variable;
# We need to do something with A1, A4, A5, A6, A7, A9, A10, A12, A13;

# In general creating a bunch of copies of your data frame is not a good practice;
# The copies simply chew up your memory;
# However, for discussion purposes we need to have the copies;

model.df <- complete.df;

# If we want to fit a logistic regression model to this data, then we need to examine 
# each categorical variable and code a family of indicator variables;

table(model.df$A1)
#> table(model.df$A1)
#  a   b 
#203 450 

model.df$A1.b <- as.numeric(model.df$A1=='b');
# What does R return if we do not use as.numeric()?;
# Code the category (or categories) with the most observations as indicator variables;
# Remember we want to estimate a coefficient for the indicator variables, hence we need
# observations in order to estimate that coefficient well;


table(model.df$A4)
#> table(model.df$A4)
#  l   u   y 
#  2 499 152 

model.df$A4.u <- as.numeric(model.df$A4=='u');

table(model.df$A5)
#> table(model.df$A5)
#  g  gg   p 
#499   2 152 

# How many categories should we code?  Why?

model.df$A5.g <- as.numeric(model.df$A5=='g');

table(model.df$A6)
#> table(model.df$A6)
# aa   c  cc   d   e  ff   i   j   k   m   q   r   w   x 
# 52 133  40  26  24  50  55  10  48  38  75   3  63  36 


model.df$A6.aa <- as.numeric(model.df$A6=='aa');
model.df$A6.c  <- as.numeric(model.df$A6=='c');
model.df$A6.cc <- as.numeric(model.df$A6=='cc');
model.df$A6.d  <- as.numeric(model.df$A6=='d');
model.df$A6.e  <- as.numeric(model.df$A6=='e');
model.df$A6.ff <- as.numeric(model.df$A6=='ff');
model.df$A6.i  <- as.numeric(model.df$A6=='i');
model.df$A6.k  <- as.numeric(model.df$A6=='k');
model.df$A6.m  <- as.numeric(model.df$A6=='m');
model.df$A6.q  <- as.numeric(model.df$A6=='q');
model.df$A6.w  <- as.numeric(model.df$A6=='w');
model.df$A6.x  <- as.numeric(model.df$A6=='x');


table(model.df$A7)
#> table(model.df$A7)
# bb  dd  ff   h   j   n   o   v   z 
# 53   6  54 137   8   4   2 381   8 

model.df$A7.bb  <- as.numeric(model.df$A7=='bb');
model.df$A7.ff  <- as.numeric(model.df$A7=='ff');
model.df$A7.h   <- as.numeric(model.df$A7=='h');
model.df$A7.v   <- as.numeric(model.df$A7=='v');


table(model.df$A9)
#> table(model.df$A9)
#  f   t 
#304 349 

model.df$A9.t <- as.numeric(model.df$A9=='t');


table(model.df$A10)
#> table(model.df$A10)
#  f   t 
#366 287 

model.df$A10.f <- as.numeric(model.df$A10=='f');


table(model.df$A12)
#> table(model.df$A12)
#  f   t 
#351 302 

model.df$A12.f <- as.numeric(model.df$A12=='f');


table(model.df$A13)
#> table(model.df$A13)
#  g   p   s 
#598   2  53 

model.df$A13.g <- as.numeric(model.df$A13=='g');



# Define our response variable;
model.df$Y <- as.numeric(model.df$A16=='+');



# Now let's fit a logistic regression model;

glm.1 <- glm(Y ~ A2 + A3+ A8 + A12.f + A13.g, family='binomial', data=model.df);

summary(glm.1)




##########################################################################################
# Follow Up Questions:
##########################################################################################
(1) How do we perform EDA in this scenario?  EDA for categorical data is different than
	EDA for continuous data, i.e. EDA for logistic regression is different than EDA for
	OLS regression.

(2) Is the model glm.1 a good model?  How should we evaluate a logistic regression model?
	What tools do we have in R?  Do we need to build our own tools?  Can you find a 
	better model?

(3) Would this coding of the data support other modeling techniques other than
	logistic regression?  Can we using this coding in a tree/RF or SVM?

# Hint:  There are better models than the glm.1 model;
##########################################################################################
































