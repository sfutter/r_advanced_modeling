# Chad R. Bhatti
# 09.25.2014
# reshape_data.R

#########################################################################################
# There are two types of data formats: long and wide;

# Wide format is one observation (row) per ID with multiple
# columns on the same measured quantity at different points in time.

# Long format is multiple observations (rows) per ID with a single column for the 
# measured variable.  Each observation is for a single point in time.
# Long format is sometimes called longitudinal or panel data;

# In SAS this is PROC TRANSPOSE.  In R this is the reshape() function.
#########################################################################################

# Download data set and change my.path to your local directory;
my.path <- '~/Dropbox/NU/ADVANCED_MODELING/';

long.data <- read.csv(paste(my.path,'long_data.csv',sep=''));
head(long.data)

# Read the help file on reshape();
help(reshape)


#########################################################################################
# Reshape long to wide;
#########################################################################################
# Need to specify: v.names, timevar, idvar, direction, sep
wide.data <- reshape(data=long.data, v.names=c('X'), timevar=c('Year'), idvar=c('Name'), 
			direction=c('wide'), sep=c('.') 
		);

head(wide.data)



#########################################################################################
# Reshape wide to long;
#########################################################################################
# Need to specify: varying, ids, times, direction
new.long <- reshape(data=wide.data,varying=c('X.1994','X.1995','X.1996','X.1997','X.1998'),
			idvar=c('Name'),times=c(1994,1995,1996,1997,1998),direction=c('long')
		);

head(new.long)



#########################################################################################
# Now sort this data frame into the original order: Name, Year(time)
#########################################################################################
i <- order(new.long$Name, new.long$time);
sort.new.long <- new.long[i,];
rownames(sort.new.long)
rownames(sort.new.long) <- seq(1,dim(sort.new.long)[1],1);
head(sort.new.long)

# No way around having row names in R;
# R may sometimes attach garbage row names;  
# The only option is to overwrite them with a sequence of numbers;


# Note:  If you converted a long data format to a wide data format in R,
# then R will let you convert it back by simply recalling the reshape function.
# Try reshape(wide.data) and see what you get;




#########################################################################################
# Note that R likes . not _ in the variable names when using reshape();  
# Try this code and see the error message;
#########################################################################################

wide.data <- reshape(data=long.data, v.names=c('X'), timevar=c('Year'), idvar=c('Name'), 
			direction=c('wide'), sep=c('_') 
		);

head(wide.data)


new.long <- reshape(data=wide.data,varying=c('X_1994','X_1995','X_1996','X_1997','X_1998'),
			idvar=c('Name'),times=c(1994,1995,1996,1997,1998),direction=c('long')
		);



#########################################################################################

























