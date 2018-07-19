#-------------------------------------
# Lab 3 - confirmatory factor analysis
#-------------------------------------
library("lavaan")

# Set the working directory to wherever you've saved the data "pisacfa.Rda" 
setwd("C:/Users/leslieru/OneDrive/Teaching/SEM/Data/PISA15_STU/")


# Load "pisacfa.Rda" 
load("pisacfa.Rda")


# Take a quick look at the data
str(pisacfa)              # Looks at "structure" of database
summary(pisacfa)          # Produces mean and 5 number summary of all variables

lapply(pisacfa[, -1], 
       function(x) mean(x, na.rm = TRUE )) # Use lapply to produce just means for numeric variables               

cor(pisacfa[, -1], use = "complete.obs")   # Correlations of all numeric variables
cov(pisacfa[, -1], use = "complete.obs")   # Covariances of all numeric variables


# Specify a simple CFA model with the ICT interest and competence variables
# Default is to fix first indicator loading to 1
ictint_m1 <- 'intcomp =~ INTICT + COMPICT + AUTICT + SOIAICT'


# Fit the ictint_m1 model
fit_m1 <- cfa(ictint_m1, data = pisacfa)


# Summarize the results
summary(fit_m1, fit.measures = TRUE, standardized = TRUE) # Gives results under both ways of setting LV metric
modindices(fit_m1, minimum.value = 3.84)  # This can also be in the summary statement. But here, we
                                          # can control the minimum value

# Modification, based on modindices
ictint_m2 <- 'intcomp =~ INTICT + COMPICT + AUTICT + SOIAICT
              INTICT ~~  AUTICT'


# Fit the ictint_m1 model - these are equivalent in fit
fit_m2a <- cfa(ictint_m2, data = pisacfa, meanstructure = TRUE)
fit_m2b <- cfa(ictint_m2, data = pisacfa, meanstructure = FALSE)


# Summarize the results
summary(fit_m2a, fit.measures = TRUE)
summary(fit_m2b, fit.measures = TRUE)


# An alternative way to fit LV standardized (Std.lv) "by hand"
ictint_Std.lv <- 'intcomp =~ NA*INTICT + COMPICT + AUTICT + SOIAICT
                  intcomp ~~ 1*intcomp'

# Fit equivalent models (ictint_m1 is from above)
fit_Std.lv <- cfa(ictint_Std.lv, data = pisacfa)
fit_m1b <- cfa(ictint_m1, data = pisacfa, std.lv = TRUE)


# Summarize the results
summary(fit_Std.lv, fit.measures = TRUE, standardized = TRUE) # Just to check equivalence
summary(fit_m1b, fit.measures = TRUE, standardized = TRUE)









