### Lead PA Examples

## You all cannot get access to this data set, unless you sign agreements
## with the organization indicating how you plan to use it for research
## I am merely using this data set to show you all some examples of how you can
## explore your data once you have actuqlly gone through the data gathering process
## If you are curious as to whete I got the data, please refer to the below link
## https://www.ustranssurvey.org/data-requests-general

## Setting Working Directory

setwd ("C:/Users/cogps/Desktop/LEAD PA Documents/Transgender Data 2015")


## Reading Data
#install.packages("haven")
library(haven)

Tran_2015 <- read.csv("2015 USTS PUDS SPSS - NO IRB 121817.csv")


Tran_2015_sub <- subset(Tran_2015, select =c("q16_1", "q16_2","q16_3",
                                             "q16_4","q16_5","q16_6",
                                             "q16_7","q16_8",
                                             "q16_9","q16_10","q16_11",
                                             "q16_12","race", "suicidethoughts", 
                                             "suicideattempt", "agefirstattempt", 
                                             "agelastattempt", "q2_2_23"
))


### Coding Conditional Variables

Tran_2015_sub$TransFemColor <- with(Tran_2015_sub, ifelse(q2_2_23 == 1 & race == 5, "BlackTransFem",
                                                          ifelse(q2_2_23==1 & race ==9, "whiteTransFem", NA)) )
summary(as.factor(Tran_2015_sub$TransFemColor)) 


## dropping rows that are neither Black or white transgender women


Tran_2015_sub<-subset(Tran_2015_sub, !is.na(TransFemColor))

summary(as.factor(Tran_2015_sub$TransFemColor)) 

## Marking Values 99, 88, 999, and 888 as NA in entire data set

Tran_2015_sub[ Tran_2015_sub == 88 ] <- NA
Tran_2015_sub[ Tran_2015_sub == 99 ] <- NA
Tran_2015_sub[ Tran_2015_sub == 999 ] <- NA
Tran_2015_sub[ Tran_2015_sub == 888 ] <- NA



## Descriptives 
  ## first 6 rows
head(Tran_2015_sub)

  ## last 6 rows
tail(Tran_2015_sub)

## structure of data set

str(Tran_2015_sub)

## the minimum of a variable
min(Tran_2015_sub$agelastattempt, na.rm = T)

max(Tran_2015_sub$agelastattempt, na.rm = TRUE)

mean(Tran_2015_sub$agelastattempt, na.rm = TRUE)

range(Tran_2015_sub$agelastattempt, na.rm = TRUE)

median(Tran_2015_sub$agelastattempt, na.rm = TRUE)

## checking out Quantiles of Variable


    ## 50th Quantile
quantile(Tran_2015_sub$agelastattempt, na.rm = TRUE, .5)


    ## 75th Quantile
quantile(Tran_2015_sub$agelastattempt, na.rm = TRUE, .75)


## Modifying the Margins of Figures

?par
par(mar=c(5, 4, 4, 2))

#boxplot(Tran_2015_sub$q16_9, na.rm = TRUE)
quantile(Tran_2015_sub$agelastattempt, probs = c(0, .25,  .5, .75, 1), na.rm = TRUE)



## Box plot Per Condition -- Age of last suicide attempt
boxplot(agelastattempt ~ TransFemColor, data=Tran_2015_sub, na.rm = TRUE,
        ylim = c(0, 27), las =1, main = "Suicide Attempts by Group",
        ylab = "Quantity")

quantile(Tran_2015_sub$agelastattempt, probs = c(0, .25,  .5, .75, 1), na.rm = TRUE)

?boxplot

## Making Boxplots Tutorial
# https://www.youtube.com/watch?v=U64yNvlhv9I



# Standard Deviation
sd(Tran_2015_sub$agelastattempt, na.rm = TRUE)

# Variance
var(Tran_2015_sub$agelastattempt, na.rm = TRUE)



## gets you Summary Statistics per group of a Categorical Variable of Choice
by(Tran_2015_sub[, 1:4],Tran_2015_sub[,"TransFemColor"],summary, na.rm = TRUE)


## Cross Tabs

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
## documentation https://rpubs.com/PaulWilliamson/6975

# q16_3 -- past 12 months did you try to kill yourself?
#ratio
crosstab(Tran_2015_sub, col.vars = "q16_3", row.vars = "TransFemColor", type = c("r"))


# frequency
crosstab(Tran_2015_sub, col.vars = "q16_3", row.vars = "TransFemColor", type = c("f"))

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/chisq.test
# Compute the p-value with Monte Carlo simulation

## Chi Square test 

chisq.test(Tran_2015_sub$TransFemColor, Tran_2015_sub$q16_3, correct=FALSE, simulate.p.value = FALSE)

  # p value .001 -- sig.
  # Black transgeder women were more likely to report that during the past 12 months
  # they attempted to kill themselves. This, in comparison to white transgender women. 

# - Descriptive Statistics
# https://statsandr.com/blog/descriptive-statistics-in-r/


# q16_9 How many times have you tried to kill yourself?
# 1-25, 26 more than 25 -- then the regular 88 and 99 
levels(as.factor(Tran_2015_sub$q16_9))
# this is numeric from 1-26


## Anova (analysis of varaince)

Tran_2015_sub$q16_9<-as.numeric(Tran_2015_sub$q16_9)

one.way <- aov(q16_9 ~ TransFemColor, data = Tran_2015_sub)

summary(one.way)
          # not significant at a .786

# regressions
# https://www.datacamp.com/community/tutorials/linear-regression-R
# q16_10 How old were you when you tried to kill yourself?

## Dummy coding Categorical Variable 

table(as.factor(Tran_2015_sub$TransFemColor))
Tran_2015_sub$TransFemColorD <- ifelse(Tran_2015_sub$TransFemColor == 'BlackTransFem', 0, 1)
table(as.factor(Tran_2015_sub$TransFemColorD))

## Mean Centering Continuous IV 
# q16_10 How old were you when you tried to kill yourself?

Tran_2015_sub$q16_10_C <- Tran_2015_sub$q16_10 - mean(Tran_2015_sub$q16_10, na.rm = T)

# regressing on q16_9: How many times have you tried to kill yourself?

LinearRegression = lm(q16_9~TransFemColorD + q16_10_C, data = Tran_2015_sub) #Create a linear regression with two variables
summary(LinearRegression) #Revi
?lm
