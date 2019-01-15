####################################
###### Final Project 335 Code ######
####################################

# Loading in Data (Remember to place Final335dt in your working directory)
dtMain <- read.csv("Final335dt")

## Ommitting unecessary variables dtMain, making dt my data set
dt <- data.frame(dtMain$EMPSTAT, dtMain$EDUC, dtMain$SEX, dtMain$MARST, 
                 dtMain$AGE, dtMain$PERWT, dtMain$STATEFIP, dtMain$SCHOOL)

## Ommitting NA's, which are coded as 0 initially.
dt = dt[dt$dtMain.EMPSTAT > 0,]

# Recoding y values to be 0 or 1.
# 1 is employed, 0 is unemployed or not in the labor force
dt$dtMain.EMPSTAT[dt$dtMain.EMPSTAT==1] <- 1 # employed
dt$dtMain.EMPSTAT[dt$dtMain.EMPSTAT==2] <- 0 # unemployed
dt$dtMain.EMPSTAT[dt$dtMain.EMPSTAT==3] <- 0 # not in the labor force

## Setting education to 0 if high school edu or lower, and 1 if at least 1 year of college or more
dt$dtMain.EDUC[dt$dtMain.EDUC < 7] <- 0 # no schooling up to the 12th grade
dt$dtMain.EDUC[dt$dtMain.EDUC > 6] <- 1 # 1 year of college or more


## Making Gender 0 and 1. 0=male, 1=female
dt$dtMain.SEX[dt$dtMain.SEX==1] <-0 # male
dt$dtMain.SEX[dt$dtMain.SEX==2] <-1 # female

## Change married into 0 and 1:
dt$dtMain.MARST[dt$dtMain.MARST <= 2] <- 1 # Married with spouse absent/present
dt$dtMain.MARST[dt$dtMain.MARST >2] <- 0 # Never married 0r sperated or divorced or widowed 

## Omitting those over 60 and under 18.
dt = dt[dt$dtMain.AGE < 60,]
dt = dt[dt$dtMain.AGE > 17,]

## Both the School and States variables do not need to be recoded because they are treated as factors


# Renaming columns of Data
# colnames(dt) <- c("Y","X1","X2","X3","X4")
colnames(dt) <- c("EmploymentStatus","EducationalAttainment","Gender",
                  "MaritalStatus","Age", "Perwt","State", "School")


## Summary Stats table function (Not used in report)
tmp <- do.call(data.frame, 
               list(mean = apply(dt, 2, mean),
                    median = apply(dt, 2, median),
                    sd = apply(dt, 2, sd),
                    min = apply(dt, 2, min),
                    max = apply(dt, 2, max),
                    n = apply(dt, 2, length)))
tmp

# Using logistic regression, however 2 unresolved issues occur:
# 1. Person weighting cannot be used with the following marginal effects function.
# 2. It is unclear how to account for heteroskedastic errors with the logit model in R

# Without person weighting
library(mfx)
logit1 <-logitmfx(EmploymentStatus ~ EducationalAttainment + Gender + MaritalStatus + Age +
           I(Age^2) + (Gender*MaritalStatus) + as.factor(State) + as.factor(School), 
           data=dt)
summary(logit1)

#With person weighting (coefficient estimates in the trillions)
logit2 <- glm(EmploymentStatus ~ EducationalAttainment + Gender + MaritalStatus + Age +
                I(Age^2) + (Gender*MaritalStatus) + as.factor(State) + as.factor(School), 
                family=binomial, data=dt, weights= Perwt)

summary(logit2)


#########################################################
### Linear Probability Model with Main Regressor Only ###
#########################################################
# Loading libraries to account for heteroskedasticity (Only works for linear models)
library(RCurl)
library(gdata) 
library(zoo)
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

options(scipen=5) # Prevents my coefficient outputs from being in scientific notation
lm1 <- lm(EmploymentStatus ~ EducationalAttainment, data=dt, weights = Perwt)

summary(lm1, robust = T)

# Generating nice graphics
library(stargazer)
stargazer(lm1, type = "html", out="lm1.htm")

#############################################################
### Linear Probability model accounting for State effects ### (Main Model)
#############################################################
library(cdlTools) # Change codes to State Name
#dt$State <- fips(dt$State, to= "Abbreviation")
# Changes p-values dramatically, Therefore I can no longer change the state codes to the state Name
lm2 <- lm(EmploymentStatus ~ EducationalAttainment + Gender + MaritalStatus + Age +
            I(Age^2) + (Gender*MaritalStatus) + School + as.factor(State), data=dt, weights = Perwt)

summary(lm2, robust = T)

# Generates graphic
library(stargazer)
stargazer(lm2, type = "html", out="lm2.htm")
