#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# loading necessary packages
lapply(c("ggplot2", "stargazer", "tidyverse"),  pkgTest)


##########################################
# QUESTION 1: POLITICAL SCIENCE
##########################################

# (a) calculate the chi^2 test statistic

# bringing the table into r as a matrix (frequencies observed)
fo <- matrix(c(14, 7, 6, 7, 7, 1), 
             nrow = 2, ncol = 3, 
             dimnames = list(
               Class = c("Upper", "Lower"),
               Outcome = c("Not Stopped", "Bribe Requested", "Stopped/Warned")
               )
             )

# calculating totals

# column totals
c1 <- sum(fo[,1]) # column 1 not stopped 
c2 <- sum(fo[,2]) # column 2 bribe requested
c3 <- sum(fo[,3]) # column 3 stopped/given warning

# row totals
r1 <- sum(fo[1,]) # row 1 upper class
r2 <- sum(fo[2,]) # row 2 lower class

# checking grand total
sum(c1, c2, c3) == sum(r1, r2)
gt <- sum(c1, c2, c3)

# calculating frequencies expected
c1xr1 <- (c1*r1)/gt # not stopped, upper class
c1xr2 <- (c1*r2)/gt # not stopped, lower class
c2xr1 <- (c2*r1)/gt # bribe requested, upper class
c2xr2 <- (c2*r2)/gt # bribe requested, lower class
c3xr1 <- (c3*r1)/gt # stopped/warned, upper class
c3xr2 <- (c3*r2)/gt # stopped/warned, lower class

# matrix-ing the frequencies expected?
fe <- matrix(c(c1xr1, c1xr2, c2xr1, c2xr2, c3xr1, c3xr2), 
             nrow = 2, ncol = 3,
             dimnames = list(
               Class = c("Upper", "Lower"),
               Outcome = c("Not Stopped", "Bribe Requested", "Stopped/Warned")
               )
             )
fe
# two expected frequency counts fall below 5 (4.64 and 2.86), 
# and one is barely above 5 (5.14), 

# calculating the chi^2 statistic
difference <- (fo-fe)^2/fe
chisq <- sum(difference)
chisq

# (b) calculating the p-value from the chi^2 statistic we just created

# calculating degrees of freedom first
# df = (r-1)(c-1)
df <- (nrow(fo)-1) * (ncol(fo)-1) 

# p-value for chi^2 is skewed right and strictly positive
# always use the upper tail!
p_val <- pchisq(chisq, df = df, lower.tail = F)

# running it the non-manual way to check
chisq; df; p_val
chisq.test(fo)
# 3.791168; 2; 0.1502306
# X-squared = 3.7912, df = 2, p-value = 0.1502

# conclusion if alpha = 0.1
tolerance <- 0.1
p_val < tolerance # FALSE
# we fail to reject the null because the p-value > 0.1.
# there is insufficient evidence at the 10% significance level
# that social class and outcome of police encounters are associated/dependent. 

# (c) standardised/adjusted residuals ("z", test statistic)
# t is always (observation-null expectation)/se
# z = (fo-fe)/sqrt(fe(1-row prop.)(1-col prop.))
# shows how far each fo is from fe in correct unit

# calculating proportions
cp1 <- c1/gt
cp2<- c2/gt
cp3 <- c3/gt
rp1 <- r1/gt
rp2 <- r2/gt

proportions <- matrix(c((1-rp1)*(1-cp1), (1-rp2)*(1-cp1),
                        (1-rp1)*(1-cp2), (1-rp2)*(1-cp2),
                        (1-rp1)*(1-cp3), (1-rp2)*(1-cp3)),
                      nrow = 2, ncol = 3,
                      dimnames = list(
                        Class = c("Upper", "Lower"),
                        Outcome = c("Not Stopped", "Bribe Requested", "Stopped/Warned")
                        )
                      )

z <- (fo-fe)/sqrt(fe*proportions)
z
# comparing it with the non-manual way 
chisq.test(fo)$stdres

# (d) using the standardised residuals to interpret results

qnorm(0.95) # 1.644854
# for 90% CL, keeping alpha = 0.1 consistent, critical value = 1.645

# because there's only two grouping rows, the two mirror each other

# no standardised residual reaches the |z| = 1.645 cutoff
# i.e., generally no cell is an 'extreme' value under null conditions
# consistent with the overall χ² test where we failed to reject H0

# however the bribe-requested column VERY narrowly misses the cutoff
# law enforcement solicit bribes less than expected from UC 
# and more than expected from LC
# this suggests class-based differences in bribe solicitation, 
# but not statistically significant at α = 0.1. 

# the stopped/warned column also narrowly misses the cutoff 
# the trend is reversed; law enforcement stops or warns UC more than expected
# and stops/warns LC drivers less than expected,
# but once again NOT statistically significant at α = 0.1. 

# the not stopped column is almost exactly the same as expected


##########################################
# QUESTION 2: ECONOMICS
##########################################

experiment <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
head(experiment)
str(experiment)

# 1. assumptions
#   a. type of data: quantitative, although one is binary
#   b. randomisation: yes
#   c. population distribution: sampling distribution will be approximately normal
#   d. large sample size (much larger than 30)

# we can check some of these assumptions
reserved_villages <- subset(experiment, reserved == 1)
unreserved_villages <- subset(experiment, reserved == 0)

# number of villages by policy
nrow(reserved_villages)    # 108 villages
nrow(unreserved_villages)  # 214 villages

# number of facilities by reservation policy
n1 <- sum(reserved_villages$water) # 2591
n2 <- sum(unreserved_villages$water) # 3154

# mean of facilities per policy group 
reserved_mean <- mean(reserved_villages$water) # 23.99074
unreserved_mean <- mean(unreserved_villages$water) # 14.73832

# 2. hypotheses
# (a) a null and alternative hypothesis
# H0: villages with and without reservation policies build/repair water facilities equally
# HA: villages with and without reservation policies do not build/repair water facilities equally

# (b) run a bivariate regression to test this hypothesis
# regression model: Y = β₀ + β₁X₁ + ε
# our model: water_hat = β₀_hat + β₁_hat(reserved) 

model <- lm(data = experiment, water ~ reserved)
summary(model)

# (c) interpret the coefficient estimate for the reservation policy

# when X/reserved == 0: 14.738 + 9.252(0) = 14.738
# when X/reserved == 1: 14.738 + 9.252(1) ≈ 24.0
# coef of 9.252 indicates a difference in average outcomes:
# in villages with no reservation policy,
# they built 14.738 on average
# in villages where a reservation policy is present,
# 9.252 more (≈ 24) water facilities were built on average

