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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2", "stargazer"),  pkgTest)

#####################
# Problem 1
#####################

# y = data set of 25 students’ IQ scores
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

mean(y)
sd(y)
length(y)

y_mean <- mean(y)
y_sd <- sd(y)
y_se <- sd(y)/sqrt(length(y))
y_length <- length(y)

# 90% CI for the average students' IQ

## means our confidence coefficient is 0.90, leaving 5% in each tail 
## can't use qnorm because n<30
## use t value instead
?qt
qt(0.05, df = (y_length) - 1)
qt(0.95, df = (y_length) - 1)

# alternatively, the lower.tail argument directly asks for the upper tail
qt(0.05, df=(y_length)-1, lower.tail=FALSE)

# now that we know our t-score, we can calculate the CI
t_score <- qt(0.95, df = (y_length) - 1)

lower_95_t <- y_mean - t_score * y_se
upper_95_t <- y_mean + t_score * y_se
lower_95_t; y_mean; upper_95_t

# the counselor can be 90% confident that the average student in her school
# will have an IQ between 93.96 and 98.4


############## hypothesis testing ##############

# h0 = school IQ is 100, having no difference with other schools' IQ
# ha = school IQ is above 100, higher than other schools' average IQ 
  # so this test is one sided 

# conduct the appropriate hypothesis test with α = 0.05. 
?t.test
t.test(y, mu = 100)

# we reject the null hypothesis
# however the alternative hypothesis is also wrong?
# the counselor's school IQ has a lower mean (98.4) than the national mean of 100 



  
#####################
# Problem 2: POLITICAL ECONOMY
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

# plot the relationships among Y, X1, X2, and X3
str(expenditure)

# plot the relationship between Y and Region?
?plot
as.factor(expenditure$Region)
?name()

plot((expenditure$Y), (expenditure$Region),
     xlab = "Expenditure per capita",
     ylab = "Region")

# plot the relationship between Y and X1?
plot((expenditure$Y), (expenditure$X1),
     xlab = "Expenditure per capita",
     ylab = "Personal income per capita"
     )

?ggplot

ggplot(
  data = expenditure,
  mapping = aes(x = Y, y = X1)
  ) +
  geom_point(mapping = aes(colour = as.factor(Region), 
                           shape = as.factor(Region))
             ) +
  labs(
    title = "Housing Assistance Expenditure and Per Capita Personal Income",
    subtitle = "Per Capita In State",
    x = "Expenditure on shelters/housing assistance", y = "Personal Income",
    color = "Region", shape = "Region")

