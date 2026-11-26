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
lapply(c("car", "stargazer"), pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#################### question 1: economics ####################

data(Prestige)
help(Prestige)
table(Prestige$type, useNA = "ifany")

# we see that 4 occupations have type = NA

# (a)  recoding <type>
# professional (prof) = 1 
# blue collar (bc) & white collar (wc) = 0

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
table(Prestige$professional, useNA = "ifany")

# (b) linear model 
lm1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(lm1)
nobs(lm1)  # 98 (4 NAs "deleted due to missingness")
stargazer(lm1, table.placement = "h", header = FALSE)

# (c) prediction equation
coef(lm1)
B1 <- 0.003170909
B2 <- 37.781279955
B3 <- -0.002325709

# predicted prestige = 
# 21.142258854 (β0)
# + 0.003170909 (β1) * income 
# + 37.781279955 (β2) * professional 
# + -0.002325709 (β3) * (income * professional)

# (d) income coef

# (e) professional coef

# (f) marginal effect of +$1000 income on prestige for professionals
(B1 + B3) * 1000

# (g) marginal effect of being professional when income = $6,000
B2 + (B3 * 6000) 


#################### question 2: lawn signs ####################

n <- 131
k <- 2
df_res <- n - k - 1 # 128

# (a) assigned lawn signs
# H₀: β₁ = 0 versus Hₐ: β₁ ≠ 0.

t_assigned <- 0.042 / 0.016
t_assigned # 2.625
2 * pt(abs(t_assigned), df = df_res, lower.tail = FALSE) # p 0.00972002
0.042 + c(-1, 1) * qt(0.975, df_res) * 0.016

# 95% CI: [0.01034127, 0.07365873] 
# excludes zero
# p < 0.05
# we reject H₀

# (b) adjacent lawn signs
t_adjacent <- 0.042 / 0.013 
t_adjacent # 3.230769
2 * pt(abs(t_adjacent), df = df_res, lower.tail = FALSE) # p 0.00156946
0.042 + c(-1, 1) * qt(0.975, df_res) * 0.013

# 95% CI: [0.01627728, 0.06772272]
# excludes zero 
# p < 0.05
# we reject H₀

# (c) constant term
# expected cuccinelli vote share is 30.2%
# if no assigned signs or adjacent to it 

0.302 / 0.011
# t 27.45455

# (d) model fit
# R^2 = 0.094
# two predictors explain 9.4% of the variation in vote share
