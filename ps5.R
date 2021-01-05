# R script EC3133 Problem set 5
# Author: Saad Khan
# Date: 09/11/2020

library(AER)
library(parameters)
library(readxl)

CPS96_15 <- read_excel("./Problem set 5/CPS96_15.xlsx")

CPS96 <- data.frame(CPS96_15[(CPS96_15$year == 1996),])

CPS15 <- data.frame(CPS96_15[(CPS96_15$year == 2015),])

data("CPSSW04")                    


### Question 1 

# creating variables with base year 2015

CPS96Base2015 <- (CPS96$ahe*(233.7/154.4))

CPSSW04Base2015 <- (CPSSW04$earnings*(233.7/185.2))

# mean

mean(CPS96Base2015)

mean(CPSSW04Base2015)

mean(CPS15$ahe)

# Standard deviation

sd(CPS96Base2015)
   
sd(CPSSW04Base2015)

sd(CPS15$ahe)

# T test for mean

t.test(CPS96Base2015)

t.test(CPSSW04Base2015)

t.test(CPS15$ahe)

# At the 5% level, all means are statistically significant 

### Question 2

# Variable for adjusted to 2015 values

CPSSW04$CPSSW04Base2015 <- CPSSW04Base2015

# Age squared

CPSSW04$agesquared <- CPSSW04$age^2

# log of earnings 2004, adjusted to 2015

CPSSW04$logearnings_base2015 <- log(CPSSW04$CPSSW04Base2015)

# interaction variable between female and bachelor

CPSSW04$female.x.bachelor <- (CPSSW04$gender == "female")*(CPSSW04$degree == "bachelor")

# interaction variable between age and female

CPSSW04$age.x.female <- (CPSSW04$gender == "female")*(CPSSW04$age)

# interaction variable between age and bachelor

CPSSW04$age.x.bachelor <- (CPSSW04$degree == "bachelor")*(CPSSW04$age)

# interaction variable between age squared and female

CPSSW04$agesquared.x.female <- (CPSSW04$gender == "female")*(CPSSW04$agesquared)

# interaction variable between age squared and bachelor

CPSSW04$agesquared.x.bachelor <- (CPSSW04$degree == "bachelor")*(CPSSW04$agesquared)

# Regression

lm1 <- lm(logearnings_base2015 ~
          age +
          agesquared +
          gender +
          degree +
          female.x.bachelor +
          age.x.female +
          agesquared.x.female +
          age.x.bachelor +
          agesquared.x.bachelor, 
          data= CPSSW04)

# Robust standard errors
parameters(lm1, robust = TRUE, vcov_type = "HC1")

library(sandwich)
cov <- vcovHC(lm1, type = "HC")
robust.se <- sqrt(diag(cov))

summary(lm1)

install.packages("stargazer")

library(stargazer)

# Report the estimated regression in a table (with robust standard errors)

stargazer(lm1, lm1,
          se= list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE,
          type = "html",
          dep.var.labels=c("Log of hourly earnings in 2004 adjusted to 2015 values"),
          covariate.labels = c("Age",
                               "Age Squared",
                               "Female",
                               "Bachelor",
                               "Female x Bachelor",
                               "Age x Female",
                               "Age Squared x Female",
                               "Age x Bachelor",
                               "Age Squared x Bachelor"),
          out = "lm1.html")

remove(cov)
remove(robust.se)

### Question 3

regfct1.agelogearnings.male.bachelor <- function(x){lm1$coefficients["(Intercept)"] +
                                                  lm1$coefficients["age"]*x +
                                                  lm1$coefficients["agesquared"]*x^2 +
                                                  lm1$coefficients["degreebachelor"] +
                                                  lm1$coefficients["age.x.bachelor"]*x +
                                                  lm1$coefficients["agesquared.x.bachelor"]*x^2}

regfct1.agelogearnings.male.highsch <- function(x){lm1$coefficients["(Intercept)"] +
                                                   lm1$coefficients["age"]*x +
                                                   lm1$coefficients["agesquared"]*x^2}

regfct1.agelogearnings.female.bachelor <- function(x){lm1$coefficients["(Intercept)"] +
                                                  lm1$coefficients["age"]*x +
                                                  lm1$coefficients["agesquared"]*x^2 +
                                                  lm1$coefficients["genderfemale"] +
                                                  lm1$coefficients["degreebachelor"] +
                                                  lm1$coefficients["female.x.bachelor"] +
                                                  lm1$coefficients["age.x.female"]*x +
                                                  lm1$coefficients["age.x.bachelor"]*x +
                                                  lm1$coefficients["agesquared.x.female"]*x^2 +
                                                  lm1$coefficients["agesquared.x.bachelor"]*x^2}

regfct1.agelogearnings.female.highsch <- function(x){lm1$coefficients["(Intercept)"] +
                                                  lm1$coefficients["age"]*x +
                                                  lm1$coefficients["agesquared"]*x^2 +
                                                  lm1$coefficients["genderfemale"] + 
                                                  lm1$coefficients["age.x.female"]*x +
                                                  lm1$coefficients["agesquared.x.female"]*x^2}

curve(regfct1.agelogearnings.male.bachelor,
      from = 25, to = 34, # Evaluate in x values from 25 to 34
      xlab = "Age", # x- axis label
      ylab = "log(Earnings for 2004 in 2015 dollars)", # y- axis label
      xlim = c(25, 34),
      ylim = c(2, 3.5),
      lwd = 3, # Set linewidth to 3
      lty = 2,
      col = "blue")

curve(regfct1.agelogearnings.male.highsch,
      from = 25, to = 34, 
      lwd = 3, 
      col = "blue", # Make the plotted line blue
      add = TRUE) # Add second curve to current plot

curve(regfct1.agelogearnings.female.bachelor,
      from = 25, to = 34,
      lwd = 3,
      lty = 2,
      col = "red", 
      add = TRUE) 

curve(regfct1.agelogearnings.female.highsch, 
      from = 25, to = 34,
      lwd = 3,
      col = "red",
      add = TRUE) 

legend("topleft", inset = 0.02, 
       legend=c("Men, high school", "Men, bachelor", "Women, high school", "Women, bachelor"),
       col=c("blue", "blue", "red", "red"), 
       lwd=c(3,3,3,3),
       lty=c(1,2,1,2))



### Question 4

CPS96$CPS96Base2015 <- CPS96Base2015

# Creating variables

CPS96$logEarnings_Base2015 <- log(CPS96$CPS96Base2015)
CPS96$agesquared <- CPS96$age^2
CPS96$female.x.bachelor <- CPS96$female*CPS96$bachelor
CPS96$age.x.female <- CPS96$age*CPS96$female
CPS96$age.x.bachelor <- CPS96$age*CPS96$bachelor
CPS96$agesquared.x.female <- (CPS96$age^2)*CPS96$female 
CPS96$agesquared.x.bachelor <- (CPS96$age^2)*CPS96$bachelor 

# Regression using CPS1996 (adjusted to 2015)

lm2 <- lm(logEarnings_Base2015 ~
            age +
            agesquared +
            female +
            bachelor +
            female.x.bachelor +
            age.x.female +
            agesquared.x.female +
            age.x.bachelor +
            agesquared.x.bachelor,
          data = CPS96)

summary(lm2)

# Robust standard errors
parameters(lm2, robust = TRUE, vcov_type = "HC1")

cov <- vcovHC(lm2, type = "HC")
robust.se <- sqrt(diag(cov))

# Report the estimated regression in a table (with robust standard errors)

stargazer(lm2, lm2,
          se= list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE,
          type = "html",
          dep.var.labels=c("Log of hourly earnings in 2004 adjusted to 2015 values"),
          covariate.labels = c("Age",
                               "Age Squared",
                               "Female",
                               "Bachelor",
                               "Female x Bachelor",
                               "Age x Female",
                               "Age Squared x Female",
                               "Age x Bachelor",
                               "Age Squared x Bachelor"),
          out = "lm2.html")

### Question 5

regfct2.agelogearnings.male.bachelor <- function(x){lm2$coefficients["(Intercept)"] +
    lm2$coefficients["age"]*x +
    lm2$coefficients["agesquared"]*x^2 +
    lm2$coefficients["bachelor"] +
    lm2$coefficients["age.x.bachelor"]*x +
    lm2$coefficients["agesquared.x.bachelor"]*x^2}

regfct2.agelogearnings.male.highsch <- function(x){lm2$coefficients["(Intercept)"] +
    lm2$coefficients["age"]*x +
    lm2$coefficients["agesquared"]*x^2}

regfct2.agelogearnings.female.bachelor <- function(x){lm2$coefficients["(Intercept)"] +
    lm2$coefficients["age"]*x +
    lm2$coefficients["agesquared"]*x^2 +
    lm2$coefficients["female"] +
    lm2$coefficients["bachelor"] +
    lm2$coefficients["female.x.bachelor"] +
    lm2$coefficients["age.x.female"]*x +
    lm2$coefficients["age.x.bachelor"]*x +
    lm2$coefficients["agesquared.x.female"]*x^2 +
    lm2$coefficients["agesquared.x.bachelor"]*x^2}

regfct2.agelogearnings.female.highsch <- function(x){lm2$coefficients["(Intercept)"] +
    lm2$coefficients["age"]*x +
    lm2$coefficients["agesquared"]*x^2 +
    lm2$coefficients["female"] + 
    lm2$coefficients["age.x.female"]*x +
    lm2$coefficients["agesquared.x.female"]*x^2}

curve(regfct2.agelogearnings.male.bachelor,
      from = 25, to = 34, # Evaluate in x values from 25 to 34
      xlab = "Age", # x- axis label
      ylab = "log(Earnings for 1996 in 2015 dollars)", # y- axis label
      xlim = c(25, 34),
      ylim = c(2, 3.5),
      lwd = 3, # Set linewidth to 3
      lty = 2,
      col = "blue")

curve(regfct2.agelogearnings.male.highsch,
      from = 25, to = 34, 
      lwd = 3, 
      col = "blue", # Make the plotted line blue
      add = TRUE) # Add second curve to current plot

curve(regfct2.agelogearnings.female.bachelor,
      from = 25, to = 34,
      lwd = 3,
      lty = 2,
      col = "red", 
      add = TRUE) 

curve(regfct2.agelogearnings.female.highsch, 
      from = 25, to = 34,
      lwd = 3,
      col = "red",
      add = TRUE) 

legend("topleft", inset = 0.02, 
       legend=c("Men, high school", "Men, bachelor", "Women, high school", "Women, bachelor"),
       col=c("blue", "blue", "red", "red"), 
       lwd=c(3,3,3,3),
       lty=c(1,2,1,2))



### Question 6

CPS15$logEarnings <- log(CPS15$ahe)
CPS15$agesquared <- CPS15$age^2
CPS15$female.x.bachelor <- CPS15$female*CPS15$bachelor
CPS15$age.x.female <- CPS15$age*CPS15$female
CPS15$age.x.bachelor <- CPS15$age*CPS15$bachelor
CPS15$agesquared.x.female <- (CPS15$age^2)*CPS15$female 
CPS15$agesquared.x.bachelor <- (CPS15$age^2)*CPS15$bachelor 

lm3 <- lm(logEarnings ~
            age +
            agesquared +
            female +
            bachelor +
            female.x.bachelor +
            age.x.female +
            agesquared.x.female +
            age.x.bachelor +
            agesquared.x.bachelor,
          data = CPS15)

summary(lm3)

# Robust standard errors
parameters(lm3, robust = TRUE, vcov_type = "HC1")

cov <- vcovHC(lm3, type = "HC")
robust.se <- sqrt(diag(cov))

# Report the estimated regression in a table (with robust standard errors)

stargazer(lm3, lm3,
          se= list(NULL, robust.se),
          column.labels=c("default","robust"), align=TRUE,
          type = "html",
          dep.var.labels=c("Log of hourly earnings in 2015"),
          covariate.labels = c("Age",
                               "Age Squared",
                               "Female",
                               "Bachelor",
                               "Female x Bachelor",
                               "Age x Female",
                               "Age Squared x Female",
                               "Age x Bachelor",
                               "Age Squared x Bachelor"),
          out = "lm3.html")

### Question 7

regfct3.agelogearnings.male.bachelor <- function(x){lm3$coefficients["(Intercept)"] +
    lm3$coefficients["age"]*x +
    lm3$coefficients["agesquared"]*x^2 +
    lm3$coefficients["bachelor"] +
    lm3$coefficients["age.x.bachelor"]*x +
    lm3$coefficients["agesquared.x.bachelor"]*x^2}

regfct3.agelogearnings.male.highsch <- function(x){lm3$coefficients["(Intercept)"] +
    lm3$coefficients["age"]*x +
    lm3$coefficients["agesquared"]*x^2}

regfct3.agelogearnings.female.bachelor <- function(x){lm3$coefficients["(Intercept)"] +
    lm3$coefficients["age"]*x +
    lm3$coefficients["agesquared"]*x^2 +
    lm3$coefficients["female"] +
    lm3$coefficients["bachelor"] +
    lm3$coefficients["female.x.bachelor"] +
    lm3$coefficients["age.x.female"]*x +
    lm3$coefficients["age.x.bachelor"]*x +
    lm3$coefficients["agesquared.x.female"]*x^2 +
    lm3$coefficients["agesquared.x.bachelor"]*x^2}

regfct3.agelogearnings.female.highsch <- function(x){lm3$coefficients["(Intercept)"] +
    lm3$coefficients["age"]*x +
    lm3$coefficients["agesquared"]*x^2 +
    lm3$coefficients["female"] + 
    lm3$coefficients["age.x.female"]*x +
    lm3$coefficients["agesquared.x.female"]*x^2}

curve(regfct3.agelogearnings.male.bachelor,
      from = 25, to = 34, # Evaluate in x values from 25 to 34
      xlab = "Age", # x- axis label
      ylab = "log(Earnings in 2015)", # y- axis label
      xlim = c(25, 34),
      ylim = c(2, 3.5),
      lwd = 3, # Set linewidth to 3
      lty = 2,
      col = "blue")

curve(regfct3.agelogearnings.male.highsch,
      from = 25, to = 34, 
      lwd = 3, 
      col = "blue", # Make the plotted line blue
      add = TRUE) # Add second curve to current plot

curve(regfct3.agelogearnings.female.bachelor,
      from = 25, to = 34,
      lwd = 3,
      lty = 2,
      col = "red", 
      add = TRUE) 

curve(regfct3.agelogearnings.female.highsch, 
      from = 25, to = 34,
      lwd = 3,
      col = "red",
      add = TRUE) 

legend("topleft", inset = 0.02, 
       legend=c("Men, high school", "Men, bachelor", "Women, high school", "Women, bachelor"),
       col=c("blue", "blue", "red", "red"), 
       lwd=c(3,3,3,3),
       lty=c(1,2,1,2))




### Question 8

# Displaying the graphs next to each other

par(mfrow=c(1,3))

# Graph 1

curve(regfct1.agelogearnings.male.bachelor,
      from = 25, to = 34, # Evaluate in x values from 25 to 34
      xlab = "Age", # x- axis label
      ylab = "log(Earnings for 2004 in 2015 dollars)", # y- axis label
      xlim = c(25, 34),
      ylim = c(2, 3.5),
      lwd = 3, # Set linewidth to 3
      lty = 2,
      col = "blue")

curve(regfct1.agelogearnings.male.highsch,
      from = 25, to = 34, 
      lwd = 3, 
      col = "blue", # Make the plotted line blue
      add = TRUE) # Add second curve to current plot

curve(regfct1.agelogearnings.female.bachelor,
      from = 25, to = 34,
      lwd = 3,
      lty = 2,
      col = "red", 
      add = TRUE) 

curve(regfct1.agelogearnings.female.highsch, 
      from = 25, to = 34,
      lwd = 3,
      col = "red",
      add = TRUE) 

legend("topleft", inset = 0.02, 
       legend=c("Men, high school", "Men, bachelor", "Women, high school", "Women, bachelor"),
       col=c("blue", "blue", "red", "red"), 
       lwd=c(3,3,3,3),
       lty=c(1,2,1,2))


# Graph 2

curve(regfct2.agelogearnings.male.bachelor,
      from = 25, to = 34, # Evaluate in x values from 25 to 34
      xlab = "Age", # x- axis label
      ylab = "log(Earnings for 1996 in 2015 dollars)", # y- axis label
      xlim = c(25, 34),
      ylim = c(2, 3.5),
      lwd = 3, # Set linewidth to 3
      lty = 2,
      col = "blue")

curve(regfct2.agelogearnings.male.highsch,
      from = 25, to = 34, 
      lwd = 3, 
      col = "blue", # Make the plotted line blue
      add = TRUE) # Add second curve to current plot

curve(regfct2.agelogearnings.female.bachelor,
      from = 25, to = 34,
      lwd = 3,
      lty = 2,
      col = "red", 
      add = TRUE) 

curve(regfct2.agelogearnings.female.highsch, 
      from = 25, to = 34,
      lwd = 3,
      col = "red",
      add = TRUE) 

legend("topleft", inset = 0.02, 
       legend=c("Men, high school", "Men, bachelor", "Women, high school", "Women, bachelor"),
       col=c("blue", "blue", "red", "red"), 
       lwd=c(3,3,3,3),
       lty=c(1,2,1,2))


# Graph 3

curve(regfct3.agelogearnings.male.bachelor,
      from = 25, to = 34, # Evaluate in x values from 25 to 34
      xlab = "Age", # x- axis label
      ylab = "log(Earnings in 2015)", # y- axis label
      xlim = c(25, 34),
      ylim = c(2, 3.5),
      lwd = 3, # Set linewidth to 3
      lty = 2,
      col = "blue")

curve(regfct3.agelogearnings.male.highsch,
      from = 25, to = 34, 
      lwd = 3, 
      col = "blue", # Make the plotted line blue
      add = TRUE) # Add second curve to current plot

curve(regfct3.agelogearnings.female.bachelor,
      from = 25, to = 34,
      lwd = 3,
      lty = 2,
      col = "red", 
      add = TRUE) 

curve(regfct3.agelogearnings.female.highsch, 
      from = 25, to = 34,
      lwd = 3,
      col = "red",
      add = TRUE) 

legend("topleft", inset = 0.02, 
       legend=c("Men, high school", "Men, bachelor", "Women, high school", "Women, bachelor"),
       col=c("blue", "blue", "red", "red"), 
       lwd=c(3,3,3,3),
       lty=c(1,2,1,2))
