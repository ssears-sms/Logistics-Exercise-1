states.data <-readRDS("~/Documents/Clark Atlanta University/Introduction to Data Science/linear_regression/dataSets/states.rds")
states.info <-data.frame(attributes(states.data)[c("names", "var.labels")])
tail(states.info, 8)
sts.ex.sat<-subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
plot(sts.ex.sat)
sat.mod <-lm(csat ~ expense, data=states.data)
summary(sat.mod)
# The association between expense and SAT scores appears to be negative
summary(lm(csat ~ expense + percent, data = states.data))
# Shows that expense is not significant
class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]
confint(sat.mod)
par(mar = c(4,4,2,2), mfrow = c(1,2))
plot(sat.mod, which = c(1,2))
sat.voting.mod <- lm(csat ~ expense + house + senate, data = na.omit(states.data))
sat.mod <-update(sat.mod, data = na.omit(states.data))
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

# Exercise 1: Least Squares Regression
eng.mod <-lm(energy ~ metro, data=states.data)
summary(eng.mod)
plot(eng.mod)

# Upon examining the plots, there seems to be a linear relationship between energy consumed per capita and the percentage of residents living in metro areas.

# Selecting one or more additional predictors to add to the model
summary(lm(energy ~ metro + income, data = states.data))

# The comparison of the model with an additional predictor does not return a significantly better model. R-squared values are relatively the same for both models. The additional predictor model yielded a lower adjusted R square value.

# Adding an interaction to a model

sat.expense.by.percent <- lm(csat ~ expense*income, data=states.data)
coef(summary(sat.expense.by.percent))

# Regression with categorical predictors

str(states.data$region)
states.data$region <- factor(states.data$region)

# Adding region to the model

sat.region <- lm(csat ~ region, data=states.data)

# Results
coef(summary(sat.region))
anova(sat.region)

contrasts(states.data$region)

coef(summary(lm(csat ~ C(region, base = 4), data=states.data)))

coef(summary(lm(csat ~ C(region, contr.helmert), data=states.data)))

# Exercise 2: Interactions and Factors

# Adding an interaction to the regression equation above
summary(lm(energy ~ metro + income*expense, data = states.data))

# Adding region to the model
eng.region <- lm(energy ~ region, data=states.data)

# Results

coef(summary(eng.region))

# There appears to be significant differences across the four regions
