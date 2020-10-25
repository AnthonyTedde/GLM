library(magrittr)
library(GLMsData)

################################################################################
#
# Weighted univariate regression model
#
################################################################################


data("gestation")

dplyr::glimpse(gestation)

################################################################################
# Plot the data
################################################################################

ggplot2::ggplot(gestation, ggplot2::aes(x = Age, y = Weight)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(formula = y ~ x, method = "lm")
# -> To be correct when we apply linear regression we should take into
# consideration the number of rows used to contruct the consolidated measures.
#
# Possible model then:
#
# var(y_i) = \sigma^2 / m_i     -> as random component
# \mu_i = \beta_0 + \beta_1 xi  -> as systematic component

##
# Weighted lm
###

ggplot2::ggplot(gestation, ggplot2::aes(x = Age, y = Weight)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(formula = y ~ x, method = "lm", color = "red") +
  ggplot2::geom_smooth(mapping = ggplot2::aes(weight = Births),
                       formula = y ~ x, method = "lm")


################################################################################
# Compute the models using R
################################################################################

mod_lm <- lm(Weight ~ Age, data = gestation)
summary(mod_lm)

mod_wgt <- lm(Weight ~ Age, data = gestation, weights = Births)
summary(mod_wgt)


################################################################################
# Compute the models using formulae
################################################################################
w <- gestation$Births
y <- gestation$Weight
x <- gestation$Age

x_bar <- weighted.mean(x, w)
y_bar <- weighted.mean(y, w)

SSxy <- sum(w * (x - x_bar) * y)
SSx <- sum(w * (x - x_bar)^2)

beta_hat_1 <- SSxy / SSx
beta_hat_0 <- y_bar - beta_hat_1 * x_bar
# RSS = Residual sum of square -> Optimum of the function S achieved.
RSS <- sum(w * (y - beta_hat_0 - beta_hat_1 * x)^2)

(y - predict(mod_wgt)) **2



# mean(y) = beta_hat_0 + beta_hat_1 * mean(u)
beta_hat_0 + beta_hat_1 * x_bar == y_bar


################################################################################
# Variance of the model
################################################################################

# n - 2 : residual degree of freedom
s2 <- RSS / (length(x) - 2)
s <- sqrt(s2) #  Residual standard Error


################################################################################
# Standard Errors of the Coefficients
################################################################################

beta_hat_0_se = s * sqrt(sum(w) ** -1 + x_bar **2 / SSx)
beta_hat_1_se = s / sqrt(SSx)


################################################################################
# Standard Errors of Fitted Values
################################################################################

mu_i_se <- sqrt(s2 * (sum(w) **-1 + (x - x_bar) **2 / SSx))


















