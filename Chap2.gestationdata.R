library(magrittr)
library(GLMsData)

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
# Compute the model
################################################################################

mod_lm <- lm(Weight ~ Age, data = gestation)
summary(mod_lm)

mod_wgt <- lm(Weight ~ Age, data = gestation, weights = Births)
summary(mod_wgt)






