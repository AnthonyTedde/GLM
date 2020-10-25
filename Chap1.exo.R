library(GLMsData)


################################################################################
# Exercice 1.1
################################################################################

data("paper")

dplyr::glimpse(paper)


ggplot2::ggplot(data = paper,
          mapping = ggplot2::aes(x = Hardwood, y = Strength)) +
  ggplot2::geom_point() +
  # ggplot2::geom_smooth(se = F) +
  ggplot2::geom_smooth(method = "lm", se = F) +
  # ggplot2::geom_smooth(method = "gam", se = F, color = "green") +
  # ggplot2::geom_smooth(formula = y ~ x**3, se = F, color = "red") +
  ggplot2::geom_smooth(formula = y ~ poly(x, degree = 2),
                       method = "lm",
                       se = F,
                       color = "orange") +
  ggplot2::geom_smooth(formula = y ~ poly(x, degree = 3),
                       method = "lm",
                       se = F,
                       color = "red") +
  ggplot2::geom_smooth(formula = y ~ poly(x, degree = 4),
                       method = "lm",
                       se = F,
                       color = "green")

####
# Solution
##

# Solution: Select the cubic systematic component because, more partimonious
# than the quartic one and seems to provide same usefull information.


################################################################################
# Exercice 1.2
################################################################################

data("heatcap")

dplyr::glimpse(heatcap)

ggplot2::ggplot(data = heatcap,
                mapping = ggplot2::aes(x = Temp, y = Cp)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = F) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)
                       , se = F, color = "green") +
  ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3)
                       , se = F, color = "orange") +
  ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, degree = 4)
                       , se = F, color = "red")


####
# Solution
##

# Cubic for the same reason as before. Solution should be balanced between
# parcimony and accuracy.


################################################################################
# Exercice 1.4
################################################################################

data("toxo")

dplyr::glimpse(toxo)

ggplot2::ggplot(data = toxo,
                mapping = ggplot2::aes(x = Rainfall, y = Proportion)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3),
                       se = F) +
  ggplot2::geom_smooth(method = "glm", formula = y ~ poly(x, degree = 3),
                       se = F)

####
# Solution
##

# Cubic for the same reason as before. Solution should be balanced between
# parcimony and accuracy.
