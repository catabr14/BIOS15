library(MASS)
library(ggplot2)
library(patchwork)

dat = read.csv("Eulaema.csv")

#Check distribution of bee abundance
hist(dat$Eulaema_nigrita,
     main = expression(""),
     xlab = "Number of bees",
     ylab = "Frequency",
     xlim = c(0, max(dat$Eulaema_nigrita) * 1.1))

#Fit Poisson GLM and check overdispersion
m <- glm(
  Eulaema_nigrita ~ altitude + MAT + MAP +
    Tseason + Pseason + forest. + lu_het +
    method + offset(effort),
  family = poisson,
  data = dat
)

summary(m)
deviance(m) / df.residual(m)

#Fit negative binomial GLM and check overdispersion
m_nb <- glm.nb(
  Eulaema_nigrita ~ altitude + MAT + MAP +
    Tseason + Pseason + forest. + lu_het +
    method + offset(effort),
  data = dat
)

summary(m_nb)
deviance(m_nb) / df.residual(m_nb)

#--------------Results table----------------------------------------------------
#Extract negative binomial model results
results <- summary(m_nb)$coefficients

#Create results table
results_table <- data.frame(
  Variable = rownames(results),
  Estimate = results[, "Estimate"],
  SE = results[, "Std. Error"],
  z = results[, "z value"],
  p = results[, "Pr(>|z|)"],
  Rate_ratio = exp(results[, "Estimate"])
)

results_table[, -1] <- signif(results_table[, -1], 3)
results_table

#--------Plots------------------------------------------------------------------
#A: Forest cover
p1 <- ggplot(dat, aes(x = forest., y = Eulaema_nigrita)) +
  geom_point(color = "grey40", alpha = 0.6) +
  geom_smooth(method = "glm.nb", formula = y ~ x, color = "black") +
  theme_bw() +
  coord_cartesian(xlim = c(0.044, 1), ylim = c(0, 700)) +
  labs(x = "Forest cover", y = expression(italic("Eulaema nigrita") ~ " abundance"))

#B: MAP
p2 <- ggplot(dat, aes(x = MAP, y = Eulaema_nigrita)) +
  geom_point(color = "grey40", alpha = 0.6) +
  geom_smooth(method = "glm.nb", formula = y ~ x, color = "black") +
  theme_bw() +
  coord_cartesian(xlim = c(735, NA), ylim = c(0, 700)) +
  labs(x = "MAP", y = expression(italic("Eulaema nigrita") ~ " abundance"))

#Combine plots
p1 + p2 + plot_annotation(tag_levels = "A")

