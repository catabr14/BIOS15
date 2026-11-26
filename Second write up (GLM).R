library(MASS)
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

#------------MAP plot-----------------------------------------------------------

MAP_seq <- seq(min(dat$MAP), max(dat$MAP), length.out = 100)

#Create prediction data
newdat <- data.frame(
  MAP = MAP_seq,
  altitude = mean(dat$altitude),
  MAT = mean(dat$MAT),
  Tseason = mean(dat$Tseason),
  Pseason = mean(dat$Pseason),
  forest. = mean(dat$forest.),
  lu_het = mean(dat$lu_het),
  method = dat$method[1],
  effort = mean(dat$effort)
)

pred <- predict(m_nb, newdata = newdat, type = "response")

#Plot using a logarithmic y-axis
plot(dat$MAP, dat$Eulaema_nigrita + 1,
     log = "y",
     xlab = "Mean annual precipitation (MAP)",
     ylab = "Bee abundance (log scale)",
     main = expression(italic("Eulaema nigrita") ~ "abundance in relation to MAP"),
     pch = 16)

#Add predicted relationship
lines(MAP_seq, pred + 1, lwd = 2)

#------------Forest cover plot--------------------------------------------------
#Create values of forest cover
forest_seq <- seq(min(dat$forest.), max(dat$forest.), length.out = 100)

#Create prediction data
newdat2 <- data.frame(
  forest. = forest_seq,
  altitude = mean(dat$altitude),
  MAT = mean(dat$MAT),
  MAP = mean(dat$MAP),
  Tseason = mean(dat$Tseason),
  Pseason = mean(dat$Pseason),
  lu_het = mean(dat$lu_het),
  method = dat$method[1],
  effort = mean(dat$effort)
)

pred2 <- predict(m_nb, newdata = newdat2, type = "response")

#Plot using a logarithmic y-axis
plot(dat$forest., dat$Eulaema_nigrita + 1,
     log = "y",
     xlab = "Forest cover",
     ylab = "Bee abundance (log scale)",
     main = expression(italic("Eulaema nigrita") ~ "abundance in relation to forest cover"),
     pch = 16)

#Add predicted relationship
lines(forest_seq, pred2 + 1, lwd = 2)