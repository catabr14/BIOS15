dat = read.csv("butterflies.csv")

#Calculate means
aggregate(DevelopmentTime ~ MaternalHost + LarvalHost,
          data = dat, FUN = mean)

#Calculate standard deviations
aggregate(DevelopmentTime ~ MaternalHost + LarvalHost,
          data = dat, FUN = sd)

#Number of observations in each group
table(dat$MaternalHost, dat$LarvalHost)

#----------------Box plot-------------------------------------------------------

#Combine maternal and larval host into one grouping variable
groups <- interaction(dat$MaternalHost, dat$LarvalHost,
                      sep = " × ", lex.order = TRUE, drop = TRUE)

#Create clearer labels
labels <- gsub(" × ", "\n× ", levels(groups), fixed = TRUE)

#Create boxplot
par(mar = c(6, 5, 4, 2))

boxplot(split(dat$DevelopmentTime, groups),
        names = labels,
        xlab = "Maternal host × Larval host",
        ylab = "Development time (days)",
        main = "Effect of host plant on butterfly development",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
        cex.axis = 0.85,
        las = 1)

#-----------Two-way ANOVA-------------------------------------------------------

#Fit the two-way ANOVA model, including the interaction
model <- aov(DevelopmentTime ~ MaternalHost * LarvalHost,
             data = dat)

#Display ANOVA results
summary(model)

interaction.plot(
  x.factor = dat$LarvalHost,
  trace.factor = dat$MaternalHost,
  response = dat$DevelopmentTime,
  fun = mean,
  type = "b",
  col = c("#0072B2", "#D55E00"),
  lwd = 2,
  pch = c(16, 17),
  xlab = "Larval host plant",
  ylab = "Mean development time (days)",
  trace.label = "Maternal host plant",
  main = "Effect of maternal and larval host plants",
  cex.main = 1.2,
  cex.lab = 1.1,
  cex.axis = 1
)