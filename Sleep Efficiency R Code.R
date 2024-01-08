#Sleep_Efficiency.csv
data = read.csv("Sleep_Efficiency.csv",header=T)
head(data)
nrow(data)
sum(is.na(data))
data$Exercise.frequency = as.integer(data$Exercise.frequency)

# remove bedtime and wake up time
dat <- data[, -c(4, 5)]

# remove NA
dat <- na.omit(dat)

# convert to factor
dat$Gender = as.factor(dat$Gender)
dat$Smoking.status=as.factor(dat$Smoking.status)

# remove ID
dat <- dat[, -c(1)]
head(dat)
View(head(dat))

numdat = dat[,-c(2,11)]
cor.table=round(cor(numdat),3)
View(cor.table)
nrow(dat)

# initial model fitting
ini.model = lm(Sleep.efficiency~., data=dat)

residuals <- residuals(ini.model)

# Create a residual plot
par(mfrow = c(1, 2))
plot(predict(ini.model), residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual Plot")
#plot(ini.model$fitted.values,ini.model$residuals)

# create QQ normal plot
qqnorm(residuals)
qqline(residuals)

summary(ini.model)
View(summary(ini.model),5)
par(mfrow = c(1, 1))

# remove light sleep pct column
sleep = dat[, -c(7)]
head(sleep)

# histogram of response
hist(sleep$Sleep.efficiency,
     main = "Histogram of Sleep Efficiency",
     xlab = "Sleep Efficiency in proportion",
     ylab = "Frequency",
     col = "skyblue",
     border = "black"
)

# create ggpairs for continuous variables
par(mfrow = c(2, 4))

plot(x = sleep$Age, y = sleep$Sleep.efficiency,
     main = "Scatterplot of Age vs Sleep Efficiency",   
     xlab = "Age", ylab = "Sleep Efficiency",cex=0.8)

plot(x = sleep$Sleep.duration, y = sleep$Sleep.efficiency,
     main = "Scatterplot of Sleep Duration vs Sleep Efficiency",   
     xlab = "Sleep Duration", ylab = "Sleep Efficiency",cex=0.8)

plot(x = sleep$REM.sleep.percentage, y = sleep$Sleep.efficiency,
     main = "Scatterplot of REM Sleep Pct. vs Sleep Efficiency",   
     xlab = "REM Sleep Pct.", ylab = "Sleep Efficiency",cex=0.8)

plot(x = sleep$Deep.sleep.percentage, y = sleep$Sleep.efficiency,
     main = "Scatterplot of Deep Sleep Pct. vs Sleep Efficiency",   
     xlab = "Deep Sleep Pct.", ylab = "Sleep Efficiency",cex=0.8)

plot(x = sleep$Awakenings, y = sleep$Sleep.efficiency,
     main = "Scatterplot of Awakenings vs Sleep Efficiency",   
     xlab = "Awakenings", ylab = "Sleep Efficiency",cex=0.8)

plot(x = sleep$Caffeine.consumption, y = sleep$Sleep.efficiency,
     main = "Scatterplot of Caffeine comsumption vs Sleep Efficiency",   
     xlab = "Caffeine comsumption", ylab = "Sleep Efficiency",cex=0.8)

plot(x = sleep$Alcohol.consumption, y = sleep$Sleep.efficiency,
     main = "Scatterplot of Alcohol comsumption vs Sleep Efficiency",   
     xlab = "Alcohol comsumption", ylab = "Sleep Efficiency",cex=0.8)

plot(x = sleep$Exercise.frequency, y = sleep$Sleep.efficiency,
     main = "Scatterplot of Exercise frequency vs Sleep Efficiency",   
     xlab = "Exercise frequency", ylab = "Sleep Efficiency",cex=0.8)

par(mfrow = c(1, 1))

corr=cor(num.sleep)
View(round(corr,3))

# create boxplot for categorical variables
boxplot(Sleep.efficiency ~ Gender,
        data = sleep,
        main = "Boxplot of Sleep efficiency by Gender",
        xlab = "Gender",
        ylab = "Sleep efficiency",
        col = "skyblue",
        border = "black"
)

boxplot(Sleep.efficiency ~ Smoking.status,
        data = sleep,
        main = "Boxplot of Sleep efficiency by Smoking status",
        xlab = "Smoking status",
        ylab = "Sleep efficiency",
        col = "skyblue",
        border = "black"
)

# data summary for continuous variable
library(tidyr)
sleep_summary <-
  sleep %>%
  select(c(-Gender,-Smoking.status)) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>% 
  summarise(
    mean = mean(value,na.rm = T),
    sd = sd(value,na.rm = T),
    median = median(value, na.rm = T),
    variance = var(value, na.rm = T),
    max = max(value, na.rm = T),
    min = min(value, na.rm = T))
View(sleep_summary)

# data summary for categorical variable
gender=table(sleep$Gender)
genderprop=prop.table(gender)
View(round(rbind(gender,genderprop),3))

smoke=table(sleep$Smoking.status)
smokeprop=prop.table(smoke)
View(round(rbind(smoke,smokeprop),3))

# model selection
library(leaps)
s <- regsubsets(Sleep.efficiency ~ ., data=sleep, nvmax=10, method="forward")
ss=summary(s)

# cp, bic, r^2
forward_summary_df <- tibble(
  n_input_variables = 1:10,
  adj.r2=ss$adjr2,
  Cp = ss$cp,
  BIC = ss$bic)
forward_summary_df

forward_summary_df$Cp-(forward_summary_df$n_input_variables+1)

# cp plot
plot(forward_summary_df$n_input_variables+1,forward_summary_df$Cp,
      xlab="number of parameters",ylab="Cp value", type="b")
abline(a=2,b=1,col="red",lty=2)

inference_model <- lm(Sleep.efficiency~Age+REM.sleep.percentage+Deep.sleep.percentage+Awakenings+Caffeine.consumption+ Alcohol.consumption+ Smoking.status + Exercise.frequency, data=sleep)
summary(inference_model)

model_with_9var <- lm( Sleep.efficiency~Age+REM.sleep.percentage+Deep.sleep.percentage+Awakenings+Caffeine.consumption+ Alcohol.consumption+ Smoking.status + Exercise.frequency+Sleep.duration ,data=sleep)
summary(model_with_9var)

AIC(inference_model) #[1] -1063.893
AIC(model_with_9var) #[1] -1062.14

residuals2 <- residuals(inference_model)

# Create a residual plot
plot(predict(inference_model), residuals2,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual Plot")

# create QQ normal plot
qqnorm(residuals2)
qqline(residuals2)

