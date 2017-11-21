library(tidyverse)
glucose = read_csv("C:/Users/taoya/Desktop/MSCS6510/Lecture/glucose.csv")

write_csv(glucose, "glucose.csv")

glucose = mutate(glucose, blood_glucose = glucose$'Blood Glucose')


glucose = select(glucose, Timestamp, blood_glucose, `Prior Activity`)
library(lubridate)
glucose <- mutate(glucose, timestamp = ymd_hm(glucose$Timestamp))
glucose <- select(glucose, timestamp, blood_glucose, `Prior Activity`)
glucose <- mutate(glucose, imputed_prior_activity = ifelse(is.na(`Prior Activity`), "none", `Prior Activity`))
glucose <- mutate(glucose, prior_activity = as.factor(glucose$imputed_prior_activity))
glucose <- select(glucose, timestamp, blood_glucose, prior_activity)
head(glucose)
ggplot(data = glucose) + geom_smooth(mapping = aes(x = timestamp, y = blood_glucose))
ggsave("glucose_smooth.jpg")
ggplot(data = glucose) + geom_path(mapping = aes(x = timestamp, y = blood_glucose))
ggsave("glucose_path.jpg")
ggplot(data = glucose, mapping = aes(x = prior_activity, y = blood_glucose)) + geom_boxplot()
ggsave("glucose_boxplot.jpg")
model_1 <- lm(blood_glucose ~ prior_activity + timestamp, data=glucose)
summary(model_1)
predicted_glucose <- mutate(glucose, predicted = predict(model_1, glucose))
ggplot(data = predicted_glucose) + geom_path(mapping = aes(x = timestamp, y = blood_glucose)) +
  geom_path(mapping = aes(x = timestamp, y = predicted, color="red"))
ggsave("model1.jpg")
ggplot(data = predicted_glucose) + geom_point(mapping = aes(x = blood_glucose, y = predicted))
ggplot(data = predicted_glucose) + geom_point(mapping = aes(x = blood_glucose, y = predicted)) +
  geom_path(x = seq(from = 0, to = 400, length = nrow(predicted_glucose)), y = seq(from = 0, to =400, length = nrow(predicted_glucose)))
ggsave("model1_glucose_45.jpg")
glucose <- mutate(glucose, day_of_week = as.factor(wday(glucose$timestamp)))
ggplot(data = glucose, mapping = aes(x = day_of_week, y = blood_glucose)) + geom_boxplot()
ggsave("glucose_boxplot_day.jpg")


model_2 <- lm(blood_glucose ~ prior_activity + timestamp + day_of_week, data=glucose)
summary(model_2)
library(lmtest)
lrtest(model_2, model_1)
glucose <- mutate(glucose, hour_of_day = as.factor(hour(glucose$timestamp)))
head(glucose)
ggplot(data = glucose, mapping = aes(x = hour_of_day, y = blood_glucose)) + geom_boxplot()
glucose <- mutate(glucose, period_of_day = cut_interval(hour(glucose$timestamp), length=4))
head(glucose)
ggplot(data = glucose, mapping = aes(x = period_of_day, y = blood_glucose)) + geom_boxplot()
model_3 <- lm(blood_glucose ~ prior_activity + timestamp + period_of_day, data=glucose)
summary(model_3)
lrtest(model_3, model_1)
predicted_glucose <- mutate(glucose, predicted = predict(model_3, glucose))
ggplot(data = predicted_glucose) + geom_path(mapping = aes(x = timestamp, y = blood_glucose)) +
  geom_path(mapping = aes(x = timestamp, y = predicted, color="red"))
ggsave("model3.jpg")
ggplot(data = predicted_glucose) + geom_point(mapping = aes(x = blood_glucose, y = predicted)) +
  geom_path(x = seq(from = 0, to = 400, length = nrow(predicted_glucose)), y = seq(from = 0, to =400, length = nrow(predicted_glucose)))
ggsave("model3_glucose_45.jpg")

glucose <- mutate(glucose, week_of_year = as.factor(week(glucose$timestamp)))
head(glucose)
ggplot(data = glucose, mapping = aes(x = week_of_year, y = blood_glucose)) + geom_boxplot()
ggsave("glucose_boxplot_week.jpg")

model_4 <- lm(blood_glucose ~ prior_activity + timestamp + period_of_day + week_of_year, data=glucose)
summary(model_4)
lrtest(model_4, model_3)

predicted_glucose <- mutate(glucose, predicted = predict(model_4, glucose))
ggplot(data = predicted_glucose) + geom_path(mapping = aes(x = timestamp, y = blood_glucose)) +
  geom_path(mapping = aes(x = timestamp, y = predicted, color="red"))
ggsave("model4.jpg")
ggplot(data = predicted_glucose) + geom_point(mapping = aes(x = blood_glucose, y = predicted)) +
  geom_path(x = seq(from = 0, to = 400, length = nrow(predicted_glucose)), y = seq(from = 0, to =400, length = nrow(predicted_glucose)))
ggsave("model4_glucose_45.jpg")

save.image("glucose")
