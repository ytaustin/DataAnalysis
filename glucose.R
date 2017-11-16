library(tidyverse)
glucose = read_csv("C:/Users/taoya/Desktop/MSCS6510/Lecture/glucose.csv")

write_csv(glucose, "glucose.csv")

glucose = mutate(glucose, blood_glucose = glucose$'Blood Glucose')


glucose = select(glucose, Timestamp, blood_glucose, `Prior Activity`)
