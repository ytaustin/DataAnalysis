library(tidyverse)
glucose = read_csv("C:/Users/taoya/Desktop/MSCS6510/Lecture/glucose.csv")

ggsave("glucose.pdf")
write_csv(glucose, "glucose.csv")

