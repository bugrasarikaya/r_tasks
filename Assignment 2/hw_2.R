#Part 1:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data_1 <- read.csv("covid-data-2020.txt", sep = "\t")
set.seed(2016556506)
samples_1 <- data_1[sample(nrow(data_1), 1000), ]
#P1Q1A:
library(tidyverse)
samples_1 %>% group_by(location, month) %>% summarize(min = min(new_cases, na.rm = TRUE), Q1 = quantile(new_cases, 0.25, na.rm = TRUE), Q2 = quantile(new_cases, 0.5, na.rm = TRUE), Q3 = quantile(new_cases, 0.75, na.rm = TRUE), max = max(new_cases, na.rm = TRUE))
#P1Q2A:
samples_1 %>% group_by(location) %>% summarize(max_case = max(new_cases, na.rm = TRUE), max_death = max(new_deaths, na.rm = TRUE))
#P1Q3A:
samples_1 %>% group_by(location, month) %>% summarize("Mean of dailycases" = mean(new_cases, na.rm = TRUE))
#P1Q4A:
ggplot(filter(samples_1, location == c("Turkey", "Russia", "United Kingdom")), aes(x = new_cases, y = month, color = location)) + geom_boxplot()
#Part 2:
data_2 <- stringr::sentences
#P2Q1A:
set.seed(2016556506)
samples_2 <- data_2[sample(length(data_2), 100)]
new_data <- unique(matrix(str_split(samples_2, " ", simplify = TRUE)))
#P2Q2A:
str_subset(new_data, "^a.*e$")
#P2Q3A:
sum(str_count(new_data, "[aeiou]") > 3)
#P2Q4A:
tail(new_data[order(nchar(new_data))], 5)
#P2Q5A:
str_subset(new_data, "age|any|day|exp|her|pro|the")