library(tidyverse)
#Part 1:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data_1 <- read.csv("covid-data-2020.txt", sep = "\t")
set.seed(2016556506)
samples_1 <- data_1[sample(nrow(data_1), 1000), ]
samples_1 %>% slice(1:10) #Implemented "slice(1:10)" and "head(1:10)" functions and "[1:10]" operation, show first 10 element for simplifying output.
#P1Q1A:
head(transmute(group_by(samples_1, location, month), min = min(new_cases, na.rm = TRUE), Q1 = quantile(new_cases, 0.25, na.rm = TRUE), Q2 = quantile(new_cases, 0.5, na.rm = TRUE), Q3 = quantile(new_cases, 0.75, na.rm = TRUE), max = max(new_cases, na.rm = TRUE)), 10)
#P1Q2A:
arrange(slice(summarize(group_by(samples_1, location), max_case = max(new_cases, na.rm = TRUE), max_death = max(new_deaths, na.rm = TRUE)), 1:10), desc(max_case))
#P1Q3A:
head(arrange(slice(summarize(group_by(samples_1, location, month), "Mean of dailycases" = mean(new_cases)), which.max(.data[["Mean of dailycases"]])), desc(.data[["Mean of dailycases"]])), 10)
#P1Q4A:
ggplot(filter(samples_1, location == c("Poland", "Germany", "Italy")), aes(x = month, y = new_cases, color = location)) + geom_boxplot()
#Part 2:
data_2 <- stringr::sentences
#P2Q1A:
set.seed(2016556506)
samples_2 <- data_2[sample(length(data_2), 100)]
new_data <- unique(matrix(str_split(samples_2, boundary("word"), simplify = TRUE)))
new_data <- new_data[new_data != ""]
new_data[1:10]
#P2Q2A:
cat("Condition of starting with \"a\" and ending with \"e\", is satisfied with these words:", intersect(str_subset(new_data, "^a"), str_subset(new_data, "e$")),"\n")
#P2Q3A:
cat("Number of words which have 3 or more vowels is:", sum(str_count(new_data, "[aeiou]") > 3))
#P2Q4A:
slice(arrange(summarize(as.data.frame(new_data), wrods = new_data, count = nchar(new_data)), desc(count)), 1:5)
#P2Q5A:
words <- c("age", "any" ," day" ,"exp", "her", "pro" ,"the")
cat(str_c(words, collapse = ", "),"words were found in these words: ", str_subset(new_data, str_c(words, collapse = "|")), "\n")