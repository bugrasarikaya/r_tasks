library(tidyverse)
view(starwars)
#Q1A:
nrow(filter(starwars, starships != "character(0)"))
subset(starwars, name, subset = (starships != "character(0)"))
#Q2A:
starwars %>% group_by(hair_color) %>% summarize(count = n()) %>% arrange(desc(count))
#Q3A:
starwars %>% group_by(species) %>% summarize(avg = mean(birth_year, na.rm = TRUE)) %>% arrange(avg)
(starwars %>% group_by(species) %>% summarize(avg = mean(birth_year, na.rm = TRUE)) %>% arrange(desc(avg)) %>% slice(1:3))$species
#Q4A:
starwars_2 <- rbind(starwars, c("Buðra Sarýkaya", 170, 60, "black", "fair", "brown", 26, "male", "masculine", "Dathomir", "Human", "Return of the Jedi", "Tsmeu-6 personal wheel bike", "Jedi Interceptor"))
#Q5A:
starwars_2 <- mutate(starwars_2, BMI = as.numeric(mass) / ((as.numeric(height) / 100) * (as.numeric(height) / 100)), weight_status = ifelse(BMI < 18.5, "underweight", ifelse(BMI >= 18.5 & BMI <= 24.99, "healthy", ifelse(BMI >= 25.0 & BMI <= 29.99, "overweight", "obese"))))
#Q6A:
ggplot(filter(starwars_2, as.numeric(birth_year) < 100)) + geom_point(aes(x = factor(weight_status, level <- c("underweight", "healthy", "overweight", "obese", NA)), y = birth_year))
#Q7A:
ggplot(starwars_2[!is.na(starwars_2$BMI),], aes(x = as.numeric(birth_year), y = as.numeric(BMI))) + geom_point() + geom_line()
ggplot(filter(starwars_2[!is.na(starwars_2$BMI),], as.numeric(birth_year) < 100 & as.numeric(BMI) < 100), aes(x = as.numeric(birth_year), y = as.numeric(BMI))) + geom_point() + geom_line()