library(tidyverse)
#Q1A:
cat("There are ", nrow(filter(starwars, starships != "character(0)")), " characters who have at least one starship.\n")
summarize(mutate(subset(starwars, subset = (starships != "character(0)")), ships = sapply(starships, toString)), name, ships, count = lengths(starships)) %>% arrange(desc(count))
#Q2A:
starwars %>% group_by(eye_color) %>% summarize(count = n()) %>% arrange(desc(count))
#Q3A:
starwars %>% group_by(species) %>% summarize(count = n(), avg = mean(birth_year, na.rm = TRUE)) %>% arrange(desc(avg)) %>% slice(1:3)
#Q4A:
starwars_2 <- mutate(starwars, height = as.numeric(height), mass = as.numeric(mass), birth_year = as.numeric(birth_year), films = sapply(films, toString), vehicles = sapply(vehicles, toString), starships = sapply(starships, toString))
starwars_2 <- rbind(starwars_2, list("Bu?ra Sar?kaya", 170, 60, "black", "fair", "brown", 26, "male", "masculine", "Dathomir", "Human", "Return of the Jedi", "Tsmeu-6 personal wheel bike", "Jedi Interceptor"))
filter(starwars_2, name == "Bu?ra Sar?kaya")
#Q5A:
starwars_2 <- mutate(starwars_2, BMI = mass / ((height / 100) * (height / 100)), weight_status = ifelse(BMI < 18.5, "underweight", ifelse(BMI >= 18.5 & BMI <= 24.99, "healthy", ifelse(BMI >= 25.0 & BMI <= 29.99, "overweight", "obese"))))
starwars_2 %>% slice(1:2)
#Q6A:
ggplot(filter(starwars_2, birth_year < 100), aes(x = weight_status, y = birth_year)) + geom_boxplot()
#Q7A:
ggplot(starwars_2, aes(x = birth_year, y = BMI)) + geom_point() + geom_smooth()
ggplot(filter(starwars_2, birth_year < 100 & BMI < 100), aes(x = birth_year, y = BMI)) + geom_point() + geom_smooth()