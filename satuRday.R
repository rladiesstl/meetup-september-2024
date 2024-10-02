install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

empire <- starwars %>% 
  filter(row_number() %in% c(1:5, 10, 13, 14, 19, 21)) %>% 
  select(1:3, 8:11)

ggplot(data = empire, aes(x = mass, y = height)) +
  geom_point(aes(size = mass, color = species), alpha = .5) +
  labs(title = "Star Wars Characters", subtitle = "By size") +
  scale_size(guide = "none")



midus <- na.omit(midus)
test = midus %>% 
  filter(sex != "Male")

miduse1 = midus %>% 
  filter(BMI < mean(BMI) + 2*sd(BMI) & BMI > mean(BMI) - 2*sd(BMI)) %>% 
  select(ID, sex, age, physical_health_self, mental_health_self, self_esteem) %>% 
  mutate(zscore = (self_esteem - mean(self_esteem))/sd(self_esteem))


midus %>% 
  group_by(sex) %>% 
  summarize(mean(BMI))

longer_midus <- midus %>% 
  pivot_longer(cols = 10:11, names_to = "Person", values_to = "HeartIssues") %>% 
  separate(Person, sep = "_", into = c("heart", "person")) %>% 
  select(-heart)
