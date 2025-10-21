library(gssr)
library(haven)
library(labelled)
library(dplyr)
library(tidyverse)
library(srvyr)
library(survey)
data(gss_all)



cont_vars <- c("year", "id", "age")
cat_vars <- c("race", "sex",
              "immameco", "abany")

my_vars <- c(cont_vars, cat_vars)
gss_fam <- gss_all|>
  select(all_of(my_vars))



gss_2024 <- gss_all|>
  filter(year == 2024)
print_labels(gss_2024$polviews)
attributes(gss_2024$polviews)
gss_2024 <- gss_2024 |>
  mutate(polviews3= as_factor(polviews),
         polviews3 = case_when(
           str_detect(polviews3, "liberal")  ~ "Liberals",
           str_detect(polviews3, "moderate") ~ "Independents",
           str_detect(polviews3, "conservative")  ~ "Conservatives")|>
           factor()
  )
summary(gss_2024$polviews3)
gss_2024|>
  distinct(polviews3, as_factor(polviews))


gss_2024 <- gss_2024 |>
  mutate(party3= as_factor(partyid),
         party3 = case_when(
           str_detect(party3, "democrat")  ~ "Democrats",
           str_detect(party3, "republican")  ~ "Republicans",
           str_detect(party3, "other|neither") ~ "Independents")|>
           factor()
  )
summary(gss_2024$party3)

gss_2024 <- gss_2024|>
  mutate(gen_age = as_factor(age),
         gen_age = case_when(
           age %in% c(18:27) ~ "Gen Z",
           age %in% c(28:43) ~ "Millenial",
           age %in% c(44:59) ~ "Gen X",
           age %in% c(60:78) ~ "Boomer"
         )
  )|>
  filter(!is.na(gen_age))

gss_2024 <- gss_2024 |>
  mutate(immgood= as_factor(immameco),
         immgood = case_when(
           str_detect(immgood, "agree")  ~ "Agree",
           str_detect(immgood, "neither") ~ "Neither",
           str_detect(immgood, "disagree")  ~ "Disagree")|>
           factor()
  )

gss_2024 <- gss_2024|>
  mutate(sex1 = as_factor(sex),
         sex1 = case_when(
           str_detect(sex1, "1") ~ "Male",
           str_detect(sex1, "2") ~ "Female"
         )
  )|>
  filter(!is.na(sex1))

  


  
