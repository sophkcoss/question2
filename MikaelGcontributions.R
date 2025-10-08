library(tidyverse)
library(srvyr)
library(gssr)
library(gssrdoc)
library(haven)

#age
summary(gss_all$age)
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

#political veiws
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

#party ID
gss_2024 <- gss_2024 |>
  mutate(party3= as_factor(partyid),
         party3 = case_when(
           str_detect(party3, "democrat")  ~ "Democrats",
           str_detect(party3, "republican")  ~ "Republicans",
           str_detect(party3, "other|neither") ~ "Independents")|>
           factor()
  )
summary(gss_2024$party3)

#immigration
attributes(gss_2024$immameco)
print_labels(gss_2024$immameco)
gss_2024 <- gss_2024 |>
  mutate(immgood= as_factor(immameco),
         immgood = case_when(
           str_detect(immgood, "agree")  ~ "Agree",
           str_detect(immgood, "neither") ~ "Neither",
           str_detect(immgood, "disagree")  ~ "Disagree")|>
           factor()
  )
gss_2024|>
  distinct(immgood, as_factor(immameco))

#abortion
print_labels(gss_2024$abany)

#age and sex


#prepping data for analysis
cont_vars <- c("year", "id", "ballot", "age")
cat_vars <- c("race", "sex", "fefam")
wt_vars <- c("vpsu",
             "vstrat",
             "oversamp",
             "formwt",              # weight to deal with experimental randomization
             "wtssps",              # weight variable
             "sampcode",            # sampling error code
             "sample")              # sampling frame and method

my_vars <- c(cont_vars, cat_vars, wt_vars)