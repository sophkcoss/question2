library(dplyr)
library(ggplot2)
library(haven)
library(tibble)
library(survey)
library(srvyr)
library(stringr)

#Variables: POLVIEWS

cont_vars <- c("year", "id", "age")

cat_vars <- c("race", "sex", "polviews", "partyid", "abany", "immecon")

wt_vars <- c("vpsu",
             "vstrat",
             "oversamp",
             "formwt",              # weight to deal with experimental randomization
             "wtssps",              # weight variable
             "sampcode",            # sampling error code
             "sample")              # sampling frame and method

my_vars <- c(cont_vars, cat_vars, wt_vars)

gss_fam <- gss_all |>
  select(all_of(my_vars))

gss_fam

gss_2024 <- gss_fam|>
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
8:17
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

# Making Generations
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


genz_polviews <- gss_2024 |>
  filter(gen_age == "Gen Z", !is.na(polviews3)) |>
  group_by(polviews3) |>
  summarise(count = n()) |>
  mutate(percent = count / sum(count) * 100)

ggplot(genz_polviews, aes(x = polviews3, y = percent, fill = polviews3)) + geom_col()
