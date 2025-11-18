library(dplyr)
library(ggplot2)
library(haven)
library(tibble)
library(survey)
library(srvyr)
library(stringr)
library(tidyverse)

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

gss_fam$polviews


gss_refactored <- gss_fam |>
  mutate(polviews3= as_factor(polviews),
         polviews3 = case_when(
           str_detect(polviews3, "liberal")  ~ "Liberals",
           str_detect(polviews3, "moderate") ~ "Independents",
           str_detect(polviews3, "conservative")  ~ "Conservatives")|>
           factor()
  )


gss_refactored|>
  distinct(polviews3, as_factor(polviews))
8:17

#party ID
gss_refactored <- gss_refactored |>
  mutate(party3= as_factor(partyid),
         party3 = case_when(
           str_detect(party3, "democrat")  ~ "Democrats",
           str_detect(party3, "republican")  ~ "Republicans",
           str_detect(party3, "other|neither") ~ "Independents")|>
           factor()
  )



# Making Generations
gss_refactored<-gss_refactored|>
  mutate(gen_age = as_factor(age),
         gen_age = case_when(
           age %in% c(18:27) ~ "Gen Z",
           age %in% c(28:43) ~ "Millenial",
           age %in% c(44:59) ~ "Gen X",
           age %in% c(60:78) ~ "Boomer"
         )
  )


genz_polviews <- gss_refactored |>
  filter(gen_age == "Gen Z", !is.na(polviews3)) |>
  group_by(polviews3, year) |>
  summarise(count = n()) |>
  mutate(percent = count / sum(count) * 100)

genz_over_time <- gss_refactored |>
  filter(gen_age == "Gen Z", !is.na(polviews3)) |>
  group_by(year) |>
  summarise(total = n(),
            Liberal = (sum(polviews3 == "Liberals") / total) * 100,
            Moderate = (sum(polviews3 == "Independents") / total) * 100,
            Conservative = (sum(polviews3 == "Conservatives") / total) * 100
            )

genz_over_time_long <- genz_over_time |>
  pivot_longer(
    cols = c(Liberal, Moderate, Conservative),
    names_to = "polviews3",
    values_to = "percent"
  )

ggplot(genz_over_time_long, aes(x = year, y = percent, color = polviews3)) + geom_line(size = 1.2, alpha = 0.5) + scale_color_manual(
  values = c(
      "Liberal" = "blue",
      "Moderate" = "purple",
      "Conservative" = 'red'
    )
  )
 
