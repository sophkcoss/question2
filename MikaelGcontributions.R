#Packages
library(tidyverse)
library(srvyr)
library(gssr)
library(gssrdoc)
library(haven)
#Data and Weights
data(gss_all)
gss_2024 <- gss_get_yr(year='2024')
wt_vars <- c("vpsu",     # PSU
             "vstrat",   # Strata          
             "wtssps",   # Weights
             "wtssnrps"  #  Weights with non-response adjustment
)   

# removing user-missing values and converting the weights to numeric
gss_2024 <-gss_2024|>
  zap_missing()|>
  mutate(across(wt_vars, .fns=~as.numeric(.x)))

#age
summary(gss_all$age)
gss_2024 <- gss_2024|>
  mutate(gen_age = as_factor(age),
         gen_age = case_when(
           age %in% c(18:27) ~ "Gen Z",
           age %in% c(28:43) ~ "Millenial",
           age %in% c(44:59) ~ "Gen X",
           age %in% c(60:78) ~ "Boomer"
         ),
         gen_age = factor(gen_age, levels = c("Gen Z", "Millenial", "Gen X", "Boomer"))
  ) |>
  filter(!is.na(gen_age))
table(gss_2024$gen_age)

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
gss_2024|>
  distinct(party3, as_factor(partyid))

#immigration
attributes(gss_2024$immameco)
print_labels(gss_2024$immameco)

gss_2024 <- gss_2024 |>
  mutate(immgood= as_factor(immameco),
         immgood = case_when(
           str_detect(immgood, "neither")  ~ "Neither",
           str_detect(immgood, "disagree") ~ "Disagree",
           str_detect(immgood, "agree")  ~ "Agree")|>
           factor()
  )

gss_2024|>
  distinct(immgood, as_factor(immameco))

#abortion
print_labels(gss_2024$abany)


gss_2024 <- gss_2024 |> 
  mutate(abanyx= as_factor(abany), abanyx = case_when( 
    str_detect(abanyx, "yes") ~ "Yes",
    str_detect(abanyx, "\\bno\\b(?!\\s*answer)") ~ "No")|> 
      factor() 
  )
summary(gss_2024$abanyx)
gss_2024|>
  distinct(abanyx, as_factor(abany))

#age and sex?

####################ANALYSIS###########################

#svygss
svygss <- gss_2024|>
  as_survey_design(
    ids = vpsu,    # PSU ID
    strata = vstrat,  # Strata ID
    weight = wtssnrps,  # Weights
    nest=T)

#vizualization 1: generations and political views
propdata1 <- svygss|>
  filter(!is.na(gen_age), !is.na(polviews3))|>
  group_by(gen_age)|>
  summarise(
    prop = survey_mean(polviews3 == "Conservatives", vartype = "ci", na.rm = TRUE)
  )
ggplot(propdata1, aes(x = factor(gen_age), y = prop)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Generations",
    y = "Weighted proportion identifying as 'Conservative'",
    title = "Weighted estimates with 95% CI"
  ) +
  theme_minimal()

#visualization 2: generations and party identifaction
propdata2 <- svygss|>
  filter(!is.na(gen_age), !is.na(party3))|>
  group_by(gen_age)|>
  summarise(
    prop = survey_mean(party3 == "Republicans", vartype = "ci", na.rm = TRUE)
  )
ggplot(propdata2, aes(x = factor(gen_age), y = prop)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Generations",
    y = "Weighted proportion identifying as 'Republican'",
    title = "Weighted estimates with 95% CI"
  ) +
  theme_minimal()

#Nearly identical as polviews3, only perhaps noteworthy difference is that a greater proportion of
#Gen X is identifying as Republican, as compared to the proportion of Boomers identifying as Republican

#visualization 3: generations and views on immigrants
#those disagreeing with the statement that immigrants are good for America
propdata3 <- svygss|>
  filter(!is.na(gen_age), !is.na(immgood))|>
  group_by(gen_age)|>
  summarise(
    prop = survey_mean(immgood == "Disagree", vartype = "ci", na.rm = TRUE)
  )
ggplot(propdata3, aes(x = factor(gen_age), y = prop)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Generations",
    y = "Weighted proportion disagreeing with the statement",
    title = "Weighted estimates with 95% CI: Are Immigrants Good for America"
  ) +
  theme_minimal()

#It seems that Gen Z are the most conservative generation on this issue, however,
#confidence interval is very large, so the results are statistically insignificant

#visualization 4: generations and abortion
#Support for abortion if a woman wants it for any reason
propdata4 <- svygss|>
  filter(!is.na(gen_age), !is.na(abanyx))|>
  group_by(gen_age)|>
  summarise(
    prop = survey_mean(abanyx == "No", vartype = "ci", na.rm = TRUE)
  )
ggplot(propdata4, aes(x = factor(gen_age), y = prop)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Generations",
    y = "Weighted proportion against abortion for any reason",
    title = "Weighted estimates with 95% CI"
  ) +
  theme_minimal()

#Gen Z seems least conservative on this issue

#It seems that Gen Z are the most conservative generation on this issue, however,
#confidence interval is very large, so the results are statistically insignificant

#visualization 4: generations and abortion
#Support for abortion if a woman wants it for any reason
propdata4 <- svygss|>
  filter(!is.na(gen_age), !is.na(abanyx))|>
  group_by(gen_age)|>
  summarise(
    prop = survey_mean(abanyx == "No", vartype = "ci", na.rm = TRUE)
  )
ggplot(propdata4, aes(x = factor(gen_age), y = prop)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = prop_low, ymax = prop_upp), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Generations",
    y = "Weighted proportion against abortion for any reason",
    title = "Weighted estimates with 95% CI"
  ) +
  theme_minimal()

#Gen Z seems least conservative on this issue