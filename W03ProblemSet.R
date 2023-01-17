# install.packages("pak", repos = "https://r-lib.github.io/p/pak/devel/")
used_packages = c("googlesheets4", "dplyr", "ggplot2", "epitools", "DescTools")
pak::pkg_install(used_packages)
sapply(used_packages, require, character.only = TRUE)

raw_data <- read_sheet("https://docs.google.com/spreadsheets/d/1xMJRrSQop81AV1IEelfiXIpmX2i9ze1D1ZiIqOPYYoA/edit?usp=sharing", sheet = 2)

processed_data = raw_data %>%
  mutate(Member = if_else(Type==1, TRUE, FALSE),
         has_date = as.numeric(!Date == "."),
         Date = gsub("\\.", -1, Date),
         Date = gsub("NK", -2, Date),
         Am_Pm = gsub("noon", 1, Am_Pm),
         missing = grepl("not", Date)) %>%
  mutate(has_date = replace(has_date, which(Date == "-2"), NA_character_),
         has_date = replace(has_date, which(missing), NA_character_),
         has_date = as.numeric(has_date),
         Date = gsub(-1, NA_integer_, Date),
         Date = gsub(-2, NA_integer_, Date),
         Date = as.numeric(Date)) 

## Is the attack rate higher among members?
with(processed_data,{
  t = table(Throat, Fever)
  print(t)
})

# Case definition, change however you prefer:
processed_data <- processed_data %>%
  mutate(num_symptoms = Throat + Fever + Headache,
         case = FALSE,
         case = replace(case, which(missing), NA),
         #case = replace(case, which(num_symptoms >= 2), TRUE)
         # case = replace(case, which(Throat == 1 | Fever == 1), TRUE)
         case = replace(case, which(Throat == 1 & Fever ==1), TRUE)
         )

## Is the attack rate higher among members?
with(processed_data,{
  t = table(Member, case)
  print(t)
  RelRisk(Rev(t))
})

## Filter members? Should we do this or not?
processed_data = filter(processed_data, Member)

cases = processed_data %>%
  filter(case) 

ggplot(cases, aes(Date-6)) + 
  geom_histogram(stat="count") + theme_classic()

ggplot(cases, aes(Date-6, group = Am_Pm, fill = Am_Pm)) + 
  geom_histogram(stat="count") + theme_classic()


foods = c("Eggsalad", "Macaroni", "Cottage", "Tunasalad", "Icecream", "Other")
df_attack = data.frame(food = foods, attack_ratio = 0)
for(i in seq_along(foods)){
  food = foods[i]
  df_attack[i,2] = with(processed_data,{
    t = table(processed_data[[food]], case)
    RelRisk(Rev(t))
  })
}
df_attack

with(processed_data,{
  t = table(Eggsalad, Tunasalad)
  print(t)
  print(RelRisk(Rev(t)))
})

with(processed_data,{
  t = table(Other, case, Eggsalad)
  print(t)
  for(i in 1:2)
    print(RelRisk(Rev(t[,,i])))
})

with(filter(processed_data, Tunasalad==1),{
  t = table(Eggsalad, case)
  print(t)
  print(RelRisk(Rev(t)))
})
