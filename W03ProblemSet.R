load_packages = c("googlesheets4", "plyr", "dplyr", "tidyr", "ggplot2",  "cowplot", "epitools", "DescTools")
pak::pkg_install(load_packages)

lapply(load_packages, require, character.only = TRUE)


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
         Date = as.numeric(Date),
         case = 0,
         case = replace(case, which(Throat == 1 | Vomit == 1), 1),
         case = as.logical(case)) 

with(processed_data,{
  t = table(Member, missing)
  print(t)
  RelRisk(Rev(t))
})

with(processed_data,{
  t = table(Member, case)
  print(t)
  RelRisk(Rev(t))
  #epitab(t, method="riskratio")
})

cases = processed_data %>%
  filter(case) 

ggplot(cases, aes(Date-6)) + 
  geom_histogram(stat="count") + theme_cowplot()

ggplot(cases, aes(Date-6, group = Am_Pm, fill = Am_Pm)) + geom_histogram(stat="count") + theme_cowplot()




foods = c("Eggsalad", "Macaroni", "Cottage", "Tunasalad", "Icecream", "Other")
df_attack = data.frame(foods)
for(i in seq_along(foods)){
  food = foods[i]
  df_attack[i,2] = with(processed_data,{
    t = table(processed_data[[food]], case)
    RelRisk(Rev(t))
  })
}
df_attack
