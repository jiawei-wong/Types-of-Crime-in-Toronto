### Preamble ###
# Purpose: Clean and Set-up the Victims of Crime data downloaded from Toronto Open Data
# Author: Julia Wong
# Date: February 2 2022
# Contact: jiawei.wong@mail.utoronto.ca
# Pre-requisites: None

### Workspace Set-up ###
#install.packages("opendatatoronto")
#install.packages("knitr")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("kableExtra")
#install.packages("bibtex")
#install.packages("bookdown")

library(opendatatoronto)
library(knitr)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(kableExtra)
library(bibtex)
library(bookdown)

### Getting the dataset from Toronto Open Data ###
package <- show_package("9cd09dd7-4453-43bd-af7a-caf42e565103")
package
resources <- list_package_resources("9cd09dd7-4453-43bd-af7a-caf42e565103")
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data

### Saving the dataset ###
write_csv(data, "inputs/data/raw_data.csv")
raw_data <- read.csv("inputs/data/raw_data.csv")

### Grabbing specific columns ###
cleaned_victims_of_crime <- raw_data |>
  as_tibble() |>
  select(ReportedYear, Subtype, AssaultSubtype, Sex, Count_)

### Creating a table with the total victims of crime from 2014 - 2020 ###
total_reported_crime <- cleaned_victims_of_crime |>
  select(ReportedYear, Count_) |>
  group_by(ReportedYear) |>
  summarise_all(sum) |>
  pivot_wider(names_from = ReportedYear, values_from = Count_)

total_reported_crime |>
  knitr::kable(caption = "Total Number of Crime Reported (2014 - 2020)",
               align = c('l', 'l', 'l', 'l', 'l', 'l', 'l'),
               booktabs = T) |>
  kable_classic(full_width = T) |>
  kable_styling(latex_options = "HOLD_position")

### Creating a graph of the victims of crime by category (2014 - 2020) ###
victims_of_crime_by_type <- cleaned_victims_of_crime |>
  select(ReportedYear, Subtype, Count_) |>
  arrange(ReportedYear) |>
  group_by(ReportedYear, Subtype) |>
  summarise_all(sum) |>
  mutate(ReportedYear = as.numeric(ReportedYear))

ggplot(victims_of_crime_by_type, aes(ReportedYear, Count_, fill = Subtype), las=2) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=Count_), size = 3, position = position_stack(vjust = 0.5)) +
  theme_light() +
  labs(x = "Year", y = "Number of Reports", fill = "Type of Crime", title = "Reported Types of Crime in Toronto (2014 - 2020)") +
  scale_x_continuous(breaks = victims_of_crime_by_type$ReportedYear) +
  theme_light()

### Creating a table with assault subcategories ###
assault_subcategories <- cleaned_victims_of_crime |>
  select(ReportedYear, Subtype, AssaultSubtype, Count_) |>
  filter(Subtype == "Assault") |>
  melt(id = c("ReportedYear", "AssaultSubtype",
              variable.name = "Subtype",
              value.name = "Count_")) |>
  group_by(ReportedYear, AssaultSubtype) |>
  summarise(Count_ = sum(Count_)) |>
  pivot_wider(names_from = ReportedYear, values_from = Count_)

assault_subcategories_total <-
  rbind(assault_subcategories, c("Total", colSums(assault_subcategories[,2:8])))

knitr::kable(assault_subcategories_total, caption = "Number of Assault Victims by Subcategory (2014 - 2020)", 
             col.names = c("Subcategory", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
             booktabs = TRUE) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  row_spec(dim(assault_subcategories_total)[1], bold = T) |>
  kable_classic() |>
  kable_styling(latex_options = "HOLD_position")

### Creating a graph of the victims of crime by gender (2014 - 2020) ###
victims_of_crime_by_gender <- cleaned_victims_of_crime |>
  select(ReportedYear, Subtype, Sex, Count_) |>
  filter(Subtype == "Sexual Violation") |>
  arrange(ReportedYear) |>
  subset(Sex!="U") |>
  mutate(Sex = recode(Sex, 'F' = "Female",
                      'M' = 'Male')) |>
  group_by(ReportedYear, Subtype, Sex) |>
  summarise_all(sum)

victims_of_crime_by_gender |>
  group_by(ReportedYear) |>
  ggplot(aes(x = ReportedYear, y = Count_, color = Sex)) +
  geom_line(aes(group = Sex), lwd = 1.5) +
  scale_x_continuous(breaks = victims_of_crime_by_gender$ReportedYear) +
  labs(color = "Gender of Victim",
       x = "Year",
       y = "Number of Reports",
       title = "Victims of Sexual Violation by Gender (2014 - 2020)") +
  theme_light()