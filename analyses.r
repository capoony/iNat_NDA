## load library tidyverse
library(tidyverse)
library(viridis)

## set working directory
setwd("D:/GitHub/iNat_NDA/")

## load observations-576350.csv
obs <- read.csv("data/observations-576350.csv")

## create results directory
if (!dir.exists("results")) {
    dir.create("results")
}

## list column names
colnames(obs)

## convert date column to date format
obs$date <- as.Date(obs$time_observed_at, format = "%Y-%m-%d")

## extract year from date
obs$year <- format(obs$date, "%Y")

## rename "iconic_taxon_name" to "taxon_group"
obs$taxon_group <- obs$iconic_taxon_name

## plot observations by year and exclude "NA" values and
obs.nona <- obs %>%
    filter(!is.na(year))

## split by "taxon_group" and plot as stacked barplot with viridis color palette
obs.nona %>%
    group_by(year, taxon_group) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = year, y = n, fill = taxon_group)) +
    geom_col() +
    labs(title = "Observations by Year and Taxon Group", x = "Year", y = "Number of Observations") +
    scale_fill_viridis_d() +
    theme_bw()

## save plot as png
ggsave("results/observations_by_year.png", width = 8, height = 6)

## now repeat the same process for month
obs$month <- format(obs$date, "%m")
obs.nona <- obs %>%
    filter(!is.na(month))

## plot observations and split by "taxon_group" and plot as stacked barplot with viridis color palette
obs.nona %>%
    group_by(month, taxon_group) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = month, y = n, fill = taxon_group)) +
    geom_col() +
    labs(title = "Observations by Month and Taxon Group", x = "Month", y = "Number of Observations") +
    scale_fill_viridis_d() +
    theme_bw()

## save plot as png
ggsave("results/observations_by_month.png", width = 8, height = 6)

## now repeat the same process for day
obs$day <- format(obs$date, "%d")
obs.nona <- obs %>%
    filter(!is.na(day))

## plot observations by weekday and split by "taxon_group" and plot as stacked barplot with viridis color palette
obs.nona %>%
    group_by(day, taxon_group) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = day, y = n, fill = taxon_group)) +
    geom_col() +
    labs(title = "Observations by Day and Taxon Group", x = "Day", y = "Number of Observations") +
    scale_fill_viridis_d() +
    theme_bw()

## now repeat the same process for weekday
obs$weekday <- weekdays(obs$date)

obs.nona <- obs %>%
    filter(!is.na(weekday))

## convert weekday to factor with levels in order and starting with Monday
obs.nona$weekday <- factor(obs.nona$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

## plot observations by weekday and split by "taxon_group" and plot as stacked barplot with viridis color palette
obs.nona %>%
    group_by(weekday, taxon_group) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = weekday, y = n, fill = taxon_group)) +
    geom_col() +
    labs(title = "Observations by Weekday and Taxon Group", x = "Weekday", y = "Number of Observations") +
    scale_fill_viridis_d() +
    theme_bw()

## save plot as png
ggsave("results/observations_by_weekday.png", width = 8, height = 6)

## now repeat the same process for hour
obs$hour <- format(obs$date, "%H")

## now plot most common "common_name" and restrict to 20 most common, color by "taxon_group"
obs.nona <- obs %>%
    filter(!is.na(common_name) & !common_name == "") %>%
    group_by(common_name, taxon_group) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

head(obs.nona, 20) %>%
    ggplot(aes(x = reorder(common_name, n), y = n, fill = taxon_group)) +
    geom_col() +
    coord_flip() +
    labs(title = "Most Common Taxa", x = "Common Name", y = "Number of Observations") +
    scale_fill_viridis_d() +
    theme_bw()

## save plot as png
ggsave("results/most_common_taxa.png", width = 8, height = 6)

## now repeat the same process but show the top 20 for each "taxon_group"
obs.nona <- obs %>%
    filter(!is.na(common_name) & !common_name == "") %>%
    group_by(common_name, taxon_group) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

## now plot most common "common_name" and restrict to 20 most common, color by "taxon_group" and facet by "taxon_group"
obs.nona %>%
    group_by(taxon_group) %>%
    slice_max(n, n = 20) %>%
    ggplot(aes(x = reorder(common_name, n), y = n, fill = taxon_group)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~taxon_group, scales = "free_y") +
    labs(title = "Most Common Taxa by Taxon Group", x = "Common Name", y = "Number of Observations") +
    scale_fill_viridis_d() +
    theme_bw()

## save plot as png
ggsave("results/most_common_taxa_by_taxon_group.png", width = 20, height = 12)

## now do the same for "scientific_name" and restrict to 20 most common, color by "taxon_group" and facet by "taxon_group"
obs.nona <- obs %>%
    filter(!is.na(scientific_name) & !scientific_name == "") %>%
    group_by(scientific_name, taxon_group) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

## now plot most common "scientific_name" and restrict to 20 most common, color by "taxon_group" and facet by "taxon_group"
obs.nona %>%
    group_by(taxon_group) %>%
    slice_max(n, n = 20) %>%
    ggplot(aes(x = reorder(scientific_name, n), y = n, fill = taxon_group)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~taxon_group, scales = "free_y") +
    labs(title = "Most Common Taxa by Taxon Group", x = "Scientific Name", y = "Number of Observations") +
    scale_fill_viridis_d() +
    theme_bw()

## save plot as png
ggsave("results/most_common_taxa_by_taxon_group_scientific_name.png", width = 20, height = 12)



## now summarise the results in a README.md file
## create a README.md file and rename it to README.md
readme <- file("README.md", "w")
writeLines("# Analysis of iNat_project [\"Die anderen Museumsbesucher\"](https://www.inaturalist.org/projects/die-anderen-museumsbesucher-the-other-museum-visitors?tab=observations)\n", readme)
writeLines("## Observations by Year and Taxon Group\n", readme)
writeLines("![Observations by Year and Taxon Group](results/observations_by_year.png)\n", readme)
writeLines("## Observations by Month and Taxon Group\n", readme)
writeLines("![Observations by Month and Taxon Group](results/observations_by_month.png)\n", readme)
writeLines("## Observations by Day and Taxon Group\n", readme)
writeLines("![Observations by Day and Taxon Group](results/observations_by_day.png)\n", readme)
writeLines("## Observations by Weekday and Taxon Group\n", readme)
writeLines("![Observations by Weekday and Taxon Group](results/observations_by_weekday.png)\n", readme)
writeLines("## Most Common Taxa\n", readme)
writeLines("![Most Common Taxa](results/most_common_taxa.png)\n", readme)
writeLines("## Most Common Taxa by Taxon Group\n", readme)
writeLines("![Most Common Taxa by Taxon Group](results/most_common_taxa_by_taxon_group.png)\n", readme)
writeLines("## Most Common Taxa by Taxon Group (Scientific Name)\n", readme)
writeLines("![Most Common Taxa by Taxon Group (Scientific Name)](results/most_common_taxa_by_taxon_group_scientific_name.png)\n", readme)

## close the file
close(readme)
