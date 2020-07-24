library(dplyr)
library(ggplot2)

traits<- read.csv("trait_data.csv")

fire_coefs<- rename(bird_response_df, species_name = species)

foodtraits<- traits %>% 
  select("X3_Taxon_common_name_2", "X163_Food_Fruit_10":"X173_Food_fish_or_invertebrates_Inland_waters_10") %>% 
  rename(species = "X3_Taxon_common_name_2") %>% 
  mutate(gsub(" ", "_", foodtraits$species)) %>% 
  rename(species_name = `gsub(" ", "_", foodtraits$species)`) %>% 
  filter(species_name %in% fire_coefs$species) %>% 
  left_join(fire_coefs, foodtraits, by = "species_name")



values<- filter(foodtraits, Term == "before.afterBefore")

boxplot(Estimate ~ X165_Food_Seeds_10, data = values)
boxplot(Estimate ~ X164_Food_Nectar_or_pollen_10, data = values)
boxplot(Estimate ~ X166_Food_Foliage_or_herbs_10, data = values)
boxplot(Estimate ~ X173_Food_fish_or_invertebrates_Inland_waters_10, data = values)
boxplot(Estimate ~ X163_Food_Fruit_10, data = values)
boxplot(Estimate ~ X167_Food_Corms_or_tubers, data = values)
boxplot(Estimate ~ X169_Food_Terrestrial_vertebrates_10, data = values)
boxplot(Estimate ~ X170_Food_Carrion_10, data = values)
boxplot(Estimate ~ X171_Food_Intertidal_invertebrates_10, data = values)
boxplot(Estimate ~ X172_Food_Fish_or_invertebrates_Marine_10, data = values)







boxplot(Estimate ~ X165_Food_Seeds_10, data = foodtraits)



##First pass use left_join(fire_coefs, trait_df) and make plots of the coefficients versus the trait values.
