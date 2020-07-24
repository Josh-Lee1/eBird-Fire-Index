library(dplyr)

traits<- read.csv("trait_data.csv")

fire_coefs<- rename(bird_response_df, species_name = species)

foodtraits<- traits %>% 
  select("X3_Taxon_common_name_2", "X163_Food_Fruit_10":"X173_Food_fish_or_invertebrates_Inland_waters_10") %>% 
  rename(species = "X3_Taxon_common_name_2") %>% 
  mutate(gsub(" ", "_", foodtraits$species)) %>% 
  rename(species_name = `gsub(" ", "_", foodtraits$species)`) %>% 
  filter(species_name %in% fire_coefs$species) %>% 
  left_join(fire_coefs, foodtraits, by = "species_name")



##First pass use left_join(fire_coefs, trait_df) and make plots of the coefficients versus the trait values.
