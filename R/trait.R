library(dplyr)
library(ggplot2)

traits<- read.csv("trait_data.csv")

fire_coefs<- rename(bird_response_df, species_name = species)

#foodtraits<- traits %>% 
  #select("X3_Taxon_common_name_2", "X99_Body_mass_average_8", "X163_Food_Fruit_10":"X173_Food_fish_or_invertebrates_Inland_waters_10", "X115_Feeding_habitat_Terrestrial_Arid_shrubland_9":"X145_Feeding_habitat_Urban_landscapes_9", "X193_National_movement_local_dispersal_13":"X197_National_movement_Irruptive_13") %>% 
  #rename(species = "X3_Taxon_common_name_2") %>% 
  #mutate(gsub(" ", "_", foodtraits$species)) %>% 
  #rename(species_name = `gsub(" ", "_", foodtraits$species)`)
  
#traitseds<- recode(foodtraits$species_name, 'Black-faced_Cuckoo-shrike' = "Black-faced_Cuckooshrike",
                  'European_Common_Blackbird' = "Eurasian_Blackbird",
                  'European_Common_Starling' = "European_Starling",
                  'Grey_Butcherbird' = "Gray_Butcherbird",
                  'Grey_Fantail' = "Gray_Fantail",
                  'Grey_Shrike-thrush' = "Gray_Shrikethrush",
                  'Australian_Wood_Duck' = "Maned_Duck",
                  'Red-browed_Finch' = "Red-browed_Firetail",
                  'Scarlet_Honeyeater' = "Scarlet_Myzomela",
                  'Superb_Fairy-wren' = "Superb_Fairywren",
                  'Variegated_Fairy-wren' = "Variegated_Fairywren",
                  'Willie_Wagtail' = "Willie-wagtail", 
                  'Jacky_Winter' = "Jacky-winter")


foodtraits<- traits %>% 
  dplyr::select("X3_Taxon_common_name_2", "X99_Body_mass_average_8", "X163_Food_Fruit_10":"X173_Food_fish_or_invertebrates_Inland_waters_10", "X115_Feeding_habitat_Terrestrial_Arid_shrubland_9":"X145_Feeding_habitat_Urban_landscapes_9", "X193_National_movement_local_dispersal_13":"X197_National_movement_Irruptive_13") %>% 
  rename(species_name = "X3_Taxon_common_name_2") %>% 
  mutate(species_name=gsub(" ", "_", species_name)) 


foodtraits$species_name<- recode(foodtraits$species_name, 'Black-faced_Cuckoo-shrike' = "Black-faced_Cuckooshrike",
                   'Common_Blackbird' = "Eurasian_Blackbird",
                   'Common_Starling' = "European_Starling",
                   'Grey_Butcherbird' = "Gray_Butcherbird",
                   'Grey_Fantail' = "Gray_Fantail",
                   'Grey_Shrike-thrush' = "Gray_Shrikethrush",
                   'Australian_Wood_Duck' = "Maned_Duck",
                   'Red-browed_Finch' = "Red-browed_Firetail",
                   'Scarlet_Honeyeater' = "Scarlet_Myzomela",
                   'Superb_Fairy-wren' = "Superb_Fairywren",
                   'Variegated_Fairy-wren' = "Variegated_Fairywren",
                   'Willie_Wagtail' = "Willie-wagtail", 
                   'Jacky_Winter' = "Jacky-winter")

traitsfin <- foodtraits %>% 
  left_join(fire_coefs, foodtraits, by = "species_name") %>% 
  filter(species_name %in% fire_coefs$species_name)
 



values<- traitsfin %>% 
  filter(Term == "before.afterBefore") %>% 
  mutate(X99_Body_mass_average_8 = as.numeric(X99_Body_mass_average_8)) %>%
  as.factor("X193_National_movement_local_dispersal_13",
               "X194_National_movement_Partial_migrant_13",
               "X195_National_movement_Total_migrant_13",
               "X196_National_movement_Nomadic_or_opportunistic_13",
               "X197_National_movement_Irruptive_13")

tot<- as.integer(values$X193_National_movement_local_dispersal_13,
          values$X194_National_movement_Partial_migrant_13,
          values$X195_National_movement_Total_migrant_13,
          values$X196_National_movement_Nomadic_or_opportunistic_13,
          values$X197_National_movement_Irruptive_13)

#check<- select(responses, species)


valuesfin<- values %>% 
  mutate(foodspec = X163_Food_Fruit_10 +                                                          
         X164_Food_Nectar_or_pollen_10 +                                                 
         X165_Food_Seeds_10 +                                                              
         X166_Food_Foliage_or_herbs_10 +                                                   
         X167_Food_Corms_or_tubers +                                                       
         X168_Food_Terrestrial_invertebrates_10 +                                          
         X169_Food_Terrestrial_vertebrates_10 +                                            
         X170_Food_Carrion_10 +                                                            
         X171_Food_Intertidal_invertebrates_10 +                                           
         X172_Food_Fish_or_invertebrates_Marine_10 +                                       
         X173_Food_fish_or_invertebrates_Inland_waters_10) %>% 
  mutate(habitatspec = X115_Feeding_habitat_Terrestrial_Arid_shrubland_9 +
           X116_Feeding_habitat_Terrestrial_Chenopod_shrubland_9 +
           X117_Feeding_habitat_Terrestrial_Heath_9 + 
           X118_Feeding_habitat_Terrestrial_Triodia_hummock_grassland_9 + 
           X119_Feeding_habitat_Terrestrial_Other_grassland_9 + 
           X120_Feeding_habitat_Terrestrial_Mallee_9 + 
           X121_Feeding_habitat_Terrestrial_Tropical_savanna_woodland_9 + 
           X122_Feeding_habitat_Terrestrial_Temperate_dry_sclerophyll_forest_and_woodland_9 +
X123_Feeding_habitat_Terrestrial_Temperate_wet_sclerophyll_forest_and_woodland_9 +
X124_Feeding_habitat_Terrestrial_Rainforest_9 +                                   
X125_Feeding_habitat_Terrestrial_Mangrove_trees_9 +                               
X126_Feeding_habitat_Inland_waters_Rivers_and_streams_9 +                         
X127_Feeding_habitat_Inland_waters_Deep_open_water_9 +                            
X128_Feeding_habitat_Inland_waters_Shallow_open_water_9 +                         
X129_Feeding_habitat_Inland_waters_Reeds_and_tall_wet_grassland_9 +               
X130_Feeding_habitat_Inland_waters_Low_marshland_and_wet_grassland_9 +            
X131_Feeding_habitat_Coastal_Sandy_9 +                                            
X132_Feeding_habitat_Coastal_Rocky_9 +                                            
X133_Feeding_habitat_Coastal_Soft_mud_9 +                                         
X134_Feeding_habitat_Coastal_Saltmarsh_9 +                                        
X135_Feeding_habitat_Coastal_Mangrove_floor_9 +                                   
X136_Feeding_habitat_Marine_Very_cold_pelagic_9 +                                 
X137_Feeding_habitat_Marine_Cold_pelagic_9 +                                      
X138_Feeding_habitat_Marine_Temperate_pelagic_9 +                                 
X139_Feeding_habitat_Marine_Warm_pelagic_9 +                                      
X140_Feeding_habitat_Marine_Cold_inshore_9 +                                      
X141_Feeding_habitat_Marine_Temperate_inshore_9 +                                 
X142_Feeding_habitat_Marine_Warm_inshore_9 +                                      
X143_Feeding_habitat_Other_non.Australian_habitat_9 +                             
X144_Feeding_habitat_Agricultural_landscapes_9 +
X145_Feeding_habitat_Urban_landscapes_9)%>% 
  mutate(movetot = X193_National_movement_local_dispersal_13 +
           X194_National_movement_Partial_migrant_13 +
           X195_National_movement_Total_migrant_13 +
           X196_National_movement_Nomadic_or_opportunistic_13 +
           X197_National_movement_Irruptive_13)

#### models
modfood<-lm(Estimate ~ foodspec, data = valuesfin,weights = 1/`Std. Error`)
modhabitat<-lm(Estimate ~ habitatspec, data = valuesfin,weights = 1/`Std. Error`)
modsize<-lm(Estimate ~ X99_Body_mass_average_8, data = valuesfin,weights = 1/`Std. Error`)
modmobility<-lm(Estimate ~ X193_National_movement_local_dispersal_13, data = valuesfin,weights = 1/`Std. Error`)

summary(modfood)
summary(modhabitat)
summary(modsize)
summary(modmobility)

food <- broom::glance(modfood)
habitat<- broom::glance(modhabitat)
size<- broom::glance(modsize)
sedentism<- broom::glance(modmobility)

outputs<- bind_rows(habitat, food, size, sedentism)
outputs$test <- c("Habitat Specialism", "Food Specialism", "Body Size", "Sedentism")

write.csv(outputs, "results.csv")

  #### Plotting models

df<- valuesfin %>% 
  mutate(Estimate, value = Estimate*-1)

ggplot(modfood, aes(foodspec, Estimate*-1)) + geom_point() + theme_bw() + xlab("Number of Feeding Guilds") + ylab("Fire Response")
ggsave("figures/foodmodel.PNG")
ggplot(modsize, aes(X99_Body_mass_average_8, Estimate*-1)) + geom_point()+ scale_x_log10()+ theme_bw() + xlab("Body Mass") + ylab("Fire Response")+ geom_smooth(method='lm')
ggsave("figures/sizemodel.PNG")
ggplot(modhabitat, aes(habitatspec, Estimate*-1)) + geom_point()+ theme_bw() + xlab("Number of Habitats") + ylab("Fire Response")
ggsave("figures/habitatmodel.PNG")
ggplot(modmobility, aes(X193_National_movement_local_dispersal_13, Estimate*-1)) + geom_point()+ theme_bw() + xlab("Sedentism") + ylab("Fire Response")
ggsave("figures/mobilitymodel.PNG")


look<- select(valuesfin, species_name, X99_Body_mass_average_8, foodspec, habitatspec, X193_National_movement_local_dispersal_13, Estimate)


######### making plots

boxplot(Estimate ~ X165_Food_Seeds_10, data = values)
boxplot(Estimate ~ X164_Food_Nectar_or_pollen_10, data = values)
boxplot(Estimate ~ X166_Food_Foliage_or_herbs_10, data = values)
boxplot(Estimate ~ X173_Food_fish_or_invertebrates_Inland_waters_10, data = values)
boxplot(Estimate ~ X163_Food_Fruit_10, data = values)
boxplot(Estimate ~ X167_Food_Corms_or_tubers, data = values)
boxplot(Estimate ~ X168_Food_Terrestrial_invertebrates_10, data = values)
boxplot(Estimate ~ X169_Food_Terrestrial_vertebrates_10, data = values)
boxplot(Estimate ~ X170_Food_Carrion_10, data = values)
boxplot(Estimate ~ X171_Food_Intertidal_invertebrates_10, data = values)
boxplot(Estimate ~ X172_Food_Fish_or_invertebrates_Marine_10, data = values)

boxplot(Estimate ~ X193_National_movement_local_dispersal_13, data = values)
boxplot(Estimate ~ X194_National_movement_Partial_migrant_13, data = values)
boxplot(Estimate ~ X195_National_movement_Total_migrant_13, data = values)
boxplot(Estimate ~ X196_National_movement_Nomadic_or_opportunistic_13, data = values)
boxplot(Estimate ~ X197_National_movement_Irruptive_13, data = values)




library(gghalves)

# default
ggplot(values, aes(x = as.factor(X193_National_movement_local_dispersal_13), y = Estimate, fill = X193_National_movement_local_dispersal_13)) +
  geom_half_boxplot(nudge = -0.1, outlier.color = NA) +
  geom_half_point() +
  theme_light() +
  guides(color = guide_legend(nrow = 1))+
  theme(legend.position = "none")

ggplot(values, aes(x = as.factor(X196_National_movement_Nomadic_or_opportunistic_13), y = Estimate, fill = X196_National_movement_Nomadic_or_opportunistic_13)) +
  geom_half_boxplot(nudge = -0.1, outlier.color = NA) +
  geom_half_point() +
  theme_light() +
  guides(color = guide_legend(nrow = 1))+
  theme(legend.position = "none")

ggplot(values, aes(x = as.factor(X168_Food_Terrestrial_invertebrates_10), y = Estimate, fill = X168_Food_Terrestrial_invertebrates_10)) +
  geom_half_boxplot(nudge = -0.1, outlier.color = NA) +
  geom_half_point() +
  theme_light() +
  guides(color = guide_legend(nrow = 1))+
  theme(legend.position = "none")

