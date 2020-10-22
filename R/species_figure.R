library(ggplot2)
library(forcats)
library(dplyr)
view(drop)
view(bird_response_df)


drop %>%
  mutate(var = fct_reorder(var, percentage_drop)) %>%
  ggplot(aes(x=var, y=percentage_drop)) +
  geom_point() +
  ylab("Proportion of detection change after fire") +
  geom_errorbar()
  coord_flip() +
  xlab("") +
  theme_bw() + 
  geom_hline(yintercept = 0, linetype="dashed", 
                color = "grey", size=1)

  
  responses<- bird_respones_new %>% 
    filter(Term == "before.afterBefore") %>% 
    rename(SE = 'Std. Error') %>% 
    mutate(Estimate, value = Estimate*-1) %>% 
    mutate(Estimate, Eplusse = Estimate+SE) %>% 
    mutate(Estimate, Eminuse = Estimate-SE) 
    
response_new<- read.csv("Data/Processed/df.csv")
  
         
  Fireresponseplot<- response_new %>%
    mutate(species = fct_reorder(species, value)) %>%
    ggplot(aes(x=species, y=value, colour = response)) +
    geom_point() +
    ylab("Modelled Response to Fire") +
    geom_errorbar(aes(ymin = value-SE, ymax = value+SE)) +
    coord_flip() +
    xlab("") +
    theme_bw() + 
    geom_hline(yintercept = 0, linetype="dashed", 
               color = "grey", size=1) +
    theme(legend.position = c(0.8, 0.2)) +
    theme(legend.title = element_blank()) +
    scale_color_brewer(palette = "Set1")

  plot(Fireresponseplot)
  
  
  ggsave(Fireresponseplot, "Output/figures/fireresponses.png")