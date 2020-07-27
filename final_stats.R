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

  
  responses<- bird_response_df %>% 
    filter(Term == "before.afterBefore") %>% 
    rename(SE = 'Std. Error') %>% 
    mutate(Estimate, value = Estimate*-1)
    
  
         
  responses %>%
    mutate(species = fct_reorder(species, value)) %>%
    ggplot(aes(x=species, y=value)) +
    geom_point() +
    ylab("Change in detection after fire") +
    geom_errorbar(aes(ymin = value-SE, ymax = value+SE)) +
    coord_flip() +
    xlab("") +
    theme_bw() + 
    geom_hline(yintercept = 0, linetype="dashed", 
               color = "grey", size=1)
  