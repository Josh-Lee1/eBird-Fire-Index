library(ggplot2)
library(forcats)
library(dplyr)
view(drop)



drop %>%
  mutate(var = fct_reorder(var, percentage_drop)) %>%
  ggplot(aes(x=var, y=percentage_drop)) +
  geom_point() +
  ylab("Proportion of detection change after fire") +
  coord_flip() +
  xlab("") +
  theme_bw() + 
  geom_hline(yintercept = 0, linetype="dashed", 
                color = "grey", size=1)
