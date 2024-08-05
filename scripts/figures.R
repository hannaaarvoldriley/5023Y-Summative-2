# packages ----

library(ggplot2)
library(kableExtra)

# source script ----
source("scripts/data_tidying.R")
source("scripts/linear_model.R")

#colours ----

colours <- c("#440154","#482777","#3E4989","#31688E","#28628E","#1F9E89","#35D779","#6DCD59") #accessible colour scheme

# dot plot 1 ----


ggplot(data = butterflies_tidy, aes(x = factor(inbreed_coeff), y = flight_inhibit_idx)) +
  geom_jitter(aes(color = factor(inbreed_coeff)), width = 0.4, height = 0, alpha = 0.6, size = 2.5) +  #jitter plot
  scale_color_manual(values = c("0" = '#1F9E89', "0.25" = '#31688E', "0.375" = '#440154'),) +  #accessible colours
  labs(title = "Flight Inhibition Index Across Levels of Inbreeding Coefficient", #title
       x = "Inbreeding Coefficient Level", #axis labels
       y = "Flight Inhibition Index") +
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0, ceiling(max(butterflies_tidy$flight_inhibit_idx)), by = 5)) + #y axis line breaks
  theme( panel.grid.major.y = element_line(color = "grey", size = 0.2),  #appearance of line breaks
         panel.grid.minor = element_blank(), #remove minor lines
    legend.title = element_text(size = 12),  #make titles more balanced
    legend.text = element_text(size = 8),
    axis.title.y = element_text(size=14),
    axis.title.x = element_text(size=14))

# anova table 1----
anova(butterflies_lm8)

anova_table <- anova(butterflies_lm8) %>%
  as.data.frame() %>%
  `rownames<-`(c("Inbreeding Coefficient", "Thorax Dry Weight (mg)", "Residuals")) %>%
  kbl(caption = "figure legend", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped"))%>%
  kable_styling(bootstrap_options = c("bordered"))

print(anova_table)

save(anova_table, file = "figures/anova_table.RData")

# scatter plot 1 ---- 