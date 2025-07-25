pacman::p_load(tidyverse, skimr, glue, jialir, geepack, ggpubr, viridisLite, patchwork, cowplot)

# load data (not included)

#-------------------------------------------------------------------------------
# plot glucose dip pattern
#-------------------------------------------------------------------------------

# Figure 2a
p1_meal_hist <- mealx %>% ggplot(aes(x = dipPctBaseGlu, fill = factor(dipYes))) +
  geom_histogram(binwidth = 2) + 
  coord_cartesian(xlim = c(-50, 50), ylim = c(0, 420)) + 
  scale_x_continuous(breaks = seq(-40, 40, by = 20)) +
  scale_y_continuous(breaks = seq(0, 400, by = 200)) +
  labs(x = "Glucose dip magnitude (% baseline)", y = "Meal counts") + 
  scale_fill_manual(values = c("#1F9E89AF", "#0018877F"))+
  annotate("text", 
           x = 12.8, y = 330, 
           label = "54% with glucose \ndips below baseline", 
           color = "#001887EF",
           lineheight = 1,size = 5.5, hjust = 0) +  
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.8, color = "grey20") + 
  theme_cowplot() + 
  theme(legend.position = "none", 
        plot.margin = margin(0.5,0,0.5,0, "cm"),
        axis.title.x = element_text(vjust = 0.1))
p1_meal_hist


# Figure 2b
dperson <- mealx %>% group_by(vid) %>% summarise(rate = mean(dipYes)) %>% ungroup() %>%
  mutate(gp = cut(rate, breaks = seq(0, 1, 0.2), include.lowest = T,
                  labels = c("0-20%", "20-40%", "40-60%","60-80%", "80-100%"))) %>%
  count(gp) %>% mutate(percent = n / sum(n) * 100, label = paste0(n, " (", round(percent, 0), "%)"))

p2_person <- dperson %>% ggplot(aes(x = gp, y = n, fill = gp %>% as.numeric())) + 
  geom_bar(stat = "identity") +
  labs(x = "Within-person proportion of meals with glucose dips below baseline", y = "Participant counts") + 
  scale_fill_gradient(low = "#0018873F", high = "#001887AF") + 
  # scale_fill_gradient(low = "#9F2F7F4F", high = "#9F2F7FCF") + 
  geom_text(aes(label = label), vjust = -0.5) +
  coord_cartesian(ylim = c(0, 310)) +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        theme(plot.margin = margin(0.5,0,0.5,0, "cm")),
        axis.title.x = element_text(vjust = 0.1))
p2_person



# sFigure 2
dtp <- mealx %>% group_by(vid) %>% summarise(rate = mean(dipYes)*100) %>% ungroup() %>% 
  arrange(rate) %>% mutate(person = row_number())
dtp %>% ggplot(aes(x = person, y = rate)) +
  geom_hline(yintercept = 100, color = "grey70", linetype = "longdash", size = 0.25) +
  geom_bar(stat = "identity", fill = "#0018873F", color = "#0018873F") +
  labs(x = "Participants sorted by within-person proportion of meals\nwith glucose dips below baseline (n = 935)", y = "Within-person proportion (%)") + 
  coord_cartesian(xlim = c(1, 935)) + 
  scale_x_continuous(breaks = c(1, 250, 500, 750, 935), expand = c(0, 0),
                     labels = c("1th", "250th", "500th", "750th", "935th")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_cowplot() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(0.5,1,0.5,0.5, "cm"),
        axis.title.x = element_text(vjust = 0.1))

