pacman::p_load(tidyverse, skimr, glue, jialir, crosstable)

# load data (not included)

##------------------------------------------------------------------------------
# participant characteristics
##------------------------------------------------------------------------------
p1 <- mealx %>% select(vid, gender, ethnicity, glycemic, smoking, alcohol, edu) %>% distinct() %>% select(-vid) %>% 
  crosstable(percent_digits = 0) %>% mutate(bind = paste0(label, "_", variable)) %>% 
  select(bind, value) %>% pivot_wider(names_from = bind, values_from = value)
p2 <- mealx %>% select(vid, age, bmi, gluf, hba1c, sbp, dbp, ldl, hdl, chol, tg) %>% group_by(vid) %>% 
  summarise_all(mean) %>% ungroup() %>% select(-vid) %>% 
  summarise_all(~est_sd(mean(.x, na.rm = T), sd(.x, na.rm = T), digit = 1)) %>% ungroup()


##------------------------------------------------------------------------------
# meal characteristics
##------------------------------------------------------------------------------
#---- overall
at1 <- mealx %>% summarise(n = glue("{n()} ({round(n()/dim(mealx)[1]*100, 0)}%)")) %>% ungroup()
  # lifestyle vars
at2a <- mealx %>% 
  summarise_at(vars(meal_itemA_refineGrain:meal_itemG_dairy, meal_itemx_soyBeanNut, 
                    meal_itemJ_vege:meal_itemM_sweets, meal_drinkSugar, meal_drinkNoSugar), 
               ~glue("{sum(.x == 1)} ({round(mean(.x)*100)}%)"))
at2b <- mealx %>% summarise_at(vars(p5_pre6.24hLPA_hr, p5_pre6.24hMVPA_hr, p5_pre6hLPA_hr, p5_pre6hMVPA_hr, p5_post2hLPA_hr, p5_post2hMVPA_hr, pre24hSleep_hr),
                               ~est_ci(quantile(.x, .5, na.rm = T), lci = quantile(.x, .25, na.rm = T), uci = quantile(.x, .75, na.rm = T), sep = ", ", digit = 2)) %>% ungroup()
  # CGM vars
at3a <- mealx %>% summarise_at(vars(baseGlu30m, iAUC_h, peak2h, rise2h, risePctBaseGlu, valley23h, dip23h, dipPctBaseGlu),
                               ~est_ci(quantile(.x, .5, na.rm = T), lci = quantile(.x, .25, na.rm = T), uci = quantile(.x, .75, na.rm = T), sep = ", ", digit = )) %>% ungroup()
at3b <- mealx %>% summarise_at(vars(dipYes), ~glue("{sum(.x == 1)} ({round(mean(.x)*100)}%)"))

  # appetite vars and the the N data available 
at4a <- mealx %>% summarise_at(vars(nextMeal_hrLater_min, nextMealOrSnack_hrLater_min, meal_fullness, 
                    hungry_pre, hungry_post23h, hungry_diff23h, hungry_post34h, hungry_diff34h, nextMeal_fullness),
               ~est_ci(quantile(.x, .5, na.rm = T), lci = quantile(.x, .25, na.rm = T), uci = quantile(.x, .75, na.rm = T), sep = ", ", digit = 0)) %>% ungroup()
at4c <- mealx  %>% 
  select(nextMeal_hrLater_min, nextMealOrSnack_hrLater_min, meal_fullness, 
         hungry_pre, hungry_post23h, hungry_diff23h, hungry_post34h, hungry_diff34h, nextMeal_fullness) %>% 
  summarise_all(~sum(!is.na(.x))) %>% rename_all(~paste0("Ndata_", .x))



#---- Summary by mealtime
t1 <- mealx %>% group_by(meal_timeBand3) %>% summarise(n = glue("{n()} ({round(n()/dim(mealx)[1]*100, 0)}%)")) %>% ungroup()
  # lifestyle vars
t2a <- mealx %>% group_by(meal_timeBand3) %>% 
  summarise_at(vars(meal_itemA_refineGrain:meal_itemG_dairy, meal_itemx_soyBeanNut, 
                    meal_itemJ_vege:meal_itemM_sweets, meal_drinkSugar, meal_drinkNoSugar), 
               ~glue("{sum(.x == 1)} ({round(mean(.x)*100)}%)"))
t2b <- mealx %>% group_by(meal_timeBand3) %>% 
  summarise_at(vars(p5_pre6.24hLPA_hr, p5_pre6.24hMVPA_hr, p5_pre6hLPA_hr, p5_pre6hMVPA_hr, p5_post2hLPA_hr, p5_post2hMVPA_hr, pre24hSleep_hr),
               ~est_ci(quantile(.x, .5, na.rm = T), lci = quantile(.x, .25, na.rm = T), uci = quantile(.x, .75, na.rm = T), sep = ", ", digit = 2)) %>% ungroup()
  # CGM vars
t3a <- mealx %>% group_by(meal_timeBand3) %>% 
  summarise_at(vars(baseGlu30m, iAUC_h, peak2h, rise2h, risePctBaseGlu, valley23h, dip23h, dipPctBaseGlu),
               ~est_ci(quantile(.x, .5, na.rm = T), lci = quantile(.x, .25, na.rm = T), uci = quantile(.x, .75, na.rm = T), sep = ", ", digit = )) %>% ungroup()
t3b <- mealx %>% group_by(meal_timeBand3) %>% summarise_at(vars(dipYes), ~glue("{sum(.x == 1)} ({round(mean(.x)*100)}%)"))

  # appetite vars and the the N data available 
t4a <- mealx %>% group_by(meal_timeBand3) %>% 
  summarise_at(vars(nextMeal_hrLater_min, nextMealOrSnack_hrLater_min, meal_fullness, 
                    hungry_pre, hungry_post23h, hungry_diff23h, hungry_post34h, hungry_diff34h, nextMeal_fullness),
               ~est_ci(quantile(.x, .5, na.rm = T), lci = quantile(.x, .25, na.rm = T), uci = quantile(.x, .75, na.rm = T), sep = ", ", digit = 0)) %>% ungroup()
t4c <- mealx  %>% group_by(meal_timeBand3) %>% 
  select(nextMeal_hrLater_min, nextMealOrSnack_hrLater_min, meal_fullness, 
         hungry_pre, hungry_post23h, hungry_diff23h, hungry_post34h, hungry_diff34h, nextMeal_fullness) %>% summarise_all(~sum(!is.na(.x))) %>% 
  rename_at(vars(!meal_timeBand3), ~paste0("Ndata_", .x))



