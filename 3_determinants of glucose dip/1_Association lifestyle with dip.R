pacman::p_load(tidyverse, skimr, glue, jialir, geepack, broom, patchwork, paletteer, ggthemes,
               ggsci, cowplot, ggstats, ggpubr, viridisLite, grid)

# load data (not included)

foodList <- c("meal_itemA_refineGrain","meal_itemB_wholeGrain","meal_itemC_fishSeafood","meal_itemD_chicken",
              "meal_itemE_otherMeat","meal_itemF_egg","meal_itemG_dairy", "meal_itemx_soyBeanNut", "meal_itemJ_vege",
              "meal_itemK_fruit","meal_itemL_fried","meal_itemM_sweets", "meal_drinkSugar", "meal_drinkNoSugar")
moveList <- c("p5_pre6hLPA_hr", "p5_pre6hMVPA_hr", "p5_pre6.24hLPA_hr", "p5_pre6.24hMVPA_hr",
              "p5_post2hLPA_hr", "p5_post2hMVPA_hr", "pre24hSleep_hr")
covList <- c("age", "gender", "ethnicity", "edu", "smoking", "alcohol", "bmi", "glycemic") 
dipListNum <- c("dipPctBaseGlu")

#----------------------------------#----------------------------------
# model function
predict.fun <- function(dtm, outcome = '', varList = c(covList, "meal_timeBand3"), outcomeType) {
  dtm <- dtm %>% select(vid, all_of(c(outcome, varList))) %>% na.omit %>% mutate(vid = factor(vid)) %>% arrange(vid)
  if(outcomeType == "numeric") {
    fit <- eval(parse(text = glue("geeglm({outcome} ~ {paste(c({varList}), collapse = '+')}, id = vid, data = dtm)")))
    out <- fit %>% tidy(conf.int = T) %>% 
      mutate(Estimate_CI = est_ci(estimate, conf.low, conf.high, sep = ", ", digit = 2),
             pvalue = p.value, yVariable = outcome) %>%
      rename(xVariable = term) %>%
      select(yVariable, xVariable, Estimate_CI, p.value, estimate, conf.low, conf.high)
  }
  if(outcomeType == "binary") {
    fit <- eval(parse(text = glue("geeglm({outcome} ~ {paste(c({varList}), collapse = '+')}, id = vid, family = 'binomial', data = dtm)")))
    out <- fit %>% tidy(conf.int = T, exponentiate = T) %>% # OR
      mutate(Estimate_CI = est_ci(estimate, conf.low, conf.high, sep = ", ", digit = 2),
             pvalue = p.value, yVariable = outcome) %>%
      rename(xVariable = term) %>%
      select(yVariable, xVariable, Estimate_CI, p.value, estimate, conf.low, conf.high)
  }
  out
}


# main model output
predict.fun(dtm = mealx, outcome = dipListNum, varList = c(covList, "meal_timeBand3", foodList, moveList), outcomeType = "numeric") 

# further adj for iAUC:
predict.fun(dtm = mealx, outcome = dipListNum, varList = c(covList, "iAUC_h", "meal_timeBand3", foodList, moveList), outcomeType = "numeric")  
