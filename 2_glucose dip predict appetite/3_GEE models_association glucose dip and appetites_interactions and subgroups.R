pacman::p_load(tidyverse, skimr, glue, jialir, geepack,broom, patchwork, paletteer, ggthemes,
               ggsci, cowplot, ggstats, ggpubr, viridisLite)

# load data (not included)

covList <- c("age", "gender", "ethnicity", "edu", "smoking", "alcohol") # no mealtime, bmi, nor glycemic status
outvarF3num <- c("nextMeal_hrLater_min", "nextMealOrSnack_hrLater_min", "hungry_post23h", "hungry_diff23h")
outvarF4num <- c("hungry_post34h", "hungry_diff34h")
expoList <- c("dipPctBaseGlu_10", "dipYes")
tpd_f3num <- expand.grid(outcomeList = outvarF3num, expoList = expoList, stringsAsFactors = F) 
tpd_f4num <- expand.grid(outcomeList = outvarF4num, expoList = expoList, stringsAsFactors = F) 

#--------------------------------- check interactions
predict.fun_int3 <- function(dtm, outcome, exposure = "", varList = c(covList, "meal_timeBand3", "glycemic", "bmi_gp"),
                             int_vars = c("meal_timeBand3", "glycemic", "bmi_gp")) {
  dtm <- dtm %>% select(vid, c(outcome, exposure, varList)) %>% na.omit %>%
    mutate(vid = factor(vid)) %>% arrange(vid)
  
  fit <- eval(parse(text = glue("geeglm({outcome} ~ {paste(c(glue('{exposure}*{int_vars}'), {varList}), collapse = '+')}, id = vid, data = dtm)")))
  fit %>% tidy(conf.int = T) %>%
    mutate(Estimate_CI = est_ci(estimate, conf.low, conf.high, sep = ", ", digit = 2),
           pvalue = p.value, yVariable = outcome) %>%
    rename(xVariable = term) %>%
    select(yVariable, xVariable, Estimate_CI, pvalue) %>%
    filter(grepl(exposure, xVariable)) %>% mutate(covN = length(varList))
}
map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
  predict.fun_int3(dtm = mealx, outcome = y, exposure = x)}) %>%
  data.table::rbindlist() %>% filter(pvalue<0.05, grepl(":", xVariable)) %>% arrange(yVariable)

map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
  predict.fun_int3(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4), outcome = y, exposure = x)}) %>% 
  data.table::rbindlist() %>% filter(pvalue<0.05, grepl(":", xVariable))


#-----------------#-----------------
# stratified subgroup analysis
#-----------------#-----------------
predict.fun <- function(dtm, outcome, exposure = "", varList) {
  dtm <- dtm %>% select(vid, c(outcome, exposure, varList)) %>% na.omit %>% mutate(vid = factor(vid)) %>% arrange(vid)
  fit <- eval(parse(text = glue("geeglm({outcome} ~ {paste(c({exposure}, {varList}), collapse = '+')}, id = vid, data = dtm)")))
  fit %>% tidy(conf.int = T) %>% 
    mutate(Estimate_CI = est_ci(estimate, conf.low, conf.high, sep = ", ", digit = 2),
           pvalue = p.value, yVariable = outcome) %>%
    rename(xVariable = term) %>%
    select(yVariable, xVariable, Estimate_CI, pvalue, everything()) %>%
    filter(grepl(exposure, xVariable)) 
  }
covListT <- c(covList, "bmi", "glycemic")
covListG <- c(covList, "bmi", "meal_timeBand3")
covListW <- c(covList, "meal_timeBand3", "glycemic")

#---
subMealtime <- rbind(
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(meal_timeBand3=="a.05-11"), outcome = y, exposure = x, varList = c(covListT))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "a.05-11"),
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(meal_timeBand3=="b.11-17"), outcome = y, exposure = x, varList = c(covListT))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "b.11-17"),
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(meal_timeBand3== "c.17-24"), outcome = y, exposure = x, varList = c(covListT))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "c.17-24"),
  #
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, meal_timeBand3=="a.05-11"), outcome = y, exposure = x, varList = c(covListT))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "a.05-11"),
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, meal_timeBand3=="b.11-17"), outcome = y, exposure = x, varList = c(covListT))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "b.11-17"),
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, meal_timeBand3== "c.17-24"), outcome = y, exposure = x, varList = c(covListT))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "c.17-24")) %>% 
  mutate(intVar = "meal_timeBand3") %>% arrange(yVariable, xVariable, subgroup)

#---
subGlycemic <- rbind(
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(glycemic=="1_normal"), outcome = y, exposure = x, varList = c(covListG))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "1_normal"),
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(glycemic=="2_pre-t2d"), outcome = y, exposure = x, varList = c(covListG))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "2_pre-t2d"),
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(glycemic== "3_t2d"), outcome = y, exposure = x, varList = c(covListG))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "3_t2d"),
  #
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, glycemic=="1_normal"), outcome = y, exposure = x, varList = c(covListG))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "1_normal"),
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, glycemic=="2_pre-t2d"), outcome = y, exposure = x, varList = c(covListG))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "2_pre-t2d"),
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, glycemic== "3_t2d"), outcome = y, exposure = x, varList = c(covListG))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "3_t2d")) %>% 
  mutate(intVar = "glycemic") %>% arrange(yVariable, xVariable, subgroup)

#---
subWeight <- rbind(
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(bmi_gp=="1_below23"), outcome = y, exposure = x, varList = c(covListW))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "1_below23"),
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(bmi_gp=="2_below27"), outcome = y, exposure = x, varList = c(covListW))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "2_below27"),
  map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(bmi_gp== "3_above27"), outcome = y, exposure = x, varList = c(covListW))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "3_above27"),
  #
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, bmi_gp=="1_below23"), outcome = y, exposure = x, varList = c(covListW))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "1_below23"),
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, bmi_gp=="2_below27"), outcome = y, exposure = x, varList = c(covListW))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "2_below27"),
  map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
    predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4, bmi_gp== "3_above27"), outcome = y, exposure = x, varList = c(covListW))}) %>% 
    data.table::rbindlist() %>% mutate(subgroup = "3_above27")) %>% 
  mutate(intVar = "bmi_gp") %>% arrange(yVariable, xVariable, subgroup)

