pacman::p_load(tidyverse, skimr, glue, jialir, geepack,broom)

#load data (not included)

covList1 <- c("age", "gender", "ethnicity", "edu", "smoking", "alcohol", "bmi", "glycemic", "meal_timeBand3")
outvarF3num <- c("nextMeal_hrLater_min", "nextMealOrSnack_hrLater_min", "hungry_post23h", "hungry_diff23h")
outvarF4num <- c("hungry_post34h", "hungry_diff34h")
expoList <- c("dipPctBaseGlu_10", "dipYes")

tpd_f3num <- expand.grid(outcomeList = outvarF3num, expoList = expoList, stringsAsFactors = F) 
tpd_f4num <- expand.grid(outcomeList = outvarF4num, expoList = expoList, stringsAsFactors = F) 

#---------------------------------- Main model
predict.fun <- function(dtm, outcome, exposure = "", varList) {
  dtm <- dtm %>% select(vid, c(outcome, exposure, varList)) %>% na.omit %>% mutate(vid = factor(vid)) %>% arrange(vid)
  
  fit <- eval(parse(text = glue("geeglm({outcome} ~ {paste(c({exposure}, {varList}), collapse = '+')}, id = vid, data = dtm)")))
  fit %>% tidy(conf.int = T) %>% 
    mutate(Estimate_CI = est_ci(estimate, conf.low, conf.high, sep = ", ", digit = 2),
           pvalue = p.value, yVariable = outcome) %>%
    rename(xVariable = term) %>%
    select(yVariable, xVariable, Estimate_CI, pvalue, everything()) %>%
    select(-std.error, -statistic, -p.value) %>% 
    filter(grepl(exposure, xVariable)) 
}

map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
  predict.fun(dtm = mealx, outcome = y, exposure = x, varList = covList1) }) %>% data.table::rbindlist()

map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
  predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4), outcome = y, exposure = x, varList = covList1)}) %>% data.table::rbindlist()


#---------------------------------- Within-person model
# restrict to vid with >=2 qualified meals
predict.fun_within <- function(dtm, outcome, exposure = "dipPctBaseGlu_10", varList) {
  dtm <- dtm %>% select(vid, c(outcome, exposure, varList)) %>% na.omit %>%
    group_by(vid) %>% mutate(n = n()) %>% filter(n>=2) %>% 
    mutate(vid = factor(vid)) %>% arrange(vid)
  
  eval(parse(text = glue("dtm <- dtm %>% group_by(vid) %>% mutate({exposure}_between = mean({exposure})) %>% 
    mutate({exposure}_within = {exposure} - {exposure}_between) %>% ungroup")))
  
  exposure2 <- paste0(exposure, c("_within"))
  
  fit <- eval(parse(text = glue("geeglm({outcome} ~ {paste(c({exposure2}, {varList}), collapse = '+')}, id = vid, data = dtm)")))
  fit %>% tidy(conf.int = T) %>% 
    mutate(Estimate_CI = est_ci(estimate, conf.low, conf.high, sep = ", ", digit = 2),
           pvalue = p.value, yVariable = outcome) %>%
    rename(xVariable = term) %>%
    select(yVariable, xVariable, Estimate_CI, pvalue, everything()) %>%
    filter(grepl(exposure, xVariable)) %>% mutate(xvar = exposure) %>% 
    select(yVariable:estimate, conf.low, conf.high, xvar)
}

 map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
  predict.fun_within(dtm = mealx, outcome = y,  exposure= x,  varList = covList1)
  }) %>% data.table::rbindlist()
map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
  predict.fun_within(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4), outcome = y, exposure = x,
                         varList = covList1)}) %>% data.table::rbindlist()


#---------------------------------- additional control for postmeal fullness
covList2 <- c("age", "gender", "ethnicity", "edu", "smoking", "alcohol", "bmi", "glycemic", "meal_timeBand3", "meal_fullness")
map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
  predict.fun(dtm = mealx, outcome = y, exposure = x, varList = covList2) }) %>% data.table::rbindlist()

map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
  predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4), outcome = y, exposure = x, varList = covList2)}) %>% data.table::rbindlist()


#---------------------------------- additional control for 2-h iAUC
covList3 <- c("age", "gender", "ethnicity", "edu", "smoking", "alcohol", "bmi", "glycemic", "meal_timeBand3", "iAUC")
map2(tpd_f3num$expoList, tpd_f3num$outcomeList, \(x, y) {
  predict.fun(dtm = mealx, outcome = y, exposure = x, varList = covList3)}) %>% data.table::rbindlist()

map2(tpd_f4num$expoList, tpd_f4num$outcomeList, \(x, y) {
  predict.fun(dtm = mealx %>% filter(nextMealOrSnack_hrLater>=4), outcome = y, exposure = x, varList = covList3)}) %>% data.table::rbindlist()
