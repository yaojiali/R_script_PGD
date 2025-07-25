pacman::p_load(tidyverse, skimr, jialir, data.table)

# load data (not included)

varAppetite1 <- c("nextMeal_hrLater", "nextMealOrSnack_hrLater", "hungry_post23h", "hungry_diff23h")
varAppetite2 <- c("hungry_post34h", "hungry_diff34h") 
varGlu <- c("dipPctBaseGlu", "iAUC_h", "risePctBaseGlu")
tpd_f3 <- expand.grid(varGlu, varAppetite1) 
tpd_f4 <- expand.grid(varGlu, varAppetite2) 


#------------------ Spearman r
get.r_spearman <- function(x, y, B = 1000, dt0 = mealx) {
  dt <- dt0 %>% select(vid, x, y) %>% filter(complete.cases(.))
  dt <- dt %>% group_by(vid) %>% mutate_at(vars(!vid), ~.x - mean(.x)) %>% ungroup() # person-mean centered
  ids <- dt$vid %>% unique
  est <- cor(pull(dt, x), pull(dt, y), method = "spearman")
  
  set.seed(2025)
  sampleR.fun <- function() {
    sid <- sample(ids, size = length(ids), replace = T)
    sdt <- sid %>% lapply(function(i) {dt %>% filter(vid == i)}) %>% 
      rbindlist()
    sr <- sdt %>% select(-vid) %>% cor(method = "spearman")
    sr[2]
  }
  Boot_sample_r <- replicate(B, sampleR.fun()) 
  data.frame(var1 = x, var2 = y, estimate = est, ci_low = quantile(Boot_sample_r, 0.025), 
             ci_high = quantile(Boot_sample_r, 0.975))
}

map2(tpd_f3$Var1, tpd_f3$Var2, \(x, y) {
  get.r_spearman(x, y, B = 1000, dt0 = mealx)}) %>% rbindlist()

map2(tpd_f4$Var1, tpd_f4$Var2, \(x, y) {
  get.r_spearman(x, y, B = 1000, dt0 = mealx %>% filter(nextMealOrSnack_hrLater >=4))}) %>% rbindlist()

get.r_spearman("dipPctBaseGlu", "iAUC_h", B = 1000, dt0 = mealx) %>% 
  mutate(est = est_ci(estimate, ci_low, ci_high, sep = ", ", digit = 3))
get.r_spearman("dipPctBaseGlu", "risePctBaseGlu", B = 1000, dt0 = mealx) %>% 
  mutate(est = est_ci(estimate, ci_low, ci_high, sep = ", ", digit = 3))


#------------------ Pearson r
get.r_pearson <- function(x, y, B = 1000, dt0 = mealx) {
  dt <- dt0 %>% select(vid, x, y) %>% filter(complete.cases(.))
  dt <- dt %>% group_by(vid) %>% mutate_at(vars(!vid), ~.x - mean(.x)) %>% ungroup() # person-mean centered
  ids <- dt$vid %>% unique
  est <- cor(pull(dt, x), pull(dt, y))
  
  set.seed(2025)
  sampleR.fun <- function() {
    sid <- sample(ids, size = length(ids), replace = T)
    sdt <- sid %>% lapply(function(i) {dt %>% filter(vid == i)}) %>% 
      rbindlist()
    sr <- sdt %>% select(-vid) %>% cor
    sr[2]
  }
  Boot_sample_r <- replicate(B, sampleR.fun()) 
  data.frame(var1 = x, var2 = y, estimate = est, ci_low = quantile(Boot_sample_r, 0.025), 
             ci_high = quantile(Boot_sample_r, 0.975))
  }

map2(tpd_f3$Var1, tpd_f3$Var2, \(x, y) {
  get.r_pearson(x, y, B = 1000, dt0 = mealx)}) %>% rbindlist()

map2(tpd_f4$Var1, tpd_f4$Var2, \(x, y) {
  get.r_pearson(x, y, B = 1000, dt0 = mealx %>% filter(nextMealOrSnack_hrLater >=4))}) %>% rbindlist()

get.r_pearson("dipPctBaseGlu", "iAUC_h", B = 1000, dt0 = mealx) %>% 
  mutate(est = est_ci(estimate, ci_low, ci_high, sep = ", ", digit = 3))
get.r_pearson("dipPctBaseGlu", "risePctBaseGlu", B = 1000, dt0 = mealx) %>% 
  mutate(est = est_ci(estimate, ci_low, ci_high, sep = ", ", digit = 3))


