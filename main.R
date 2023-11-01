source("utils.R")

cover_path_1 = 0
cover_path_2 = 0
cover_path_3 = 0
cover_path_4 = 0
cover_confounder = 0
sd_path_1 = 0
sd_path_2 = 0
sd_path_3 = 0
sd_path_4 = 0
sd_confounder = 0
bias_path_1 = 0
bias_path_2 = 0
bias_path_3 = 0
bias_path_4 = 0
bias_confounder = 0

data = sim_data(n=5000, option = 1, option_zero = 2)$sim_data
data = data.frame(apply(data, 2, as.numeric))

attr_models_outcome = c("mean", "glm", "earth", "lightgbm")
attr_models_multiclass = c("multinom", "lightgbm")

params <- .recanting_twins_control(#.g_folds = 10,
  .g_learners = attr_models_outcome,
  .m_learners = attr_models_multiclass,
  #.m_folds = 10,
  .pmz_learners = attr_models_outcome,
  #.pmz_folds = 10,
  .pz_learners = attr_models_multiclass,
  #.pz_folds = 10,
  .pm1_learners = attr_models_multiclass,
  #.pm1_folds = 10,
  .pm2_learners = attr_models_multiclass)
#.pm2_folds = 10)

res = recanting_twins(data, c("X_1", "X_2", "X_3"), "A", "Z", "M", "y", "binomial", .control = params)

bias_path_1 = res$p1
bias_path_2 = res$p2
bias_path_3 = res$p3
bias_path_4 = res$p4
bias_confounder = res$intermediate_confounding

low_path_1 = round(calc_ci(res$p1, res$eif_p1)[1], 4)
high_path_1 = round(calc_ci(res$p1, res$eif_p1)[2], 4)

low_path_2 = round(calc_ci(res$p2, res$eif_p2)[1], 4)
high_path_2 = round(calc_ci(res$p2, res$eif_p2)[2], 4)

low_path_3 = round(calc_ci(res$p3, res$eif_p3)[1], 4)
high_path_3 = round(calc_ci(res$p3, res$eif_p3)[2], 4)

low_path_4 = round(calc_ci(res$p4, res$eif_p4)[1], 4)
high_path_4 = round(calc_ci(res$p4, res$eif_p4)[2], 4)

if (is.na(low_path_1) || is.na(high_path_1)){
  cover_path_1 = NA
} else if (low_path_1 < 0 && high_path_1 > 0){
  cover_path_1 = 1
}

if (is.na(low_path_2) || is.na(high_path_2)){
  cover_path_2 = NA
} else if (low_path_2 < 0 && high_path_2 > 0){
  cover_path_2 = 1
}

if (is.na(low_path_3) || is.na(high_path_3)){
  cover_path_3 = NA
} else if(low_path_3 < 0 && high_path_3 > 0){
  cover_path_3 = 1
}

if (is.na(low_path_4) || is.na(high_path_4)){
  cover_path_4 = NA
} else if(low_path_4 < 0 && high_path_4 > 0){
  cover_path_4 = 1
}

low_confounder = round(calc_ci(res$intermediate_confounding, res$eif_intermediate_confounding)[1], 4)
high_confounder = round(calc_ci(res$intermediate_confounding, res$eif_intermediate_confounding)[2], 4)

if (is.na(low_confounder) || is.na(high_confounder)){
  cover_confounder = NA
} else if(low_confounder < 0 && high_confounder > 0){
  cover_confounder = 1
}

sd_path_1 = (res$p1 - calc_ci(res$p1, res$eif_p1)[1]) / qnorm(0.975)
sd_path_2 = (res$p2 - calc_ci(res$p2, res$eif_p2)[1]) / qnorm(0.975)
sd_path_3 = (res$p3 - calc_ci(res$p3, res$eif_p3)[1]) / qnorm(0.975)
sd_path_4 = (res$p4 - calc_ci(res$p4, res$eif_p4)[1]) / qnorm(0.975)
sd_confounder = (res$intermediate_confounding - calc_ci(res$intermediate_confounding, res$eif_intermediate_confounding)[1]) / qnorm(0.975)
