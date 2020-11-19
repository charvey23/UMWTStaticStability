dat_num_simp        <- subset(dat_num, FrameID %in% wtwings & WingID == "17_0285")[,c(4,7,5,6,31,32,33)]
dat_num_simp$method <- "n"
dat_exp_simp        <- subset(dat_exp, U < 14 & alpha < 10 & alpha > -10)[,c(1,4,47,48,38,40,42)]
dat_exp_simp$method <- "e"
dat_comp_simp       <- rbind(dat_exp_simp,dat_num_simp)

dat_stab_exp_simp        <- subset(dat_stab_exp, U < 14)[,c(1,2,3,19,23)]
dat_stab_exp_simp$method <- "e"
dat_stab_num_simp        <- dat_stab_comp[,c(4,5,6,7,11)]
dat_stab_num_simp$method <- "n"
dat_stab_comp_simp       <- rbind(dat_stab_exp_simp,dat_stab_num_simp)
remove(dat_stab_exp_simp,dat_stab_num_simp)

# Models
mod_comp_L    = lm(L_comp ~ elbow*manus + alpha + elbow:method + manus:method + elbow:manus:method + alpha:method + method, data = dat_comp_simp)
mod_comp_m    = lm(m_comp ~ elbow*manus + alpha + elbow:method + manus:method + elbow:manus:method + alpha:method + method, data = dat_comp_simp)
mod_comp_d    = lm(D_comp ~ elbow*manus + alpha + elbow:method + manus:method + elbow:manus:method + alpha:method + method, data = dat_comp_simp)

mod_comp_cmcl = lm(cmcl ~ elbow*manus*method, data = dat_stab_comp_simp)
mod_comp_cm0  = lm(cm0 ~ elbow*manus*method, data = dat_stab_comp_simp)

# Models that are used to visualize the confidence intervals predited from each dataset
mod_con_cL_exp = lm(L_comp ~ elbow*manus + alpha, data = subset(dat_comp_simp, method == "e"))
mod_con_cm_exp = lm(m_comp ~ elbow*manus + alpha, data = subset(dat_comp_simp, method == "e"))
mod_cmcl_exp = lm(cmcl ~ elbow*manus, data = subset(dat_stab_comp_simp, method == "e"))

mod_con_cL_num = lm(L_comp ~ elbow*manus + alpha, data = subset(dat_comp_simp, method == "n"))
mod_con_cm_num = lm(m_comp ~ elbow*manus + alpha, data = subset(dat_comp_simp, method == "n"))
mod_cmcl_num  = lm(cmcl ~ elbow*manus, data = subset(dat_stab_comp_simp, method == "n"))

# ---- Create the data frame used to compare numerical to experimental results
dat_comp <- data.frame(matrix(nrow = 22, ncol = 6))
names(dat_comp) <- c("value","effect","numorexp","low_ci","mean","upp_ci")

dat_comp$value[1:8]   = "L"
dat_comp$value[9:16]  = "m"
dat_comp$value[17:22] = "cmcl"

dat_comp$effect[c(1,5,9,13,17,20)]    = "intercept"
dat_comp$effect[c(2,6,10,14,18,21)]   = "elbow"
dat_comp$effect[c(3,7,11,15,19,22)]   = "wrist"
dat_comp$effect[c(4,8,12,16)]         = "alpha"

dat_comp$numorexp[c(1:4,9:12,17:19)]  = "exp"
dat_comp$numorexp[c(5:8,13:16,20:22)] = "num"

for (i in 1:4){
  mean_exp   = coef(mod_con_cL_exp)[i]
  highci_exp = confint(mod_con_cL_exp)[i,2]
  dat_comp$mean[i]   =  0
  dat_comp$low_ci[i] = -1
  dat_comp$upp_ci[i] =  1
  
  dat_comp$mean[i+4]   = (coef(mod_con_cL_num)[i] - mean_exp)/(highci_exp-mean_exp)
  dat_comp$low_ci[i+4] = (confint(mod_con_cL_num)[i,1] - mean_exp)/(highci_exp-mean_exp)
  dat_comp$upp_ci[i+4] = (confint(mod_con_cL_num)[i,2] - mean_exp)/(highci_exp-mean_exp)
  
  mean_exp   = coef(mod_con_cm_exp)[i]
  highci_exp = confint(mod_con_cm_exp)[i,2]
  dat_comp$mean[i+8]   =  0
  dat_comp$low_ci[i+8] = -1
  dat_comp$upp_ci[i+8] =  1
  
  dat_comp$mean[i+12]   = (coef(mod_con_cm_num)[i] - mean_exp)/(highci_exp-mean_exp)
  dat_comp$low_ci[i+12] = (confint(mod_con_cm_num)[i,1] - mean_exp)/(highci_exp-mean_exp)
  dat_comp$upp_ci[i+12] = (confint(mod_con_cm_num)[i,2] - mean_exp)/(highci_exp-mean_exp)
  
  if (i < 4){
    mean_exp   = coef(mod_cmcl_exp)[i]
    highci_exp = confint(mod_cmcl_exp)[i,2]
    dat_comp$mean[i+16]   =  0
    dat_comp$low_ci[i+16] = -1
    dat_comp$upp_ci[i+16] =  1
    
    dat_comp$mean[i+19]   = (coef(mod_cmcl_num)[i] - mean_exp)/(highci_exp-mean_exp)
    dat_comp$low_ci[i+19] = (confint(mod_cmcl_num)[i,1] - mean_exp)/(highci_exp-mean_exp)
    dat_comp$upp_ci[i+19] = (confint(mod_cmcl_num)[i,2] - mean_exp)/(highci_exp-mean_exp)
  }
  
}



