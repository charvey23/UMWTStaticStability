## -------- Analyze wind tunnel data ----------

## ---------------- Load libraries ----------------
library(ggplot2)
library(ggthemes) # need for geom_rangeframe

library(quantities) # needed for error propagation - uses first order Taylor series
library(emmeans)  # for emmip https://stats.idre.ucla.edu/r/seminars/interactions-r/
library(spatstat) # needed for the convex hull fitting
library(ptinpoly) # need for determining points outside the convex hull

## ---------------- Load functions ----------------
setwd("/Users/christinaharvey/Google Drive/DoctoralThesis/StaticStability/AvianWingLLT") #For Windows
source("interaction_info.R")

## This code reads in the output from the wind tunnel data on the 9 3D printed gull wings to extrapolate the key parameters
setwd("/Users/christinaharvey/Google Drive/DoctoralThesis/WindTunnel/2020_Gull_PassiveLongitudinalStudy/Data") #For MAC
setwd("/Users/Inman PC/Google Drive/DoctoralThesis/WindTunnel/2020_Gull_PassiveLongitudinalStudy/Data") #For MAC

dat_raw  <- read.csv('2020_09_WindTunnelCompleteData_full.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_wing <- read.csv('2020_08_26_selectedwtwings.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_wing$WingID = paste("F", dat_wing$frameID, sep = "")


dat_raw$PT_V[count]      <- lapply(completedata$PT_V[count], function(x) set_errors(x, info_PT_V$data_std_true))
dat_raw$LC_FX_V[count]   <- lapply(completedata$LC_FX_V_std[count], function(x) set_errors(x, info_LC_FX_V$data_std_true))
dat_raw$LC_FY_V[count]   <- lapply(completedata$LC_FY_V_std[count], function(x) set_errors(x, info_LC_FY_V$data_std_true))
dat_raw$LC_FZ_V[count]   <- lapply(completedata$LC_FZ_V_std[count], function(x) set_errors(x, info_LC_FZ_V$data_std_true))
dat_raw$LC_MX_V[count]   <- lapply(completedata$LC_MX_V_std[count], function(x) set_errors(x, info_LC_MX_V$data_std_true))
dat_raw$LC_MY_V[count]   <- lapply(completedata$LC_MY_V_std[count], function(x) set_errors(x, info_LC_MY_V$data_std_true))
dat_raw$LC_MZ_V[count]   <- lapply(completedata$LC_MZ_V_std[count], function(x) set_errors(x, info_LC_MZ_V$data_std_true))
dat_raw$T_atm[count]     <- lapply(completedata$T_atm_std[count], function(x) set_errors(x, info_T_atm$data_std_true))






# pre-define the matrix
dat_stab_exp  <- data.frame(matrix(nrow = 18, ncol = 3))
names(dat_stab_exp) <- c("WingID","elbow","manus")
count = 1
# manually input the stall angle of each wing; first column is low speed second column is high speed
first_stall_alpha = rbind(c(24,24), # F4849
                          c(23,23), # F4911
                          c(18,24), # F6003
                          c(16,21), # F2195
                          c(22,21), # F4647
                          c(16,23), # F4352
                          c(23,22), # F3891
                          c(12,16), # F1380
                          c(14,20)) # F4546
avg_root_chord = 0.1959887
max_S          = 0.1644825
# **Probably want to non-dimensionalize by the full maximum avaiable wing area and the average root chord
# merge the results with the geometry
dat_full = merge(dat_raw,dat_wing, by ="WingID")
dat_full$CL = dat_full$FL_wt_mean/(0.5*dat_full$rho*dat_full$U^2*dat_full$S)
dat_full$CD = dat_full$FD_wt_mean/(0.5*dat_full$rho*dat_full$U^2*dat_full$S)
dat_full$Cm = dat_full$Mm_wt_mean/(0.5*dat_full$rho*dat_full$U^2*dat_full$S*dat_full$MAC)

dat_full$CL_avg = dat_full$FL_wt_mean/(0.5*dat_full$rho*dat_full$U^2*(0.5*max_S))
dat_full$CD_avg = dat_full$FD_wt_mean/(0.5*dat_full$rho*dat_full$U^2*(0.5*max_S))
dat_full$Cm_avg = dat_full$Mm_wt_mean/(0.5*dat_full$rho*dat_full$U^2*(0.5*max_S)*avg_root_chord)
# get to a number that is comparable to the numerical data
dat_full$L_dim = dat_full$FL_wt_mean/(dat_full$rho*dat_full$U^2*(0.8)^2)
dat_full$D_dim = dat_full$FD_wt_mean/(dat_full$rho*dat_full$U^2*(0.8)^2)
dat_full$m_dim = dat_full$Mm_wt_mean/(dat_full$rho*dat_full$U^2*(0.8)^3)

# Loop through every wing configuration and test to save key results
for (i in 1:nrow(dat_wing)){
  for (j in 1:2){

    curr_WingID = paste("F", dat_wing$frameID[i], sep = "")

    if (j == 1){
      dat_curr <- subset(dat_full, WingID == curr_WingID & U < 14)
      if(nrow(dat_curr) < 4){next}
    }else{
      dat_curr <- subset(dat_full, WingID == curr_WingID & U > 14)
      if(nrow(dat_curr) < 4){next}
    }

    dat_stab_exp$U[count]        = mean(dat_curr$U)
    dat_stab_exp$WingID[count]   = curr_WingID
    dat_stab_exp$elbow[count]    = dat_wing$elbow[i]
    dat_stab_exp$manus[count]    = dat_wing$manus[i]
    dat_stab_exp$dihedral[count] = mean(dat_curr$dihedral)
    dat_stab_exp$sweep[count]    = mean(dat_curr$sweep)
    dat_stab_exp$twist[count]    = mean(dat_curr$twist)
    dat_stab_exp$MAC[count]      = mean(dat_curr$MAC)
    dat_stab_exp$b[count]        = mean(dat_curr$b)
    # ----- Determine the linear region ------
    # angle of attack where lift = 0 - uses extrapolation
    lift_low  = -min(abs(dat_curr$L_dim[which(dat_curr$L_dim < 0)]))
    lift_high = min(abs(dat_curr$L_dim[which(dat_curr$L_dim > 0)]))
    dat_stab_exp$alpha_lift0[count]    = (lift_low*(dat_curr$alpha[which(dat_curr$L_dim == lift_high)]-dat_curr$alpha[which(dat_curr$L_dim == lift_low)])/(lift_high-lift_low)) + dat_curr$alpha[which(dat_curr$L_dim == lift_low)]
    # angle of attack where pitch = 0 - uses extrapolation
    pitch_low  = -min(abs(dat_curr$m_dim[which(dat_curr$m_dim < 0)]))
    pitch_high = min(abs(dat_curr$m_dim[which(dat_curr$m_dim > 0)]))
    dat_stab_exp$alpha_pitch0[count]   = (pitch_low*(dat_curr$alpha[which(dat_curr$m_dim == pitch_high)]-dat_curr$alpha[which(dat_curr$m_dim == pitch_low)])/(pitch_high-pitch_low)) + dat_curr$alpha[which(dat_curr$m_dim == pitch_low)]

    # Maximum lift at the first stall point
    dat_stab_exp$lift_max[count]       = dat_curr$L_dim[which(dat_curr$alpha == first_stall_alpha[i,j])]

    # Save the maximum lift to drag ratio
    dat_stab_exp$LD_max[count]      <- max(dat_curr$FL_wt_mean/dat_curr$FD_wt_mean)
    dat_stab_exp$alpha_L_D[count]   <- dat_curr$alpha[which.max(dat_curr$FL_wt_mean/dat_curr$FD_wt_mean)]
    dat_stab_exp$LD_max_ci[count]   <- abs(dat_stab_exp$LD_max[count])*sqrt((dat_curr$FL_wt_ci[which(dat_curr$alpha == dat_stab_exp$alpha_L_D[count])]/dat_curr$FL_wt_mean[which(dat_curr$alpha == dat_stab_exp$alpha_L_D[count])])^2 + (dat_curr$FD_wt_ci[which(dat_curr$alpha == dat_stab_exp$alpha_L_D[count])]/dat_curr$FD_wt_mean[which(dat_curr$alpha == dat_stab_exp$alpha_L_D[count])])^2)
    # Save the minimum drag
    dat_stab_exp$D_min[count]       <- min(dat_curr$FD_wt_mean)
    dat_stab_exp$D_dim_min[count]   <- min(dat_curr$D_dim)
    # Save the maximum lift before stall
    dat_stab_exp$L_max[count]       <- dat_curr$FL_wt_mean[which(dat_curr$alpha == first_stall_alpha[i,j])]
    dat_stab_exp$L_dim_max[count]   <- dat_curr$L_dim[which(dat_curr$alpha == first_stall_alpha[i,j])]
    # ----------------- Fit models to the linear range --------------------------

    # Define the linear data range
    dat_curr_lin = subset(dat_curr, alpha < first_stall_alpha[i,j]-2 & alpha > dat_stab_exp$alpha_lift0[count]+2)
      if (count == 1){
        dat_lin_exp  = dat_curr_lin
      } else{
        dat_lin_exp  = rbind(dat_lin_exp,dat_curr_lin)
      }

    #fit linear model to Cm vs. CL
    mod.pstab  <- lm(m_dim~L_dim, data = dat_curr_lin)
    test       <- summary(mod.pstab)
    mod.pstaba <- lm(m_dim~alpha, data = dat_curr_lin)
    mod.lift   <- lm(L_dim~alpha, data = dat_curr_lin)
    mod.drag   <- lm(D_dim~poly(CL_avg,2), data = dat_curr) # doesn't need to be determined in the linear region

    # save all info about the lnear model
    # ------------- Cm/CL -------------
    # Intercept
    dat_stab_exp$cm0[count]     <- summary(mod.pstab)$coefficients[1,1]
    dat_stab_exp$cm0_err[count] <- summary(mod.pstab)$coefficients[1,2]
    dat_stab_exp$cm0_lcb[count] <- confint(mod.pstab)[1,1]
    dat_stab_exp$cm0_ucb[count] <- confint(mod.pstab)[1,2]
    # Slope
    dat_stab_exp$cmcl[count]    <- summary(mod.pstab)$coefficients[2,1]
    dat_stab_exp$cmcl_err[count]<- summary(mod.pstab)$coefficients[2,2]
    dat_stab_exp$R2[count]      <- summary(mod.pstab)$r.squared
    dat_stab_exp$cmcl_lcb[count]<- confint(mod.pstab)[2,1]
    dat_stab_exp$cmcl_ucb[count]<- confint(mod.pstab)[2,2]

    # ------------- Cm/alpha -------------
    # Intercept
    dat_stab_exp$cm0_a[count]     <- summary(mod.pstaba)$coefficients[1,1]
    dat_stab_exp$cm0_a_err[count] <- summary(mod.pstaba)$coefficients[1,2]
    dat_stab_exp$cm0_a_lcb[count] <- confint(mod.pstaba)[1,1]
    dat_stab_exp$cm0_a_ucb[count] <- confint(mod.pstaba)[1,2]
    # Slope
    dat_stab_exp$cmalp[count]     <- summary(mod.pstaba)$coefficients[2,1]
    dat_stab_exp$cmalp_err[count] <- summary(mod.pstaba)$coefficients[2,2]
    dat_stab_exp$cmalp_lcb[count] <- confint(mod.pstaba)[2,1]
    dat_stab_exp$cmalp_ucb[count] <- confint(mod.pstaba)[2,2]

    # ------------- CL/alpha -------------
    # Intercept
    dat_stab_exp$cl0[count]      <- summary(mod.lift)$coefficients[1,1]
    dat_stab_exp$cl0_err[count]  <- summary(mod.lift)$coefficients[1,2]
    dat_stab_exp$cl0_lcb[count]  <- confint(mod.lift)[1,1]
    dat_stab_exp$cl0_ucb[count]  <- confint(mod.lift)[1,2]
    # Slope
    dat_stab_exp$clalp[count]    <- summary(mod.lift)$coefficients[2,1]
    dat_stab_exp$clalp_err[count]<- summary(mod.lift)$coefficients[2,2]
    dat_stab_exp$clalp_lcb[count]<- confint(mod.lift)[2,1]
    dat_stab_exp$clalp_ucb[count]<- confint(mod.lift)[2,2]

    # ------------- Cd/alpha -------------
    # Intercept
    dat_stab_exp$cd0[count]       <- summary(mod.drag)$coefficients[1,1]
    dat_stab_exp$cd0_err[count]   <- summary(mod.drag)$coefficients[1,2]
    dat_stab_exp$cd0_lcb[count]   <- confint(mod.drag)[1,1]
    dat_stab_exp$cd0_ucb[count]   <- confint(mod.drag)[1,2]
    # Slope of B1
    dat_stab_exp$cdalp[count]     <- summary(mod.drag)$coefficients[2,1]
    dat_stab_exp$cdalp_err[count] <- summary(mod.drag)$coefficients[2,2]
    dat_stab_exp$cdalp_lcb[count] <- confint(mod.drag)[2,1]
    dat_stab_exp$cdalp_ucb[count] <- confint(mod.drag)[2,2]
    # Slope of B2
    dat_stab_exp$cdalp2[count]    <- summary(mod.drag)$coefficients[3,1]
    dat_stab_exp$cdalp2_err[count]<- summary(mod.drag)$coefficients[3,2]
    dat_stab_exp$cdalp2_lcb[count]<- confint(mod.drag)[3,1]
    dat_stab_exp$cdalp2_ucb[count]<- confint(mod.drag)[3,2]

    count = count + 1
  }
}

# Models that are used to compare to experimental results
mod_con_cL_exp = lm(L_dim ~ elbow + manus + alpha, data = subset(dat_full, U <14))
mod_con_cm_exp = lm(m_dim ~ elbow + manus + alpha, data = subset(dat_full, U <14))
mod_con_cd_exp = lm(D_dim ~ elbow+manus + poly(alpha,2), data = subset(dat_full, U <14))

mod_cmcl_exp = lm(cmcl ~ elbow + manus, data = subset(dat_stab_exp, U < 14))
mod_cm0_exp  = lm(cm0 ~ elbow + manus, data = subset(dat_stab_exp, U < 14))

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
  dat_comp$mean[i]   = summary(mod_con_cL_exp)$coefficients[i,1]
  dat_comp$low_ci[i] = confint(mod_con_cL_exp)[i,1]
  dat_comp$upp_ci[i] = confint(mod_con_cL_exp)[i,2]

  dat_comp$mean[i+4]   = summary(mod_con_cL_num)$coefficients[i,1]
  dat_comp$low_ci[i+4] = confint(mod_con_cL_num)[i,1]
  dat_comp$upp_ci[i+4] = confint(mod_con_cL_num)[i,2]

  dat_comp$mean[i+8]   = summary(mod_con_cm_exp)$coefficients[i,1]
  dat_comp$low_ci[i+8] = confint(mod_con_cm_exp)[i,1]
  dat_comp$upp_ci[i+8] = confint(mod_con_cm_exp)[i,2]

  dat_comp$mean[i+12]   = summary(mod_con_cm_num)$coefficients[i,1]
  dat_comp$low_ci[i+12] = confint(mod_con_cm_num)[i,1]
  dat_comp$upp_ci[i+12] = confint(mod_con_cm_num)[i,2]

  if (i < 4){
    dat_comp$mean[i+16]   = summary(mod_cmcl_exp)$coefficients[i,1]
    dat_comp$low_ci[i+16] = confint(mod_cmcl_exp)[i,1]
    dat_comp$upp_ci[i+16] = confint(mod_cmcl_exp)[i,2]

    dat_comp$mean[i+19]   = summary(mod_cmcl_num)$coefficients[i,1]
    dat_comp$low_ci[i+19] = confint(mod_cmcl_num)[i,1]
    dat_comp$upp_ci[i+19] = confint(mod_cmcl_num)[i,2]
  }

}


# Define key analysis points
manus_pts <- seq(100, 180, by=10)
elbow_pts <- seq(100, 180, by=10)

# determined the effect of manus by elbow
dat_int_elb_cm <- interaction_info(mod_con_cm, "elbow", elbow_pts, manus_pts, unique(dat_all[4:5]))
dat_int_elb_cl <- interaction_info(mod_con_cl, "elbow", elbow_pts, manus_pts, unique(dat_all[4:5]))
dat_int_elb_cd <- interaction_info(mod_con_cd, "elbow", elbow_pts, manus_pts, unique(dat_all[4:5]))
# determined the effect of elbow by manus
dat_int_man_cm <- interaction_info(mod_con_cm, "manus", elbow_pts, manus_pts, unique(dat_all[4:5]))
dat_int_man_cl <- interaction_info(mod_con_cl, "manus", elbow_pts, manus_pts, unique(dat_all[4:5]))
dat_int_man_cd <- interaction_info(mod_con_cd, "manus", elbow_pts, manus_pts, unique(dat_all[4:5]))
