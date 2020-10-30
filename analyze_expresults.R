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
setwd("/Users/Inman PC/Documents/UMWTStaticStability/Data") #For MAC

dat_raw          <- read.csv('2020_10_26_ProcessedData.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_wing         <- read.csv('2020_08_26_selectedwtwings.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_wing$FrameID <- paste("F", dat_wing$frameID, sep = "")
dat_raw$alpha    <- as.numeric(dat_raw$alpha)
dat_raw$U        <- as.numeric(dat_raw$U)
dat_exp          <- merge(dat_raw,dat_wing, by ="FrameID")

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

## ------------------------------------------------------------------
## ------------------ Longitudinal Stability ------------------------
## ------------------------------------------------------------------

# Loop through every wing configuration and test to save key results
for (i in 1:nrow(dat_wing)){
  for (j in 1:2){

    curr_FrameID = paste("F", dat_wing$frameID[i], sep = "")

    if (j == 1){
      dat_curr <- subset(dat_exp, FrameID == curr_FrameID & U < 14)
      if(nrow(dat_curr) < 4){next}
    }else{
      dat_curr <- subset(dat_exp, FrameID == curr_FrameID & U > 14)
      if(nrow(dat_curr) < 4){next}
    }

    dat_stab_exp$U[count]        = mean(dat_curr$U)
    dat_stab_exp$FrameID[count]  = curr_FrameID
    dat_stab_exp$elbow[count]    = dat_wing$elbow[i]
    dat_stab_exp$manus[count]    = dat_wing$manus[i]
    dat_stab_exp$dihedral[count] = mean(dat_curr$dihedral)
    dat_stab_exp$sweep[count]    = mean(dat_curr$sweep)
    dat_stab_exp$twist[count]    = mean(dat_curr$twist)
    dat_stab_exp$MAC[count]      = mean(dat_curr$MAC)
    dat_stab_exp$b[count]        = mean(dat_curr$b)
    # ----- Determine the linear region ------
    # angle of attack where lift = 0 - uses linear interpolation
    lift_low  = -min(abs(dat_curr$L_comp[which(dat_curr$L_comp < 0)]))
    lift_high = min(abs(dat_curr$L_comp[which(dat_curr$L_comp > 0)]))
    dat_stab_exp$alpha_lift0[count] <- (lift_low*(dat_curr$alpha[which(dat_curr$L_comp == lift_high)]-dat_curr$alpha[which(dat_curr$L_comp == lift_low)])/(lift_high-lift_low)) + dat_curr$alpha[which(dat_curr$L_comp == lift_low)]
    
    # angle of attack where pitch = 0 - uses linear interpolation
    pitch_low  = -min(abs(dat_curr$m_comp[which(dat_curr$m_comp < 0)]))
    pitch_high = min(abs(dat_curr$m_comp[which(dat_curr$m_comp > 0)]))
    dat_stab_exp$alpha_pitch0[count]<- (pitch_low*(dat_curr$alpha[which(dat_curr$m_comp == pitch_high)]-dat_curr$alpha[which(dat_curr$m_comp == pitch_low)])/(pitch_high-pitch_low)) + dat_curr$alpha[which(dat_curr$m_comp == pitch_low)]

    # Maximum lift at the first stall point
    dat_stab_exp$lift_max[count]    <- dat_curr$L_comp[which(dat_curr$alpha == first_stall_alpha[i,j])]

    # Save the maximum lift to drag ratio
    dat_stab_exp$LD_max[count]      <- max(dat_curr$L/dat_curr$D)
    dat_stab_exp$alpha_L_D[count]   <- dat_curr$alpha[which.max(dat_curr$L/dat_curr$D)]
    dat_stab_exp$LD_max_ci[count]   <- abs(dat_stab_exp$LD_max[count])*sqrt((dat_curr$FL_wt_ci[which(dat_curr$alpha == dat_stab_exp$alpha_L_D[count])]/dat_curr$L[which(dat_curr$alpha == dat_stab_exp$alpha_L_D[count])])^2 + (dat_curr$FD_wt_ci[which(dat_curr$alpha == dat_stab_exp$alpha_L_D[count])]/dat_curr$D[which(dat_curr$alpha == dat_stab_exp$alpha_L_D[count])])^2)
    
    # Save the minimum drag
    dat_stab_exp$D_min[count]       <- min(dat_curr$D)
    dat_stab_exp$D_comp_min[count]  <- min(dat_curr$D_comp)
    
    # Save the maximum lift before stall
    dat_stab_exp$L_max[count]       <- dat_curr$L[which(dat_curr$alpha == first_stall_alpha[i,j])]
    dat_stab_exp$L_comp_max[count]  <- dat_curr$L_comp[which(dat_curr$alpha == first_stall_alpha[i,j])]
    # ----------------- Fit models to the linear range --------------------------

    # Define the linear data range
    dat_curr_lin = subset(dat_curr, alpha < first_stall_alpha[i,j]-2 & alpha > dat_stab_exp$alpha_lift0[count]+2)
      if (count == 1){
        dat_lin_exp  = dat_curr_lin
      } else{
        dat_lin_exp  = rbind(dat_lin_exp,dat_curr_lin)
      }

    #fit linear model to Cm vs. CL
    mod.pstab  <- lm(m_comp~L_comp, data = dat_curr_lin)
    test       <- summary(mod.pstab)
    mod.pstaba <- lm(m_comp~alpha, data = dat_curr_lin)
    mod.lift   <- lm(L_comp~alpha, data = dat_curr_lin)
    mod.drag   <- lm(D_comp~poly(CL_avg,2), data = dat_curr) # doesn't need to be determined in the linear region

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

    count = count + 1
  }
}


## ------------------------------------------------------------------
## ------------------- Longitudinal Control -------------------------
## ------------------------------------------------------------------

mod_con_cL_exp = lm(L_comp ~ elbow + manus + alpha, data = subset(dat_exp, U <14))
mod_con_cm_exp = lm(m_comp ~ elbow + manus + alpha, data = subset(dat_exp, U <14))
mod_con_cd_exp = lm(D_comp ~ elbow + manus + poly(alpha,2), data = subset(dat_exp, U <14))

mod_cmcl_exp = lm(cmcl ~ elbow + manus, data = subset(dat_stab_exp, U < 14))
mod_cm0_exp  = lm(cm0 ~ elbow + manus, data = subset(dat_stab_exp, U < 14))
