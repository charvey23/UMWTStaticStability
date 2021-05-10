## -------- Analyze wind tunnel data ----------

## ---------------- Load libraries ----------------

## This code reads in the output from the wind tunnel data on the 9 3D printed gull wings to extrapolate the key parameters
setwd("/Users/christinaharvey/Documents/UMWTStaticStability/Data") 

dat_raw          <- read.csv('2020_11_30_ProcessedData.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_wing         <- read.csv('2020_08_26_selectedwtwings.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_wing$FrameID <- paste("F", dat_wing$FrameID, sep = "")
dat_raw$alpha    <- as.numeric(dat_raw$alpha)
dat_raw$U        <- as.numeric(dat_raw$U)
dat_exp          <- merge(dat_raw,dat_wing, by ="FrameID")
remove(dat_raw)

# pre-define the matrix
dat_stab_exp  <- data.frame(matrix(nrow = 18, ncol = 3))
names(dat_stab_exp) <- c("FrameID","elbow","manus")
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
                          c(12,20)) # F4546

## ------------------------------------------------------------------
## ------------------ Longitudinal Stability ------------------------
## ------------------------------------------------------------------

# Loop through every wing configuration and test to save key results
for (i in 1:nrow(dat_wing)){
  for (j in 1:2){

    curr_FrameID = dat_wing$FrameID[i]

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
    dat_curr_lin = subset(dat_curr, alpha < first_stall_alpha[i,j]-1 & alpha > dat_stab_exp$alpha_lift0[count])
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
    mod.drag   <- lm(D_comp~poly(L_comp,2), data = dat_curr) # doesn't need to be determined in the linear region

    # save all info about the linear model
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
remove(dat_curr, mod.pstab, mod.pstaba, mod.lift, mod.drag, test, first_stall_alpha)

## -------------------------------------------------------------------------------------
## ------------------- Compare numerical and experimental data -------------------------
## -------------------------------------------------------------------------------------

# Change the comparable values to the true morphing coefficients 
# L_comp and m_comp from the previous outputs were already divided by dynamic pressure
# CAUTION: halve the wing area because we are looking at results from a half wing only in this section
dat_num$L_comp <- dat_num$L_comp/(0.5*dat_num$S_max) 
dat_num$m_comp <- dat_num$m_comp/(0.5*dat_num$S_max*dat_num$c_max)

dat_exp$L_comp     <- dat_exp$L_comp/(0.5*max(dat_num$S[which(dat_num$WingID == "17_0285")]))
dat_exp$L_comp_std <- dat_exp$L_comp_std/(0.5*max(dat_num$S[which(dat_num$WingID == "17_0285")]))
dat_exp$m_comp     <- dat_exp$m_comp/(0.5*max(dat_num$S[which(dat_num$WingID == "17_0285")])*max(dat_num$ref_c[which(dat_num$WingID == "17_0285")]))
dat_exp$m_comp_std <- dat_exp$m_comp_std/(0.5*max(dat_num$S[which(dat_num$WingID == "17_0285")])*max(dat_num$ref_c[which(dat_num$WingID == "17_0285")]))

# investigate comparable
dat_num_simp        <- subset(dat_num, FrameID %in% wtwings & WingID == "17_0285")[,c("L_comp","alpha","FrameID","m_comp","elbow","manus")]
dat_num_simp$method <- "n"
dat_exp_simp        <- subset(dat_exp, U < 14 & alpha <= 10 & alpha >= -10)[,c("L_comp","alpha","FrameID","m_comp","elbow","manus")]
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

dat_num_simp$L_error <- NA
dat_num_simp$m_error <- NA
for (i in 1:length(dat_num_simp$FrameID)){
  if (nrow(subset(dat_exp_simp, FrameID == dat_num_simp$FrameID[i] & alpha == dat_num_simp$alpha[i]))==0){
    next
  }
  dat_num_simp$L_error[i] <- min(abs(subset(dat_num_simp, FrameID == dat_num_simp$FrameID[i] & alpha == dat_num_simp$alpha[i])$L_comp - mean(subset(dat_exp_simp, FrameID == dat_num_simp$FrameID[i] & alpha == dat_num_simp$alpha[i])$L_comp)))
  dat_num_simp$m_error[i] <- abs(subset(dat_num_simp, FrameID == dat_num_simp$FrameID[i] & alpha == dat_num_simp$alpha[i])$m_comp - mean(subset(dat_exp_simp, FrameID == dat_num_simp$FrameID[i] & alpha == dat_num_simp$alpha[i])$m_comp))
}

## ------------------------------------------------------
## ---- Extract numbers referenced within the paper -----
## ------------------------------------------------------

## Experimental Re number
min(dat_exp$Re_c[which(dat_exp$U < 14)])*(0.2201308*0.8)
max(dat_exp$Re_c[which(dat_exp$U < 14)])*(0.2201308*0.8)
## Numerical Re number
(1.225*10/(1.81*10^-5))*0.2201308

# output the maximum error for each angle of attack
aggregate(dat_num_simp$L_error, list(dat_num_simp$alpha), mean)
aggregate(dat_num_simp$m_error, list(dat_num_simp$alpha), mean)

# select the angle of attack that will be used in the predictions
alpha_select <- 0

## To check that the ranges remain constant 
max(subset(dat_num, alpha == 0)$L_comp)-min(subset(dat_num, alpha == 0)$L_comp)
max(subset(dat_num, alpha == 0)$m_comp)-min(subset(dat_num, alpha == 0)$m_comp)

aggregate(dat_exp$L_comp, list(dat_exp$alpha), function(x){max(x)-min(x)})
aggregate(dat_exp$m_comp, list(dat_exp$alpha), function(x){max(x)-min(x)})

###------------------------------------------------------------------------------------------------------------------------------
###------------------------------------------------------------------------------------------------------------------------------
## --------------------------------------------- COMPARE TO TAXIDERMIED WINGS --------------------------------------------------- 
###------------------------------------------------------------------------------------------------------------------------------
###------------------------------------------------------------------------------------------------------------------------------

# can't really be done since I don't now the maximum available chord on each of the posed wings to non-dimensionalize
test <- subset(fit_results, Grid == "NoGrid")
test <- merge(test,subset(pc_results, dataset %in% "windtunnel" & orientation %in% "hand"), by.x = c("WingID"), by.y = c("ID"))
test <- merge(test,subset(gullinfo[,c("WingID","Grid","Root.Chord")], Grid %in% "NoGrid"), by.x = c("WingID"), by.y = c("WingID"))
test_curr <- merge(dat_stab_exp,dat_all[which(dat_all$WingID == "17_0285"),c("FrameID","c_root")], by = c("FrameID"))
                   
clean_test <- test[,c("elbow.angle","manus.angle","Root.Chord")]
clean_test$elbow_scale <- clean_test$elbow.angle/1000
clean_test$manus_scale <- clean_test$manus.angle/1000
clean_test$pred_cmcl   <- predict(mod_cmcl_num, newdata = clean_test, re.form = ~0)
clean_test$pred_cmcl   <- clean_test$pred_cmcl*(clean_test$Root.Chord/100) # make the most comparable to the wind tunnel results

plot(test$Angle.True,test$slopemean, col = "red")         # prepared wing results
plot(clean_test$elbow.angle,clean_test$pred_cmcl)       # predicted results from our model
plot(test_curr$elbow,test_curr$cmcl*test_curr$c_root, col = "blue") 

plot(dat$Lift[which(dat$Angle.True > 80)],dat$Pitch[which(dat$Angle.True > 80)])
points(dat_exp$L[which(dat_exp$manus > 150)],dat_exp$m[which(dat_exp$manus > 150)], col = "red")
