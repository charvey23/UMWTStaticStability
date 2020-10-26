

## ---------------- Load libraries ----------------
library(R.matlab)
library(pracma)
library(quantities)
setwd("/Users/Inman PC/Documents/UMWTStaticStability/Data") #For Windows

## ---------------- Load data ----------------
dat_raw    <- read.csv("2020_10_23_ConcatenatedRawData.csv", stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_static <- readMat("WingIDF2195_U0_A0_fulldata.mat")

## --------------- Set the known constants --------------
# error will be set to zero
ATI_cal <-  rbind(c(-0.91164,   0.22078,  -0.71620, -35.41503,   2.10003,  34.48183),     
                 c(1.56291,  39.81764,  -1.03218, -20.15276,  -0.44775, -20.02389),
                 c(59.90570,   0.52238,  59.85577,   0.00933,  60.85201,   0.46774),
                 c(0.03351,   0.48419,  -2.10348,  -0.25082,   2.08893,  -0.21805), 
                 c(2.37399,   0.02293,  -1.18146,   0.42919,  -1.22948,  -0.43109), 
                 c(-0.06047,  -1.31555,  -0.05634,  -1.34543,  -0.03746,  -1.31427))
errors(ATI_cal) <- 0

PT_M         <- 497.68/5
errors(PT_M) <- 0
no_samples   <- 180000

# ------------- Static variables with the tunnel off -------------
# error is being assumed from a previous run of the tunnel off with F2195 installed
PT_V_off         <- dat_raw$PT_V_off
info_PT_V_off    <- timeseries_std(dat_static$data.P.V[,1], no_samples)
errors(PT_V_off) <- info_PT_V_off$data_std_true  # Type B uncertainty - assuming constant for all tests

LC1_off         <- dat_raw$LC_FX_V_off
LC2_off         <- dat_raw$LC_FY_V_off
LC3_off         <- dat_raw$LC_FZ_V_off
LC4_off         <- dat_raw$LC_MX_V_off
LC5_off         <- dat_raw$LC_MY_V_off
LC6_off         <- dat_raw$LC_MZ_V_off
info_LC1_off    <- timeseries_std(dat_static$data.LC.V[,1], no_samples)
errors(LC1_off) <- info_LC1_off$data_std_true # Type B uncertainty - assuming constant for all tests
info_LC2_off    <- timeseries_std(dat_static$data.LC.V[,2], no_samples)
errors(LC2_off) <- info_LC2_off$data_std_true # Type B uncertainty - assuming constant for all tests
info_LC3_off    <- timeseries_std(dat_static$data.LC.V[,3], no_samples)
errors(LC3_off) <- info_LC3_off$data_std_true # Type B uncertainty - assuming constant for all tests
info_LC4_off    <- timeseries_std(dat_static$data.LC.V[,4], no_samples)
errors(LC4_off) <- info_LC4_off$data_std_true # Type B uncertainty - assuming constant for all tests
info_LC5_off    <- timeseries_std(dat_static$data.LC.V[,5], no_samples)
errors(LC5_off) <- info_LC5_off$data_std_true # Type B uncertainty - assuming constant for all tests
info_LC6_off    <- timeseries_std(dat_static$data.LC.V[,6], no_samples)
errors(LC6_off) <- info_LC6_off$data_std_true # Type B uncertainty - assuming constant for all tests

# ------------- Dynamic variables with the tunnel on -------------
# prep all measurands with errors and correlations built in
PT_V                  <- dat_raw$PT_V
errors(PT_V)          <- dat_raw$PT_V_std             # Type A uncertainty

P_atm                 <- dat_raw$P_atm
errors(P_atm)         <- 0.025*3386.39/sqrt(3) # Type B uncertainty - follows GUM 4.3.7 a = half width 

T_atm                 <- dat_raw$T_atm
errors(T_atm)         <- dat_raw$T_atm_std     # Type A uncertainty

LC1         <- dat_raw$LC_FX_V
LC2         <- dat_raw$LC_FY_V
LC3         <- dat_raw$LC_FZ_V
LC4         <- dat_raw$LC_MX_V
LC5         <- dat_raw$LC_MY_V
LC6         <- dat_raw$LC_MZ_V
errors(LC1) <- dat_raw$LC_FX_V_std
errors(LC2) <- dat_raw$LC_FY_V_std
errors(LC3) <- dat_raw$LC_FZ_V_std
errors(LC4) <- dat_raw$LC_MX_V_std
errors(LC5) <- dat_raw$LC_MY_V_std
errors(LC6) <- dat_raw$LC_MZ_V_std

## --------- Iterate through each entry ---------------
dat_process        <- as.data.frame(matrix(nrow = nrow(dat_raw), ncol = 3))
names(dat_process) <- c("FrameID","U_des","alpha")

for (i in 558:length(LC1)){

  alpha = dat_raw$alpha[i]
  errors(alpha) <- 0.025/(200*sqrt(3)) # Type B uncertainty - follows GUM 4.3.7 a = half width 
  # -------------- Define inputs one by one to ensure errors are appropriate -----------------------
  
  # -------------------------------------------------
  # ------------ Dynamic Variables ------------------
  # -------------------------------------------------
  
  LC1_curr   = LC1[i]
  LC2_curr   = LC2[i]
  LC3_curr   = LC3[i]
  LC4_curr   = LC4[i]
  LC5_curr   = LC5[i]
  LC6_curr   = LC6[i]
  PT_V_curr  = PT_V[i]

  correl(PT_V_curr, LC1_curr) <- dat_raw$cor_PT_LC1[i]
  correl(PT_V_curr, LC2_curr) <- dat_raw$cor_PT_LC2[i]
  correl(PT_V_curr, LC3_curr) <- dat_raw$cor_PT_LC3[i]
  correl(PT_V_curr, LC4_curr) <- dat_raw$cor_PT_LC4[i]
  correl(PT_V_curr, LC5_curr) <- dat_raw$cor_PT_LC5[i]
  correl(PT_V_curr, LC6_curr) <- dat_raw$cor_PT_LC6[i]
  
  correl(LC1_curr, LC2_curr)  <- dat_raw$cor_LC1_LC2[i]
  correl(LC1_curr, LC3_curr)  <- dat_raw$cor_LC1_LC3[i]
  correl(LC1_curr, LC4_curr)  <- dat_raw$cor_LC1_LC4[i]
  correl(LC1_curr, LC5_curr)  <- dat_raw$cor_LC1_LC5[i]
  correl(LC1_curr, LC6_curr)  <- dat_raw$cor_LC1_LC6[i]
  
  correl(LC2_curr, LC3_curr)  <- dat_raw$cor_LC2_LC3[i]
  correl(LC2_curr, LC4_curr)  <- dat_raw$cor_LC2_LC4[i]
  correl(LC2_curr, LC5_curr)  <- dat_raw$cor_LC2_LC5[i]
  correl(LC2_curr, LC6_curr)  <- dat_raw$cor_LC2_LC6[i]
  
  correl(LC3_curr, LC4_curr)  <- dat_raw$cor_LC3_LC4[i]
  correl(LC3_curr, LC5_curr)  <- dat_raw$cor_LC3_LC5[i]
  correl(LC3_curr, LC6_curr)  <- dat_raw$cor_LC3_LC6[i]
  
  correl(LC4_curr, LC5_curr)  <- dat_raw$cor_LC4_LC5[i]
  correl(LC4_curr, LC6_curr)  <- dat_raw$cor_LC4_LC6[i]
  
  correl(LC5_curr, LC6_curr)  <- dat_raw$cor_LC5_LC6[i]
  
  # ------------------------------------------------
  # ------------ Static Variables ------------------
  # ------------------------------------------------
  
  LC1_off_curr   = LC1_off[i]
  LC2_off_curr   = LC2_off[i]
  LC3_off_curr   = LC3_off[i]
  LC4_off_curr   = LC4_off[i]
  LC5_off_curr   = LC5_off[i]
  LC6_off_curr   = LC6_off[i]
  PT_V_off_curr  = PT_V_off[i]
  
  # - Correlation between static variables
  correl(PT_V_off_curr,LC1_off_curr)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,1]) # PT and lc channel 1
  correl(PT_V_off_curr,LC2_off_curr)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,2]) # PT and lc channel 2
  correl(PT_V_off_curr,LC3_off_curr)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,3]) # PT and lc channel 3
  correl(PT_V_off_curr,LC4_off_curr)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,4]) # PT and lc channel 4
  correl(PT_V_off_curr,LC5_off_curr)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,5]) # PT and lc channel 5
  correl(PT_V_off_curr,LC6_off_curr)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,6]) # PT and lc channel 6
  
  correl(LC1_off_curr,LC2_off_curr)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,2]) # lc channel 1 and lc channel 2
  correl(LC1_off_curr,LC3_off_curr)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,3]) # lc channel 1 and lc channel 3
  correl(LC1_off_curr,LC4_off_curr)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,4]) # lc channel 1 and lc channel 4
  correl(LC1_off_curr,LC5_off_curr)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,5]) # lc channel 1 and lc channel 5
  correl(LC1_off_curr,LC6_off_curr)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,6]) # lc channel 1 and lc channel 6
  
  correl(LC2_off_curr,LC3_off_curr)   <- cor(dat_static$data.LC.V[,2],dat_static$data.LC.V[,3]) # lc channel 2 and lc channel 3
  correl(LC2_off_curr,LC4_off_curr)   <- cor(dat_static$data.LC.V[,2],dat_static$data.LC.V[,4]) # lc channel 2 and lc channel 4
  correl(LC2_off_curr,LC5_off_curr)   <- cor(dat_static$data.LC.V[,2],dat_static$data.LC.V[,5]) # lc channel 2 and lc channel 5
  correl(LC2_off_curr,LC6_off_curr)   <- cor(dat_static$data.LC.V[,2],dat_static$data.LC.V[,6]) # lc channel 2 and lc channel 6
  
  correl(LC3_off_curr,LC4_off_curr)   <- cor(dat_static$data.LC.V[,3],dat_static$data.LC.V[,4]) # lc channel 3 and lc channel 4
  correl(LC3_off_curr,LC5_off_curr)   <- cor(dat_static$data.LC.V[,3],dat_static$data.LC.V[,5]) # lc channel 3 and lc channel 5
  correl(LC3_off_curr,LC6_off_curr)   <- cor(dat_static$data.LC.V[,3],dat_static$data.LC.V[,6]) # lc channel 3 and lc channel 6
  
  correl(LC4_off_curr,LC5_off_curr)   <- cor(dat_static$data.LC.V[,4],dat_static$data.LC.V[,5]) # lc channel 4 and lc channel 5
  correl(LC4_off_curr,LC6_off_curr)   <- cor(dat_static$data.LC.V[,4],dat_static$data.LC.V[,6]) # lc channel 4 and lc channel 5
  
  correl(LC5_off_curr,LC6_off_curr)   <- cor(dat_static$data.LC.V[,5],dat_static$data.LC.V[,6]) # lc channel 5 and lc channel 6
  
  # -------------- Calculate the velocity in the test ----------------
  fluidprop = calc_fluid_prop(P_atm[i],T_atm[i])
  res_vel   = calc_U(fluidprop, PT_M, PT_V_curr, PT_V_off_curr)

  # -------------- Adjust forces appropriately -----------------------
  LC1_zero   = LC1_curr - LC1_off_curr
  LC2_zero   = LC2_curr - LC2_off_curr
  LC3_zero   = LC3_curr - LC3_off_curr
  LC4_zero   = LC4_curr - LC4_off_curr
  LC5_zero   = LC5_curr - LC5_off_curr
  LC6_zero   = LC6_curr - LC6_off_curr
  # --- Convert with the ATI calibration matrix ------
  # loads_LC   = mult_matvec_err(ATI_cal,LC_raw)
  loads_FX_LC = ATI_cal[1,1]*LC1_zero + ATI_cal[1,2]*LC2_zero + ATI_cal[1,3]*LC3_zero + ATI_cal[1,4]*LC4_zero + ATI_cal[1,5]*LC5_zero + ATI_cal[1,6]*LC6_zero
  loads_FY_LC = ATI_cal[2,1]*LC1_zero + ATI_cal[2,2]*LC2_zero + ATI_cal[2,3]*LC3_zero + ATI_cal[2,4]*LC4_zero + ATI_cal[2,5]*LC5_zero + ATI_cal[2,6]*LC6_zero
  loads_FZ_LC = ATI_cal[3,1]*LC1_zero + ATI_cal[3,2]*LC2_zero + ATI_cal[3,3]*LC3_zero + ATI_cal[3,4]*LC4_zero + ATI_cal[3,5]*LC5_zero + ATI_cal[3,6]*LC6_zero
  loads_MX_LC = ATI_cal[4,1]*LC1_zero + ATI_cal[4,2]*LC2_zero + ATI_cal[4,3]*LC3_zero + ATI_cal[4,4]*LC4_zero + ATI_cal[4,5]*LC5_zero + ATI_cal[4,6]*LC6_zero
  loads_MY_LC = ATI_cal[5,1]*LC1_zero + ATI_cal[5,2]*LC2_zero + ATI_cal[5,3]*LC3_zero + ATI_cal[5,4]*LC4_zero + ATI_cal[5,5]*LC5_zero + ATI_cal[5,6]*LC6_zero
  loads_MZ_LC = ATI_cal[6,1]*LC1_zero + ATI_cal[6,2]*LC2_zero + ATI_cal[6,3]*LC3_zero + ATI_cal[6,4]*LC4_zero + ATI_cal[6,5]*LC5_zero + ATI_cal[6,6]*LC6_zero
  
  # --- Rotate from the LC axis to the wind tunnel axis ------
  #forces_WT  = mult_matvec_err(rotz(alpha),loads_LC[1:3])
  #moments_WT = mult_matvec_err(rotz(alpha),loads_LC[4:6])
  #dim_moments_WT = moments_WT/(0.5*fluidprop$rho_air*res_vel$U^2)
  
  lift      = cosd(alpha)*loads_FX_LC - sind(alpha)*loads_FY_LC
  drag      = sind(alpha)*loads_FX_LC + cosd(alpha)*loads_FY_LC
  sideforce = loads_FZ_LC
  
  yaw_meas  = cosd(alpha)*loads_MX_LC - sind(alpha)*loads_MY_LC
  roll_meas = sind(alpha)*loads_MX_LC + cosd(alpha)*loads_MY_LC
  pitch     = loads_MZ_LC
  # adjust for offset from load cell so that these values are about the shoulder joint
  yaw       = yaw_meas - drag*(2.4*0.0254)
  roll      = roll_meas - lift*(2.4*0.0254)
  
  cL  = lift/(0.5*fluidprop$rho_air*res_vel$U^2)
  cD  = drag/(0.5*fluidprop$rho_air*res_vel$U^2)
  cY  = sideforce/(0.5*fluidprop$rho_air*res_vel$U^2)
  
  cn  = yaw/(0.5*fluidprop$rho_air*res_vel$U^2)
  cl  = roll/(0.5*fluidprop$rho_air*res_vel$U^2)
  cm  = pitch/(0.5*fluidprop$rho_air*res_vel$U^2)
  
  # get to a number that is comparable to the numerical data
  L_comp = lift/(0.5*fluidprop$rho_air*res_vel$U^2*(0.8)^2)
  D_comp = drag/(0.5*fluidprop$rho_air*res_vel$U^2*(0.8)^2)
  m_comp = pitch/(0.5*fluidprop$rho_air*res_vel$U^2*(0.8)^3)
  
  #------------- Save the data ------------
  dat_process$FrameID[i]  = dat_raw$FrameID[i]
  dat_process$U_des[i]    = dat_raw$U_des[i]
  dat_process$alpha[i]    = alpha
  
  dat_process$rho[i]      = fluidprop$rho_air
  dat_process$rho_std[i]  = errors(fluidprop$rho_air)
  dat_process$visc[i]     = fluidprop$visc_air
  dat_process$visc_std[i] = errors(fluidprop$visc_air)
  dat_process$U[i]        = res_vel$U
  dat_process$U_std[i]    = errors(res_vel$U)
  dat_process$Re_c[i]     = res_vel$Re_c
  dat_process$Re_c_std[i] = errors(res_vel$Re_c)
  
  dat_process$L[i]     = lift
  dat_process$L_std[i] = errors(lift)
  dat_process$D[i]     = drag
  dat_process$D_std[i] = errors(drag)
  dat_process$Y[i]     = sideforce
  dat_process$Y_std[i] = errors(sideforce)
  dat_process$n[i]     = yaw
  dat_process$n_std[i] = errors(yaw)
  dat_process$l[i]     = roll
  dat_process$l_std[i] = errors(roll)
  dat_process$m[i]     = pitch
  dat_process$m_std[i] = errors(pitch)
  
  dat_process$CL[i]     = cL
  dat_process$CL_std[i] = errors(cL)
  dat_process$CD[i]     = cD
  dat_process$CD_std[i] = errors(cD)
  dat_process$CY[i]     = cY
  dat_process$CY_std[i] = errors(cY)
  dat_process$Cn[i]     = cn
  dat_process$Cn_std[i] = errors(cn)
  dat_process$Cl[i]     = cl
  dat_process$Cl_std[i] = errors(cl)
  dat_process$Cm[i]     = cm
  dat_process$Cm_std[i] = errors(cm)
  
  dat_process$L_comp[i]     = L_comp
  dat_process$L_comp_std[i] = errors(L_comp)
  dat_process$D_comp[i]     = D_comp
  dat_process$D_comp_std[i] = errors(D_comp)
  dat_process$m_comp[i]     = m_comp
  dat_process$m_comp_std[i] = errors(m_comp)
}

write.csv(dat_process, file="2020_10_26_ProcessedData.csv")
