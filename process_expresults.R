

## ---------------- Load libraries ----------------
library(R.matlab)
library(pracma)
library(quantities)

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
ATI_cal <- lapply(ATI_cal, function(x) set_errors(x, x*0))

PT_M         <- 497.68/5
errors(PT_M) <- 0
no_samples   <- 1800000

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

# - Correlation between static variables
correl(PT_V_off,LC1_off)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,1]) # PT and lc channel 1
correl(PT_V_off,LC2_off)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,2]) # PT and lc channel 2
correl(PT_V_off,LC3_off)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,3]) # PT and lc channel 3
correl(PT_V_off,LC4_off)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,4]) # PT and lc channel 4
correl(PT_V_off,LC5_off)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,5]) # PT and lc channel 5
correl(PT_V_off,LC6_off)  <- cor(dat_static$data.P.V[,1],dat_static$data.LC.V[,6]) # PT and lc channel 6

correl(LC1_off,LC2_off)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,2]) # lc channel 1 and lc channel 2
correl(LC1_off,LC3_off)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,3]) # lc channel 1 and lc channel 3
correl(LC1_off,LC4_off)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,4]) # lc channel 1 and lc channel 4
correl(LC1_off,LC5_off)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,5]) # lc channel 1 and lc channel 5
correl(LC1_off,LC6_off)   <- cor(dat_static$data.LC.V[,1],dat_static$data.LC.V[,6]) # lc channel 1 and lc channel 6

correl(LC2_off,LC3_off)   <- cor(dat_static$data.LC.V[,2],dat_static$data.LC.V[,3]) # lc channel 2 and lc channel 3
correl(LC2_off,LC4_off)   <- cor(dat_static$data.LC.V[,2],dat_static$data.LC.V[,4]) # lc channel 2 and lc channel 4
correl(LC2_off,LC5_off)   <- cor(dat_static$data.LC.V[,2],dat_static$data.LC.V[,5]) # lc channel 2 and lc channel 5
correl(LC2_off,LC6_off)   <- cor(dat_static$data.LC.V[,2],dat_static$data.LC.V[,6]) # lc channel 2 and lc channel 6

correl(LC3_off,LC4_off)   <- cor(dat_static$data.LC.V[,3],dat_static$data.LC.V[,4]) # lc channel 3 and lc channel 4
correl(LC3_off,LC5_off)   <- cor(dat_static$data.LC.V[,3],dat_static$data.LC.V[,5]) # lc channel 3 and lc channel 5
correl(LC3_off,LC6_off)   <- cor(dat_static$data.LC.V[,3],dat_static$data.LC.V[,6]) # lc channel 3 and lc channel 6

correl(LC4_off,LC5_off)   <- cor(dat_static$data.LC.V[,4],dat_static$data.LC.V[,5]) # lc channel 4 and lc channel 5
correl(LC4_off,LC6_off)   <- cor(dat_static$data.LC.V[,4],dat_static$data.LC.V[,6]) # lc channel 4 and lc channel 5

correl(LC5_off,LC6_off)   <- cor(dat_static$data.LC.V[,5],dat_static$data.LC.V[,6]) # lc channel 5 and lc channel 6

# ------------- Dynamic variables with the tunnel on -------------
# prep all measurands with errors and correlations built in
PT_V                  <- dat_raw$PT_V
errors(PT_V)          <- dat_raw$PT_V_std             # Type A uncertainty

P_atm                 <- dat_raw$PT_V
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

correl(PT_V, LC1) <- dat_raw$cor_PT_LC1
correl(PT_V, LC2) <- dat_raw$cor_PT_LC2
correl(PT_V, LC3) <- dat_raw$cor_PT_LC3
correl(PT_V, LC4) <- dat_raw$cor_PT_LC4
correl(PT_V, LC5) <- dat_raw$cor_PT_LC5
correl(PT_V, LC6) <- dat_raw$cor_PT_LC6

correl(LC1, LC2)  <- dat_raw$cor_LC1_LC2
correl(LC1, LC3)  <- dat_raw$cor_LC1_LC3
correl(LC1, LC4)  <- dat_raw$cor_LC1_LC4
correl(LC1, LC5)  <- dat_raw$cor_LC1_LC5
correl(LC1, LC6)  <- dat_raw$cor_LC1_LC6

correl(LC2, LC3)  <- dat_raw$cor_LC2_LC3
correl(LC2, LC4)  <- dat_raw$cor_LC2_LC4
correl(LC2, LC5)  <- dat_raw$cor_LC2_LC5
correl(LC2, LC6)  <- dat_raw$cor_LC2_LC6

correl(LC3, LC4)  <- dat_raw$cor_LC3_LC4
correl(LC3, LC5)  <- dat_raw$cor_LC3_LC5
correl(LC3, LC6)  <- dat_raw$cor_LC3_LC6

correl(LC4, LC5)  <- dat_raw$cor_LC4_LC5
correl(LC4, LC6)  <- dat_raw$cor_LC4_LC6

correl(LC5, LC6)  <- dat_raw$cor_LC5_LC6

## --------- Iterate through each entry ---------------

for (i in 1:length(LC1)){
  alpha = dat_raw$alpha
  # -------------- Calculate the velocity in the test ----------------
  fluidprop = calc_fluid_prop(P_atm[i],T_atm[i])
  res_vel   = calc_U(fluidprop, M, PT_V[i], PT_V_off[i])
  
  # -------------- Adjust forces appropriately -----------------------
  LC_raw     = c(LC1,LC2,LC3,LC4,LC5,LC6) - c(LC1_off,LC2_off,LC3_off,LC4_off,LC5_off,LC6_off)
  loads_LC   = ATI_cal*LC_raw
  rotation   = rotz(alpha);
  forces_WT  = rotz(alpha)*loads_LC[1:3]
  moments_WT = rotz(alpha)*loads_LC[4:6]
}

