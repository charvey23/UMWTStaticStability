## ----------------- Calculate the STD from a time series ----------------

timeseries_std <- function(timeseries,no_samples){
  # equations refer to paper: Zieba, A. (2010). Effective number of observations
  # and unbiased estimators of variance for autocorrelated data-an overview.
  # Metrol. Meas. Syst, 17(1), 3-16.
  # URL: http://www.metrology.pg.gda.pl/full/2010/M&MS_2010_003.pdf

  data_std_sample = sd(timeseries)
  # choose the number of lags to be the entire sample-1 
  no_lags    <- no_samples - 1
  
  # compute autocorrelation of the timeseries but do not plot, for no window must select rectangular
  autocor    <- fastacf(timeseries, lag.max = no_lags, show = FALSE, window = "rectangular")
  
  sum_1 = 0
  sum_2 = 0
  for (j in 1:no_lags){
    sum_1 = sum_1 + autocor$acf[j]*((no_samples-j)/no_samples) # Summation in Eqn. 12
    sum_2 = sum_2 + autocor$acf[j]^2 # Summation in Eqn. 30
  }
  
  # effective independent samples in the data Eqn. 12
  no_indsample          = no_samples/(1+2*sum_1)
  # effective degrees of freedom in the entire sample Eqn. 30
  no_dof                = (no_samples/(1+2*sum_2))-1
  # correct the standard deviation to be unbiased for time series Eqn. 24b
  correction            = no_indsample*(no_samples-1)/(no_samples*(no_indsample-1))
  data_std_true         = sqrt(correction*data_std_sample^2)
  results               = list()
  results$data_std_true = data_std_true
  results$no_dof        = no_dof
  
  return(results)
}



## ----------------- Calculate the fluid properties in the flow ----------------

calc_fluid_prop <- function(P_atm,T_atm){
  R_air = 286.9;          # (J/kg K)
  C_air = 1.458e-6;       # Sutherland's curve fit constant
  S_air = 110.4;          # Sutherland's curve fit constant
  T_atm_K = T_atm + 273.15; # Kelvin conversion
  
  fluidprop          = list() 
  fluidprop$rho_air  = (P_atm) / (R_air * T_atm_K)               # (kg/m^3)
  fluidprop$visc_air = C_air * T_atm_K^(1.5) / (S_air + T_atm_K) # (kg/m-s) Dynamic viscosity 
  
  return(fluidprop)
}

## ----------------- Calculate the velocity in the flow ----------------

calc_U <- function(fluidprop, M, PT_V_avg, offset){
  
  velocityprop = list() 
  
  PT_avg            = M*(PT_V_avg-offset)                    # calculate the dynamic pressure
  velocityprop$U    = sqrt(PT_avg*2/fluidprop$rho_air)       # calculate the velocity  
  velocityprop$Re_c = fluidprop$rho_air*velocityprop$U/fluidprop$visc_air # calculate the Reynolds number/chord

  return(velocityprop)
}


## ----------------- Rotation matrix about z -------------------

rotz <- function(angle){
  R = matrix(0, nrow = 3, ncol = 3)
  R[1,1] =  cosd(angle)
  R[1,2] = -sind(angle)
  R[2,1] =  sind(angle)
  R[2,2] =  cosd(angle)
  R[3,3] = 1
  # set the error to 0
  errors(R) <- 0
  return(R)
}


## ------------------ Calculate the matrix*vector ------------
# only meant to work for square matrices
# inputs should both already have uncertainties assigned
mult_matvec_err <- function(mat,vec){
  dimen <- length(vec) # dimension of the vector
  vec_out = vec        # initialize
  for (i in 1:dimen){
    tmp = 0
    for (j in 1:dimen){
      tmp = tmp + mat[i,j]*vec[j]
    }
    vec_out[i] = tmp
  }
  return(vec_out)
}
