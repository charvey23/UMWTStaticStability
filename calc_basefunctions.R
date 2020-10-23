calc_fluid_prop <- function(P_atm,T_atm_K){
  R_air = 286.9;          # (J/kg K)
  C_air = 1.458e-6;       # Sutherland's curve fit constant
  S_air = 110.4;          # Sutherland's curve fit constant
  T_atm_K = T_atm + 273.15; # Kelvin conversion
  
  rho_air     = (P_atm) / (R_air * T_atm_K);               # (kg/m^3)
  visc_air    = C_air * T_atm_K^(1.5) / (S_air + T_atm_K); # (kg/m-s) Dynamic viscosity 
}