
## ---------------------------- Model Selection for Numerical results --------------------------


# --- Lift model fit ---
possible_models <- matrix(nrow=1, ncol = 47)
base_int        = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred     = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + (1|WingID), data = subset(dat_num, alpha < 5))
base_add        = lmer(CL_adj ~ elbow_scale+manus_scale+alpha_scale + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a2     = lmer(CL_adj ~ elbow_scale+manus_scale+alpha_scale + I(alpha_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a2e2   = lmer(CL_adj ~ elbow_scale+manus_scale+alpha_scale + I(alpha_scale^2)+ I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a2e2m2 = lmer(CL_adj ~ elbow_scale+manus_scale+alpha_scale + I(alpha_scale^2)+ I(elbow_scale^2)+ I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a3     = lmer(CL_adj ~ elbow_scale+manus_scale+alpha_scale + I(alpha_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a3e3   = lmer(CL_adj ~ elbow_scale+manus_scale+alpha_scale + I(alpha_scale^2)+ I(elbow_scale^2)+ I(alpha_scale^3)+ I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a3e2m3 = lmer(CL_adj ~ elbow_scale+manus_scale+alpha_scale + I(alpha_scale^2)+ I(elbow_scale^2)+ I(manus_scale^2)+ I(alpha_scale^3)+ I(elbow_scale^3)+ I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int with first order alpha_scale 
base_int_a1e2m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a1e2m3 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a1e3m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(elbow_scale^2) + I(manus_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a1e1m3 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a1e3m1 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a1e1m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a1e2m1 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int with square alpha_scale 
base_int_a2e2m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e2m3 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e3m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e1m1 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e1m3 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e3m1 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e1m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e2m1 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int with cubic alpha_scale 
base_int_a3e2m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e2m3 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e3m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e1m1 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e1m3 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e3m1 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(alpha_scale^3) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e1m2 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e2m1 = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int reduced with first order alpha_scale 
base_intred_a1e2m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e2m3 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e3m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(elbow_scale^2) + I(manus_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e1m3 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e3m1 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e1m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e2m1 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int reduced with square alpha_scale 
base_intred_a2e2m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e2m3 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e3m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e1m1 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e1m3 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e3m1 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e1m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e2m1 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
# all intred reduced with cubic alpha_scale 
base_intred_a3e2m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e2m3 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e3m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e1m1 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e1m3 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e3m1 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(alpha_scale^3) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e1m2 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(manus_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e2m1 = lmer(CL_adj ~ elbow_scale*manus_scale+alpha_scale + I(alpha_scale^2) + I(elbow_scale^2) + I(alpha_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))

possible_models <- c(AIC(base_int),AIC(base_intred),AIC(base_add),
                     AIC(base_add_a2),AIC(base_add_a2e2),AIC(base_add_a2e2m2),
                     AIC(base_add_a3),AIC(base_add_a3e3),AIC(base_add_a3e2m3),
                     #first order alpha_scale models
                     AIC(base_int_a1e2m2),AIC(base_int_a1e2m3),AIC(base_int_a1e3m2),
                     AIC(base_int_a1e1m3),AIC(base_int_a1e3m1),
                     AIC(base_int_a1e1m2),AIC(base_int_a1e2m1),
                     #square models
                     AIC(base_int_a2e2m2),AIC(base_int_a2e2m3),AIC(base_int_a2e3m2),
                     AIC(base_int_a2e1m1),AIC(base_int_a2e1m3),AIC(base_int_a2e3m1),
                     AIC(base_int_a2e1m2),AIC(base_int_a2e2m1),
                     #cubic models
                     AIC(base_int_a3e2m2),AIC(base_int_a3e2m3),AIC(base_int_a3e3m2),
                     AIC(base_int_a3e1m1),AIC(base_int_a3e1m3),AIC(base_int_a3e3m1),
                     AIC(base_int_a3e1m2),AIC(base_int_a3e2m1),
                     #first order alpha_scale models
                     AIC(base_intred_a1e2m2),AIC(base_intred_a1e2m3),AIC(base_intred_a1e3m2),
                     AIC(base_intred_a1e1m3),AIC(base_intred_a1e3m1),
                     AIC(base_intred_a1e1m2),AIC(base_intred_a1e2m1),
                     #square models reduced
                     AIC(base_intred_a2e2m2),AIC(base_intred_a2e2m3),AIC(base_intred_a2e3m2),
                     AIC(base_intred_a2e1m1),AIC(base_intred_a2e1m3),AIC(base_intred_a2e3m1),
                     AIC(base_intred_a2e1m2),AIC(base_intred_a2e2m1),
                     #cubic models reduced
                     AIC(base_intred_a3e2m2),AIC(base_intred_a3e2m3),AIC(base_intred_a3e3m2),
                     AIC(base_intred_a3e1m1),AIC(base_intred_a3e1m3),AIC(base_intred_a3e3m1),
                     AIC(base_intred_a3e1m2),AIC(base_intred_a3e2m1))

delta <- possible_models - min(possible_models)
L <- exp(-0.5 * delta)            # relative likelihoods of models
w <- L/sum(L)                     # Akaike weights
which.max(w)


# --- Pitch model fit ---
possible_models <- matrix(nrow=1, ncol = 47)
base_int        = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred     = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + (1|WingID), data = subset(dat_num, alpha < 5))
base_add        = lmer(Cm_adj ~ elbow_scale+manus_scale+CL_adj + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a2     = lmer(Cm_adj ~ elbow_scale+manus_scale+CL_adj + I(CL_adj^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a2e2   = lmer(Cm_adj ~ elbow_scale+manus_scale+CL_adj + I(CL_adj^2)+ I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a2e2m2 = lmer(Cm_adj ~ elbow_scale+manus_scale+CL_adj + I(CL_adj^2)+ I(elbow_scale^2)+ I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a3     = lmer(Cm_adj ~ elbow_scale+manus_scale+CL_adj + I(CL_adj^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a3e3   = lmer(Cm_adj ~ elbow_scale+manus_scale+CL_adj + I(CL_adj^2)+ I(elbow_scale^2)+ I(CL_adj^3)+ I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_add_a3e2m3 = lmer(Cm_adj ~ elbow_scale+manus_scale+CL_adj + I(CL_adj^2)+ I(elbow_scale^2)+ I(manus_scale^2)+ I(CL_adj^3)+ I(elbow_scale^3)+ I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int reduced with first order CL_adj 
base_intred_a1e2m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e2m3 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e3m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + I(manus_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e1m3 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e3m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e1m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e2m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int with square CL_adj 
base_int_a2e2m2 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e2m3 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e3m2 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e1m1 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e1m3 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e3m1 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e1m2 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a2e2m1 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int with cubic CL_adj 
base_int_a3e2m2 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e2m3 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(CL_adj^3) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e3m2 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(CL_adj^3) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e1m1 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e1m3 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(manus_scale^2) + I(CL_adj^3) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e3m1 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(CL_adj^3) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e1m2 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(manus_scale^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_int_a3e2m1 = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int reduced with first order CL_adj 
base_intred_a1e2m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e2m3 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e3m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + I(manus_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e1m3 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e3m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e1m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a1e2m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
# all int reduced with square CL_adj 
base_intred_a2e2m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e2m3 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e3m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e1m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e1m3 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e3m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e1m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(manus_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a2e2m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + (1|WingID), data = subset(dat_num, alpha < 5))
# all intred reduced with cubic CL_adj 
base_intred_a3e2m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e2m3 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(CL_adj^3) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e3m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(manus_scale^2) + I(CL_adj^3) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e1m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e1m3 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(manus_scale^2) + I(CL_adj^3) + I(manus_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e3m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(CL_adj^3) + I(elbow_scale^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e1m2 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(manus_scale^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))
base_intred_a3e2m1 = lmer(Cm_adj ~ elbow_scale*manus_scale+CL_adj + I(CL_adj^2) + I(elbow_scale^2) + I(CL_adj^3) + (1|WingID), data = subset(dat_num, alpha < 5))

possible_models <- c(AIC(base_int),AIC(base_intred),AIC(base_add),
                     AIC(base_add_a2),AIC(base_add_a2e2),AIC(base_add_a2e2m2),
                     AIC(base_add_a3),AIC(base_add_a3e3),AIC(base_add_a3e2m3),
                     #first order alpha_scale models
                     AIC(base_int_a1e2m2),AIC(base_int_a1e2m3),AIC(base_int_a1e3m2),
                     AIC(base_int_a1e1m3),AIC(base_int_a1e3m1),
                     AIC(base_int_a1e1m2),AIC(base_int_a1e2m1),
                     #square models
                     AIC(base_int_a2e2m2),AIC(base_int_a2e2m3),AIC(base_int_a2e3m2),
                     AIC(base_int_a2e1m1),AIC(base_int_a2e1m3),AIC(base_int_a2e3m1),
                     AIC(base_int_a2e1m2),AIC(base_int_a2e2m1),
                     #cubic models
                     AIC(base_int_a3e2m2),AIC(base_int_a3e2m3),AIC(base_int_a3e3m2),
                     AIC(base_int_a3e1m1),AIC(base_int_a3e1m3),AIC(base_int_a3e3m1),
                     AIC(base_int_a3e1m2),AIC(base_int_a3e2m1),
                     #first order alpha_scale models
                     AIC(base_intred_a1e2m2),AIC(base_intred_a1e2m3),AIC(base_intred_a1e3m2),
                     AIC(base_intred_a1e1m3),AIC(base_intred_a1e3m1),
                     AIC(base_intred_a1e1m2),AIC(base_intred_a1e2m1),
                     #square models reduced
                     AIC(base_intred_a2e2m2),AIC(base_intred_a2e2m3),AIC(base_intred_a2e3m2),
                     AIC(base_intred_a2e1m1),AIC(base_intred_a2e1m3),AIC(base_intred_a2e3m1),
                     AIC(base_intred_a2e1m2),AIC(base_intred_a2e2m1),
                     #cubic models reduced
                     AIC(base_intred_a3e2m2),AIC(base_intred_a3e2m3),AIC(base_intred_a3e3m2),
                     AIC(base_intred_a3e1m1),AIC(base_intred_a3e1m3),AIC(base_intred_a3e3m1),
                     AIC(base_intred_a3e1m2),AIC(base_intred_a3e2m1))

delta <- possible_models - min(possible_models)
L <- exp(-0.5 * delta)            # relative likelihoods of models
w <- L/sum(L)                     # Akaike weights
which.max(w)

### ------------- dCm/dCL fit ------------------
possible_models <- matrix(nrow=1, ncol = 18)
base_int      = lmer(cmcl ~ elbow_scale*manus_scale + (1|WingID), data = dat_stab)
base_add      = lmer(cmcl ~ elbow_scale+manus_scale + (1|WingID), data = dat_stab)
base_add_e2   = lmer(cmcl ~ elbow_scale+manus_scale + I(elbow_scale^2) + (1|WingID), data = dat_stab)
base_add_m2   = lmer(cmcl ~ elbow_scale+manus_scale + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_add_e2m2 = lmer(cmcl ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_add_e3   = lmer(cmcl ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_add_m3   = lmer(cmcl ~ elbow_scale+manus_scale + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_add_e2m3 = lmer(cmcl ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_add_e3m2 = lmer(cmcl ~ elbow_scale+manus_scale + I(manus_scale^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_add_e3m3 = lmer(cmcl ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e2   = lmer(cmcl ~ elbow_scale*manus_scale + I(elbow_scale^2) + (1|WingID), data = dat_stab)
base_int_m2   = lmer(cmcl ~ elbow_scale*manus_scale + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_int_e2m2 = lmer(cmcl ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_int_e3   = lmer(cmcl ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_int_m3   = lmer(cmcl ~ elbow_scale*manus_scale + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e2m3 = lmer(cmcl ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e3m2 = lmer(cmcl ~ elbow_scale*manus_scale + I(manus_scale^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_int_e3m3 = lmer(cmcl ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
possible_models <- c(AIC(base_int),AIC(base_add),
                     AIC(base_add_e2),AIC(base_add_m2),AIC(base_add_e2m2),
                     AIC(base_add_e3),AIC(base_add_m3),AIC(base_add_e2m3),AIC(base_add_e3m2),AIC(base_add_e3m3),
                     AIC(base_int_e2),AIC(base_int_m2),AIC(base_int_e2m2),
                     AIC(base_int_e3),AIC(base_int_m3),AIC(base_int_e2m3),AIC(base_int_e3m2),AIC(base_int_e3m3))

delta <- possible_models - min(possible_models)
L <- exp(-0.5 * delta)            # relative likelihoods of models
w <- L/sum(L)                     # Akaike weights
which.max(w)

### ------------- Cm0 fit ------------------
possible_models <- matrix(nrow=1, ncol = 18)
base_int      = lmer(cm0 ~ elbow_scale*manus_scale + (1|WingID), data = dat_stab)
base_add      = lmer(cm0 ~ elbow_scale+manus_scale + (1|WingID), data = dat_stab)
base_add_e2   = lmer(cm0 ~ elbow_scale+manus_scale + I(elbow_scale^2) + (1|WingID), data = dat_stab)
base_add_m2   = lmer(cm0 ~ elbow_scale+manus_scale + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_add_e2m2 = lmer(cm0 ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_add_e3   = lmer(cm0 ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_add_m3   = lmer(cm0 ~ elbow_scale+manus_scale + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_add_e2m3 = lmer(cm0 ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_add_e3m2 = lmer(cm0 ~ elbow_scale+manus_scale + I(manus_scale^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_add_e3m3 = lmer(cm0 ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e2   = lmer(cm0 ~ elbow_scale*manus_scale + I(elbow_scale^2) + (1|WingID), data = dat_stab)
base_int_m2   = lmer(cm0 ~ elbow_scale*manus_scale + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_int_e2m2 = lmer(cm0 ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_int_e3   = lmer(cm0 ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_int_m3   = lmer(cm0 ~ elbow_scale*manus_scale + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e2m3 = lmer(cm0 ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e3m2 = lmer(cm0 ~ elbow_scale*manus_scale + I(manus_scale^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_int_e3m3 = lmer(cm0 ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
possible_models <- c(AIC(base_int),AIC(base_add),
                     AIC(base_add_e2),AIC(base_add_m2),AIC(base_add_e2m2),
                     AIC(base_add_e3),AIC(base_add_m3),AIC(base_add_e2m3),AIC(base_add_e3m2),AIC(base_add_e3m3),
                     AIC(base_int_e2),AIC(base_int_m2),AIC(base_int_e2m2),
                     AIC(base_int_e3),AIC(base_int_m3),AIC(base_int_e2m3),AIC(base_int_e3m2),AIC(base_int_e3m3))

delta <- possible_models - min(possible_models)
L <- exp(-0.5 * delta)            # relative likelihoods of models
w <- L/sum(L)                     # Akaike weights
which.max(w)



### ------------- dihedral fit ------------------
possible_models <- matrix(nrow=1, ncol = 18)
base_int      = lmer(dihedral ~ elbow_scale*manus_scale + (1|WingID), data = dat_stab)
base_add      = lmer(dihedral ~ elbow_scale+manus_scale + (1|WingID), data = dat_stab)
base_add_e2   = lmer(dihedral ~ elbow_scale+manus_scale + I(elbow_scale^2) + (1|WingID), data = dat_stab)
base_add_m2   = lmer(dihedral ~ elbow_scale+manus_scale + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_add_e2m2 = lmer(dihedral ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_add_e3   = lmer(dihedral ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_add_m3   = lmer(dihedral ~ elbow_scale+manus_scale + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_add_e2m3 = lmer(dihedral ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_add_e3m2 = lmer(dihedral ~ elbow_scale+manus_scale + I(manus_scale^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_add_e3m3 = lmer(dihedral ~ elbow_scale+manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e2   = lmer(dihedral ~ elbow_scale*manus_scale + I(elbow_scale^2) + (1|WingID), data = dat_stab)
base_int_m2   = lmer(dihedral ~ elbow_scale*manus_scale + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_int_e2m2 = lmer(dihedral ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(manus_scale^2) + (1|WingID), data = dat_stab)
base_int_e3   = lmer(dihedral ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_int_m3   = lmer(dihedral ~ elbow_scale*manus_scale + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e2m3 = lmer(dihedral ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
base_int_e3m2 = lmer(dihedral ~ elbow_scale*manus_scale + I(manus_scale^2) + I(elbow_scale^2) + I(elbow_scale^3) + (1|WingID), data = dat_stab)
base_int_e3m3 = lmer(dihedral ~ elbow_scale*manus_scale + I(elbow_scale^2) + I(elbow_scale^3) + I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
possible_models <- c(AIC(base_int),AIC(base_add),
                     AIC(base_add_e2),AIC(base_add_m2),AIC(base_add_e2m2),
                     AIC(base_add_e3),AIC(base_add_m3),AIC(base_add_e2m3),AIC(base_add_e3m2),AIC(base_add_e3m3),
                     AIC(base_int_e2),AIC(base_int_m2),AIC(base_int_e2m2),
                     AIC(base_int_e3),AIC(base_int_m3),AIC(base_int_e2m3),AIC(base_int_e3m2),AIC(base_int_e3m3))

delta <- possible_models - min(possible_models)
L <- exp(-0.5 * delta)            # relative likelihoods of models
w <- L/sum(L)                     # Akaike weights
which.max(w)
