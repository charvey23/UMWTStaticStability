
## ---------------------------- Model Selection for Numerical results --------------------------


# --- Lift model fit ---
possible_models <- matrix(nrow=1, ncol = 47)
base_int        = lm(CL_adj ~ elbow*manus*alpha, data = subset(dat_num, alpha < 5))
base_intred     = lm(CL_adj ~ elbow*manus+alpha, data = subset(dat_num, alpha < 5))
base_add        = lm(CL_adj ~ elbow+manus+alpha, data = subset(dat_num, alpha < 5))
base_add_a2     = lm(CL_adj ~ elbow+manus+alpha + I(alpha^2), data = subset(dat_num, alpha < 5))
base_add_a2e2   = lm(CL_adj ~ elbow+manus+alpha + I(alpha^2)+ I(elbow^2), data = subset(dat_num, alpha < 5))
base_add_a2e2m2 = lm(CL_adj ~ elbow+manus+alpha + I(alpha^2)+ I(elbow^2)+ I(manus^2), data = subset(dat_num, alpha < 5))
base_add_a3     = lm(CL_adj ~ elbow+manus+alpha + I(alpha^2) + I(alpha^3), data = subset(dat_num, alpha < 5))
base_add_a3e3   = lm(CL_adj ~ elbow+manus+alpha + I(alpha^2)+ I(elbow^2)+ I(alpha^3)+ I(elbow^3), data = subset(dat_num, alpha < 5))
base_add_a3e2m3 = lm(CL_adj ~ elbow+manus+alpha + I(alpha^2)+ I(elbow^2)+ I(manus^2)+ I(alpha^3)+ I(elbow^3)+ I(manus^3), data = subset(dat_num, alpha < 5))
# all int with first order alpha 
base_int_a1e2m2 = lm(CL_adj ~ elbow*manus*alpha + I(elbow^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_int_a1e2m3 = lm(CL_adj ~ elbow*manus*alpha + I(elbow^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a1e3m2 = lm(CL_adj ~ elbow*manus*alpha + I(elbow^2) + I(manus^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a1e1m3 = lm(CL_adj ~ elbow*manus*alpha + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a1e3m1 = lm(CL_adj ~ elbow*manus*alpha + I(elbow^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a1e1m2 = lm(CL_adj ~ elbow*manus*alpha + I(manus^2), data = subset(dat_num, alpha < 5))
base_int_a1e2m1 = lm(CL_adj ~ elbow*manus*alpha + I(elbow^2), data = subset(dat_num, alpha < 5))
# all int with square alpha 
base_int_a2e2m2 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_int_a2e2m3 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a2e3m2 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a2e1m1 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2), data = subset(dat_num, alpha < 5))
base_int_a2e1m3 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a2e3m1 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a2e1m2 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_int_a2e2m1 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2), data = subset(dat_num, alpha < 5))
# all int with cubic alpha 
base_int_a3e2m2 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(alpha^3), data = subset(dat_num, alpha < 5))
base_int_a3e2m3 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(alpha^3) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a3e3m2 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(alpha^3) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a3e1m1 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(alpha^3), data = subset(dat_num, alpha < 5))
base_int_a3e1m3 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(manus^2) + I(alpha^3) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a3e3m1 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(alpha^3) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a3e1m2 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(manus^2) + I(alpha^3), data = subset(dat_num, alpha < 5))
base_int_a3e2m1 = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(elbow^2) + I(alpha^3), data = subset(dat_num, alpha < 5))
# all int reduced with first order alpha 
base_intred_a1e2m2 = lm(CL_adj ~ elbow*manus+alpha + I(elbow^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a1e2m3 = lm(CL_adj ~ elbow*manus+alpha + I(elbow^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a1e3m2 = lm(CL_adj ~ elbow*manus+alpha + I(elbow^2) + I(manus^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a1e1m3 = lm(CL_adj ~ elbow*manus+alpha + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a1e3m1 = lm(CL_adj ~ elbow*manus+alpha + I(elbow^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a1e1m2 = lm(CL_adj ~ elbow*manus+alpha + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a1e2m1 = lm(CL_adj ~ elbow*manus+alpha + I(elbow^2), data = subset(dat_num, alpha < 5))
# all int reduced with square alpha 
base_intred_a2e2m2 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a2e2m3 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a2e3m2 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a2e1m1 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2), data = subset(dat_num, alpha < 5))
base_intred_a2e1m3 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a2e3m1 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a2e1m2 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a2e2m1 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2), data = subset(dat_num, alpha < 5))
# all intred reduced with cubic alpha 
base_intred_a3e2m2 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(alpha^3), data = subset(dat_num, alpha < 5))
base_intred_a3e2m3 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(alpha^3) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a3e3m2 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(manus^2) + I(alpha^3) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a3e1m1 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(alpha^3), data = subset(dat_num, alpha < 5))
base_intred_a3e1m3 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(manus^2) + I(alpha^3) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a3e3m1 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(alpha^3) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a3e1m2 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(manus^2) + I(alpha^3), data = subset(dat_num, alpha < 5))
base_intred_a3e2m1 = lm(CL_adj ~ elbow*manus+alpha + I(alpha^2) + I(elbow^2) + I(alpha^3), data = subset(dat_num, alpha < 5))

possible_models <- c(AIC(base_int),AIC(base_intred),AIC(base_add),
                     AIC(base_add_a2),AIC(base_add_a2e2),AIC(base_add_a2e2m2),
                     AIC(base_add_a3),AIC(base_add_a3e3),AIC(base_add_a3e2m3),
                     #first order alpha models
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
                     #first order alpha models
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
base_int        = lm(Cm_adj ~ elbow*manus*CL_adj, data = subset(dat_num, alpha < 5))
base_intred     = lm(Cm_adj ~ elbow*manus+CL_adj, data = subset(dat_num, alpha < 5))
base_add        = lm(Cm_adj ~ elbow+manus+CL_adj, data = subset(dat_num, alpha < 5))
base_add_a2     = lm(Cm_adj ~ elbow+manus+CL_adj + I(CL_adj^2), data = subset(dat_num, alpha < 5))
base_add_a2e2   = lm(Cm_adj ~ elbow+manus+CL_adj + I(CL_adj^2)+ I(elbow^2), data = subset(dat_num, alpha < 5))
base_add_a2e2m2 = lm(Cm_adj ~ elbow+manus+CL_adj + I(CL_adj^2)+ I(elbow^2)+ I(manus^2), data = subset(dat_num, alpha < 5))
base_add_a3     = lm(Cm_adj ~ elbow+manus+CL_adj + I(CL_adj^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))
base_add_a3e3   = lm(Cm_adj ~ elbow+manus+CL_adj + I(CL_adj^2)+ I(elbow^2)+ I(CL_adj^3)+ I(elbow^3), data = subset(dat_num, alpha < 5))
base_add_a3e2m3 = lm(Cm_adj ~ elbow+manus+CL_adj + I(CL_adj^2)+ I(elbow^2)+ I(manus^2)+ I(CL_adj^3)+ I(elbow^3)+ I(manus^3), data = subset(dat_num, alpha < 5))
# all int reduced with first order CL_adj 
base_intred_a1e2m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a1e2m3 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a1e3m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2) + I(manus^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a1e1m3 = lm(Cm_adj ~ elbow*manus+CL_adj + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a1e3m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a1e1m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a1e2m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2), data = subset(dat_num, alpha < 5))
# all int with square CL_adj 
base_int_a2e2m2 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_int_a2e2m3 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a2e3m2 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a2e1m1 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2), data = subset(dat_num, alpha < 5))
base_int_a2e1m3 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a2e3m1 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a2e1m2 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_int_a2e2m1 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2), data = subset(dat_num, alpha < 5))
# all int with cubic CL_adj 
base_int_a3e2m2 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))
base_int_a3e2m3 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(CL_adj^3) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a3e3m2 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(CL_adj^3) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a3e1m1 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))
base_int_a3e1m3 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(manus^2) + I(CL_adj^3) + I(manus^3), data = subset(dat_num, alpha < 5))
base_int_a3e3m1 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(CL_adj^3) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_int_a3e1m2 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(manus^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))
base_int_a3e2m1 = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + I(elbow^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))
# all int reduced with first order CL_adj 
base_intred_a1e2m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a1e2m3 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a1e3m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2) + I(manus^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a1e1m3 = lm(Cm_adj ~ elbow*manus+CL_adj + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a1e3m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a1e1m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a1e2m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(elbow^2), data = subset(dat_num, alpha < 5))
# all int reduced with square CL_adj 
base_intred_a2e2m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a2e2m3 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a2e3m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a2e1m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2), data = subset(dat_num, alpha < 5))
base_intred_a2e1m3 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(manus^2) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a2e3m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a2e1m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(manus^2), data = subset(dat_num, alpha < 5))
base_intred_a2e2m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2), data = subset(dat_num, alpha < 5))
# all intred reduced with cubic CL_adj 
base_intred_a3e2m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))
base_intred_a3e2m3 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(CL_adj^3) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a3e3m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(manus^2) + I(CL_adj^3) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a3e1m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))
base_intred_a3e1m3 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(manus^2) + I(CL_adj^3) + I(manus^3), data = subset(dat_num, alpha < 5))
base_intred_a3e3m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(CL_adj^3) + I(elbow^3), data = subset(dat_num, alpha < 5))
base_intred_a3e1m2 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(manus^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))
base_intred_a3e2m1 = lm(Cm_adj ~ elbow*manus+CL_adj + I(CL_adj^2) + I(elbow^2) + I(CL_adj^3), data = subset(dat_num, alpha < 5))

possible_models <- c(AIC(base_int),AIC(base_intred),AIC(base_add),
                     AIC(base_add_a2),AIC(base_add_a2e2),AIC(base_add_a2e2m2),
                     AIC(base_add_a3),AIC(base_add_a3e3),AIC(base_add_a3e2m3),
                     #first order alpha models
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
                     #first order alpha models
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
base_int      = lm(cmcl ~ elbow*manus, data = dat_stab)
base_add      = lm(cmcl ~ elbow+manus, data = dat_stab)
base_add_e2   = lm(cmcl ~ elbow+manus + I(elbow^2), data = dat_stab)
base_add_m2   = lm(cmcl ~ elbow+manus + I(manus^2), data = dat_stab)
base_add_e2m2 = lm(cmcl ~ elbow+manus + I(elbow^2) + I(manus^2), data = dat_stab)
base_add_e3   = lm(cmcl ~ elbow+manus + I(elbow^2) + I(elbow^3), data = dat_stab)
base_add_m3   = lm(cmcl ~ elbow+manus + I(manus^2) + I(manus^3), data = dat_stab)
base_add_e2m3 = lm(cmcl ~ elbow+manus + I(elbow^2) + I(manus^2) + I(manus^3), data = dat_stab)
base_add_e3m2 = lm(cmcl ~ elbow+manus + I(manus^2) + I(elbow^2) + I(elbow^3), data = dat_stab)
base_add_e3m3 = lm(cmcl ~ elbow+manus + I(elbow^2) + I(elbow^3) + I(manus^2) + I(manus^3), data = dat_stab)
base_int_e2   = lm(cmcl ~ elbow*manus + I(elbow^2), data = dat_stab)
base_int_m2   = lm(cmcl ~ elbow*manus + I(manus^2), data = dat_stab)
base_int_e2m2 = lm(cmcl ~ elbow*manus + I(elbow^2) + I(manus^2), data = dat_stab)
base_int_e3   = lm(cmcl ~ elbow*manus + I(elbow^2) + I(elbow^3), data = dat_stab)
base_int_m3   = lm(cmcl ~ elbow*manus + I(manus^2) + I(manus^3), data = dat_stab)
base_int_e2m3 = lm(cmcl ~ elbow*manus + I(elbow^2) + I(manus^2) + I(manus^3), data = dat_stab)
base_int_e3m2 = lm(cmcl ~ elbow*manus + I(manus^2) + I(elbow^2) + I(elbow^3), data = dat_stab)
base_int_e3m3 = lm(cmcl ~ elbow*manus + I(elbow^2) + I(elbow^3) + I(manus^2) + I(manus^3), data = dat_stab)
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
base_int      = lm(cm0 ~ elbow*manus, data = dat_stab)
base_add      = lm(cm0 ~ elbow+manus, data = dat_stab)
base_add_e2   = lm(cm0 ~ elbow+manus + I(elbow^2), data = dat_stab)
base_add_m2   = lm(cm0 ~ elbow+manus + I(manus^2), data = dat_stab)
base_add_e2m2 = lm(cm0 ~ elbow+manus + I(elbow^2) + I(manus^2), data = dat_stab)
base_add_e3   = lm(cm0 ~ elbow+manus + I(elbow^2) + I(elbow^3), data = dat_stab)
base_add_m3   = lm(cm0 ~ elbow+manus + I(manus^2) + I(manus^3), data = dat_stab)
base_add_e2m3 = lm(cm0 ~ elbow+manus + I(elbow^2) + I(manus^2) + I(manus^3), data = dat_stab)
base_add_e3m2 = lm(cm0 ~ elbow+manus + I(manus^2) + I(elbow^2) + I(elbow^3), data = dat_stab)
base_add_e3m3 = lm(cm0 ~ elbow+manus + I(elbow^2) + I(elbow^3) + I(manus^2) + I(manus^3), data = dat_stab)
base_int_e2   = lm(cm0 ~ elbow*manus + I(elbow^2), data = dat_stab)
base_int_m2   = lm(cm0 ~ elbow*manus + I(manus^2), data = dat_stab)
base_int_e2m2 = lm(cm0 ~ elbow*manus + I(elbow^2) + I(manus^2), data = dat_stab)
base_int_e3   = lm(cm0 ~ elbow*manus + I(elbow^2) + I(elbow^3), data = dat_stab)
base_int_m3   = lm(cm0 ~ elbow*manus + I(manus^2) + I(manus^3), data = dat_stab)
base_int_e2m3 = lm(cm0 ~ elbow*manus + I(elbow^2) + I(manus^2) + I(manus^3), data = dat_stab)
base_int_e3m2 = lm(cm0 ~ elbow*manus + I(manus^2) + I(elbow^2) + I(elbow^3), data = dat_stab)
base_int_e3m3 = lm(cm0 ~ elbow*manus + I(elbow^2) + I(elbow^3) + I(manus^2) + I(manus^3), data = dat_stab)
possible_models <- c(AIC(base_int),AIC(base_add),
                     AIC(base_add_e2),AIC(base_add_m2),AIC(base_add_e2m2),
                     AIC(base_add_e3),AIC(base_add_m3),AIC(base_add_e2m3),AIC(base_add_e3m2),AIC(base_add_e3m3),
                     AIC(base_int_e2),AIC(base_int_m2),AIC(base_int_e2m2),
                     AIC(base_int_e3),AIC(base_int_m3),AIC(base_int_e2m3),AIC(base_int_e3m2),AIC(base_int_e3m3))

delta <- possible_models - min(possible_models)
L <- exp(-0.5 * delta)            # relative likelihoods of models
w <- L/sum(L)                     # Akaike weights
which.max(w)
