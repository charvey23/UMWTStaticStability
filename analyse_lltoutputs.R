## -------------------------------------------------------------------
## -------------------- Load Required Info ---------------------------
## -------------------------------------------------------------------

## ------------------------------------------------
## ---------------- Load libraries ----------------
## ------------------------------------------------

library(ptinpoly)  # need for determining points outside the convex hull
library(contoureR) # need for extracting the contour lines
library(alphahull) # need for the convex hull
library(lme4)      # need for mixed effect model
library(MuMIn)     # need for r^2 for mixed effects model
setwd('/Users/christinaharvey/Documents/UMWTStaticStability/')
source("support_basefunctions.R")
## -------------------------------------------
## ---------------- Load data ----------------
## -------------------------------------------

#  ----- linkage data ----- 
setwd('/Users/christinaharvey/Documents/UMWTStaticStability/Data')
dat_link_ext  <- read.csv('Lar_gla.linkage.ext.loess.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
names(dat_link_ext)[names(dat_link_ext) == "elbowAngle"] <- "elbow"
names(dat_link_ext)[names(dat_link_ext) == "manusAngle"] <- "manus"
dat_link_ext  <- subset(dat_link_ext, manus > 100 & elbow > 85)

setwd('/Users/christinaharvey/Google Drive/DoctoralThesis/StaticStability/AvianWingLLT')

# ----- All wing shapes read into the code -------------
dat_all <- read.csv('2020_05_25_OrientedWings.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_all <- subset(dat_all, species == "lar_gla" & sweep == 0 & dihedral == 0)
# Calculate the project area for each wing
dat_all$S_proj <- 0
for (i in 1:length(dat_all$frameID)){
  x_vertices = c(dat_all$Pt6X[i],dat_all$Pt7X[i],dat_all$Pt8X[i],dat_all$Pt9X[i],dat_all$Pt10X[i],dat_all$Pt11X[i],dat_all$Pt12X[i])
  y_vertices = c(dat_all$Pt6Y[i],dat_all$Pt7Y[i],dat_all$Pt8Y[i],dat_all$Pt9Y[i],dat_all$Pt10Y[i],dat_all$Pt11Y[i],dat_all$Pt12Y[i])
  dat_all$S_proj[i] <- polyarea(x_vertices, y_vertices)
}

dat_all$S_proj_max <- 0
dat_all$S_proj_max[which(dat_all$WingID == "17_0285")] = max(dat_all$S_proj[which(dat_all$WingID == "17_0285")])
dat_all$S_proj_max[which(dat_all$WingID == "17_0243")] = max(dat_all$S_proj[which(dat_all$WingID == "17_0243")])
dat_all$S_proj_max[which(dat_all$WingID == "16_0048")] = max(dat_all$S_proj[which(dat_all$WingID == "16_0048")])
dat_all$S_proj_ref  = dat_all$S_proj/dat_all$S_proj_max
dat_all$c_root = dat_all$Pt12X - dat_all$Pt11X
dat_all$FrameID <- paste("F", dat_all$frameID, sep = "")

# ----- All numerical results -------
dat_num1        <- read.csv('2020_09_22_List_Converged_1e-8.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c(""),header = FALSE)
# remove wt wings
dat_num1        <- dat_num1[-which(dat_num1[,2] == "17_0285"),]
# load wings where we had a pause after
dat_num2        <- read.csv('2020_10_16_List_Converged_1e-6.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c(""),header = FALSE)
# load rest of wings
dat_num3        <- read.csv('2020_11_20_List_Converged_1e-6.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c(""),header = FALSE)
dat_num         <- rbind(dat_num1,dat_num2,dat_num3)
names(dat_num) <- c("species","WingID","TestID","FrameID","elbow","manus","alpha","U","build_err_max","date","S","ref_c","b_MX","MAC","b","sweep","dihedral","twist",'relax',"CL","CD","Cm","FL","FD","Mm")
dat_num$FrameID <- paste("F", dat_num$FrameID, sep = "")
remove(dat_num1,dat_num2,dat_num3)

## --------------------------------------------
## ---------------- Clean data ----------------
## --------------------------------------------

# remove wings that had unphysical dihedral for some reason
dat_num <- dat_num[-which(dat_num$dihedral < -90),]

dat_num$elbow_scale <- dat_num$elbow/1000
dat_num$manus_scale <- dat_num$manus/1000
dat_num$alpha_scale <- dat_num$alpha/10

# dynamic pressure
q <- 0.5*1.225*(10^2)

dat_num$c_max <- 0
dat_num$c_max[which(dat_num$WingID == "17_0285")] = max(dat_num$ref_c[which(dat_num$WingID == "17_0285")])
dat_num$c_max[which(dat_num$WingID == "17_0243")] = max(dat_num$ref_c[which(dat_num$WingID == "17_0243")])
dat_num$c_max[which(dat_num$WingID == "16_0048")] = max(dat_num$ref_c[which(dat_num$WingID == "16_0048")])

dat_num$S_max <- 0
dat_num$S_max[which(dat_num$WingID == "17_0285")] = max(dat_num$S[which(dat_num$WingID == "17_0285")])
dat_num$S_max[which(dat_num$WingID == "17_0243")] = max(dat_num$S[which(dat_num$WingID == "17_0243")])
dat_num$S_max[which(dat_num$WingID == "16_0048")] = max(dat_num$S[which(dat_num$WingID == "16_0048")])

dat_num$b_max <- 0
dat_num$b_max[which(dat_num$WingID == "17_0285")] = max(dat_num$b[which(dat_num$WingID == "17_0285")])
dat_num$b_max[which(dat_num$WingID == "17_0243")] = max(dat_num$b[which(dat_num$WingID == "17_0243")])
dat_num$b_max[which(dat_num$WingID == "16_0048")] = max(dat_num$b[which(dat_num$WingID == "16_0048")])

dat_num$CL_adj = dat_num$FL/(q*dat_num$S_max)
dat_num$CD_adj = dat_num$FD/(q*dat_num$S_max)
dat_num$Cm_adj = dat_num$Mm/(q*dat_num$S_max*dat_num$c_max)
dat_num$S_ref  = dat_num$S/dat_num$S_max

# ----- Data to compare to wind tunnel data -------
wtwings = c("F4849","F4911","F6003","F2195","F4647","F4352","F3891","F1380","F4546")
# get to a number that is comparable to the experimental data
# The 0.5 here accounts for half the lift produces this ends up being cancelled out in the next adjustment that multiplies S by 0.5 as well
dat_num$L_comp = (0.5*dat_num$FL)/q
dat_num$D_comp = (0.5*dat_num$FD)/q
dat_num$m_comp = (0.5*dat_num$Mm)/q

# ----- Create dataframe with a single row per FrameID --------------
dat_wingspec <- unique(dat_num[c("WingID","TestID","FrameID","elbow","manus","species","twist","sweep","dihedral","S_ref","c_max","elbow_scale","manus_scale")])
no_testedconfigs = nrow(dat_wingspec)

## -------------------------------------------------------------------
## ---------------- Compute the stability derivatives ----------------
## -------------------------------------------------------------------

## ---------------- Initialize matrices ----------------
for (h in 1:2){
  dat_stab  <- data.frame(matrix(nrow = no_testedconfigs, ncol = 6))
  names(dat_stab) <- c("species","WingID","TestID","FrameID","elbow","manus")
  count = 1
  compare = FALSE
  if (h == 1){
    compare = TRUE
  }
  
  # loop through each wing configuration
  for (m in 1:no_testedconfigs){
    
    # subset data to be of one wing configuration at a time
    dat_curr <- subset(dat_num, 
                       species == dat_wingspec$species[m] & WingID == dat_wingspec$WingID[m] & 
                         TestID == dat_wingspec$TestID[m] & FrameID == dat_wingspec$FrameID[m])
    
    # save all wing specific information  
    dat_stab$species[count] <- as.character(dat_wingspec$species[m])
    dat_stab$WingID[count]  <- dat_wingspec$WingID[m]
    dat_stab$TestID[count]  <- dat_wingspec$TestID[m]
    dat_stab$FrameID[count] <- dat_wingspec$FrameID[m]
    dat_stab$elbow[count]   <- dat_wingspec$elbow[m]
    dat_stab$manus[count]   <- dat_wingspec$manus[m]
    
    # don't try to fit a curve to less than 4 data points
    if(nrow(dat_curr) < 4){
      dat_stab$cm0[count] = NA
      next}
    
    if (compare){
      mod.pstab  <- lm(m_comp~L_comp, data = dat_curr)
    } else {
      mod.pstab  <- lm(Cm_adj~CL_adj, data = dat_curr)
    }
    #fit linear model to Cm vs. CL
    test       <- summary(mod.pstab)
    mod.pstaba <- lm(Cm_adj~alpha, data = dat_curr)
    mod.lift   <- lm(CL_adj~alpha, data = dat_curr)
    mod.drag   <- lm(CD_adj~poly(alpha,2), data = dat_curr)
    
    # If the full range is linear
    if(test$r.squared < 0.99){
      # If not linear then limit to upper range only
      dat_curr <- subset(dat_curr, CL > 0)
      # don't try to fit a curve to less than 4 data points
      if(nrow(dat_curr) < 4){
        dat_stab$cm0[count] = NA
        next} 
      # re-fit linear models
      if (compare){
        mod.pstab  <- lm(m_comp~L_comp, data = dat_curr)
      } else {
        mod.pstab  <- lm(Cm_adj~CL_adj, data = dat_curr)
      }
      test       <- summary(mod.pstab)
      mod.pstaba <- lm(Cm_adj~alpha, data = dat_curr)
      mod.lift   <- lm(CL_adj~alpha, data = dat_curr)
      mod.drag   <- lm(CD_adj~poly(alpha,2), data = dat_curr)
    }
    
    # save all info about the linear model
    # ------------- Cm/CL ------------- 
    # Intercept
    dat_stab$cm0[count]     <- summary(mod.pstab)$coefficients[1,1]
    dat_stab$cm0_err[count] <- summary(mod.pstab)$coefficients[1,2]
    dat_stab$cm0_lcb[count] <- confint(mod.pstab)[1,1]
    dat_stab$cm0_ucb[count] <- confint(mod.pstab)[1,2]
    # Slope
    dat_stab$cmcl[count]    <- summary(mod.pstab)$coefficients[2,1]
    dat_stab$cmcl_err[count]<- summary(mod.pstab)$coefficients[2,2]
    dat_stab$R2[count]      <- summary(mod.pstab)$r.squared
    dat_stab$cmcl_lcb[count]<- confint(mod.pstab)[2,1]
    dat_stab$cmcl_ucb[count]<- confint(mod.pstab)[2,2]
    
    count = count + 1
  }
  
  if (compare){
    dat_stab_comp      <- subset(dat_stab, FrameID %in% wtwings & WingID == "17_0285")
  } else {
    # remove rows that have not been completed due to lack of info on that wing 
    dat_stab           <- dat_stab[complete.cases(dat_stab[,1]),]
    dat_stab           <- subset(dat_stab, R2 > 0.99)
  }
  remove(dat_curr, mod.pstab, mod.pstaba, mod.lift, mod.drag, test)
}

dat_stab  <- merge(dat_stab,dat_wingspec, by =c("FrameID","WingID","TestID","elbow","manus"))
dat_stab$x_int <- -dat_stab$cm0/dat_stab$cmcl
dat_stab$elbow_scale <- dat_stab$elbow/1000
dat_stab$manus_scale <- dat_stab$manus/1000

dat_all_plot <- merge(dat_wingspec,dat_all, by =c("FrameID","WingID","TestID","elbow","manus"))
dat_all_plot$elbow_scale  <- dat_all_plot$elbow/1000
dat_all_plot$manus_scale  <- dat_all_plot$manus/1000
## -------------------------------------------------------------------
## ----------------- Compute the control derivatives -----------------
## -------------------------------------------------------------------
# define control models - 
# NOTE: These models are selected using Akaike Criterion assuming that they have polynomial relationships
# AIC max was model 26
mod_con_cL_num = lmer(CL_adj ~ elbow_scale*manus_scale*alpha_scale + I(alpha_scale^2) + I(alpha_scale^3) + 
                      I(elbow_scale^2) + 
                      I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = subset(dat_num,alpha < 5))
# AIC max was model 27
mod_con_cm_num = lmer(Cm_adj ~ elbow_scale*manus_scale*CL_adj + I(CL_adj^2) + I(CL_adj^3) + 
                      I(elbow_scale^2) + I(elbow_scale^3) + 
                      I(manus_scale^2) + (1|WingID), data = subset(dat_num,alpha < 5))
mod_cmcl_num  = lmer(cmcl ~ elbow_scale*manus_scale + 
                     I(elbow_scale^2) + I(elbow_scale^3) +
                     I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_stab)
mod_cm0_num   = lmer(cm0 ~ elbow_scale*manus_scale + 
                     I(elbow_scale^2) + I(elbow_scale^3) +
                     I(manus_scale^2) + I(manus_scale^3)  + (1|WingID), data = dat_stab)
## models for how elbow and wrist affect twist
mod_twist    = lmer(twist ~ elbow_scale*manus_scale +
                      I(elbow_scale^2) + I(elbow_scale^3) +
                      I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_wingspec)
mod_sweep    = lmer(sweep ~ elbow_scale+manus_scale +
                 I(elbow_scale^2) + I(elbow_scale^3) +
                 I(manus_scale^2) + (1|WingID), data = dat_wingspec)
mod_dihedral = lmer(dihedral ~ elbow_scale*manus_scale +
                 I(elbow_scale^2) + I(elbow_scale^3) +
                 I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_wingspec)
mod_S_total  = lmer(S_ref ~ elbow_scale*manus_scale +
                    I(elbow_scale^2) + I(elbow_scale^3) + 
                    I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_wingspec)
mod_S_proj  = lmer(S_proj_ref ~ elbow_scale*manus_scale +
                      I(elbow_scale^2) + I(elbow_scale^3) +
                      I(manus_scale^2) + I(manus_scale^3) + (1|WingID), data = dat_all_plot)

# Generate data frame with every possible combination of elbow and wrist
alpha_select <- 0

xgrid <-  seq(floor(min(dat_num$elbow)), ceiling(max(dat_num$elbow)), 1)
ygrid <-  seq(floor(min(dat_num$manus)), ceiling(max(dat_num$manus)), 1)
data.fit       <- expand.grid(elbow = xgrid, manus = ygrid)
tmp            <- cut_trueshape(data.fit,unique(subset(dat_all, elbow > 85 & manus > 100)[4:5]),1,2) # cut elbow and wrist to the true shape of the data
data.fit       <- tmp$dat_cut
vertices       <- tmp$vertices
data.fit$elbow_scale <- data.fit$elbow/1000
data.fit$manus_scale <- data.fit$manus/1000
data.fit.stab        <- data.fit

data.fit$alpha       <- alpha_select
data.fit$alpha_scale <- data.fit$alpha/10
remove(xgrid,ygrid)
# --- Predict data based on the control models --------
data.fit$CL_adj  <-  predict(mod_con_cL_num, newdata = data.fit, re.form = ~0)
data.fit$Cm_adj  <-  predict(mod_con_cm_num, newdata = data.fit, re.form = ~0)
data.fit$twist   <-  predict(mod_twist, newdata = data.fit, re.form = ~0)
data.fit$sweep   <-  predict(mod_sweep, newdata = data.fit, re.form = ~0)
data.fit$dihedral<-  predict(mod_dihedral, newdata = data.fit, re.form = ~0)
data.fit$S_ref   <-  predict(mod_S_total, newdata = data.fit, re.form = ~0)
data.fit$S_proj_ref <-  predict(mod_S_proj, newdata = data.fit, re.form = ~0)

data.fit.stab$cmcl  <-  predict(mod_cmcl_num, newdata = data.fit.stab, re.form = ~0)
data.fit.stab$cm0   <-  predict(mod_cm0_num, newdata = data.fit.stab, re.form = ~0)

## -------------------------------------------------------------------
## ----------------- Compute the pathway derivatives -----------------
## ------------------------------------------------------------------- 
dat_link_ext  <- predict_path(dat_link_ext,"linkage", FALSE)
contour_cl    <- contour_specific(data.fit$CL_adj,data.fit,10,"con_cl", TRUE, TRUE)
contour_cm    <- contour_specific(data.fit$Cm_adj,data.fit,1,"con_cm", TRUE, TRUE)
contour_cmcl  <- contour_specific(data.fit.stab$cmcl,data.fit.stab,1,"con_cmcl", FALSE, TRUE)

type_list <- c("CL","Cm","CmCL","Cm0")
path_list <- c("con_cl", "con_cm", "con_cmcl","linkage")
dat_contour <- as.data.frame(matrix(ncol = 0, nrow = 16))
count = 1
for (i in 1:4){
  for (j in 1:4){
    if (i == 1){
      dat = contour_cl
    } else if (i == 2){
      dat = contour_cm
    } else if (i == 3){
      dat = contour_cmcl
    } else {
      dat = dat_link_ext[2:16]
    }
    dat_contour$path[count]           <- path_list[i]
    dat_contour$type[count]           <- type_list[j]
    dist_diff                         <- dat$dist[which.max(dat$dist)] - dat$dist[which.min(dat$dist)]
    dat_contour$value_diff[count]     <- dat[which.max(dat$dist), (5+j)] - dat[which.min(dat$dist), (5+j)] 
    dat_contour$value_diff_deg[count] <- dat_contour$value_diff[count]/dist_diff
    count = count + 1
  }
}
dat_contour$path <- factor(dat_contour$path, levels = c("con_cl","con_cm","con_cmcl","linkage"))
dat_contour$type <- factor(dat_contour$type, levels = c("CL","Cm","CmCL","Cm0"))

remove(dat,type_list,path_list)

## - Calculate the instantaneous control effectiveness
contour_cl$ctl_eff_cl <- 0
contour_cl$ctl_eff_cm <- 0
for (i in 2:(length(contour_cl$elbow)-1)){
  contour_cl$ctl_eff_cl[i] <- (contour_cl$CL_adj[i+1] - contour_cl$CL_adj[i-1])/(contour_cl$dist[i+1] - contour_cl$dist[i-1])
  contour_cl$ctl_eff_cm[i] <- (contour_cl$Cm_adj[i+1] - contour_cl$Cm_adj[i-1])/(contour_cl$dist[i+1] - contour_cl$dist[i-1])
}


contour_cmcl$ctl_eff_cl <- 0
contour_cmcl$ctl_eff_cm <- 0
for (i in 2:(length(contour_cmcl$elbow)-1)){
  contour_cmcl$ctl_eff_cl[i] <- (contour_cmcl$CL_adj[i+1] - contour_cmcl$CL_adj[i-1])/(contour_cmcl$dist[i+1] - contour_cmcl$dist[i-1])
  contour_cmcl$ctl_eff_cm[i] <- (contour_cmcl$Cm_adj[i+1] - contour_cmcl$Cm_adj[i-1])/(contour_cmcl$dist[i+1] - contour_cmcl$dist[i-1])
}

dat_link_ext$ctl_eff_cl <- 0
dat_link_ext$ctl_eff_cm <- 0
for (i in 2:(length(dat_link_ext$elbow)-1)){
  dat_link_ext$ctl_eff_cl[i] <- (dat_link_ext$CL_adj[i+1] - dat_link_ext$CL_adj[i-1])/(dat_link_ext$dist[i+1] - dat_link_ext$dist[i-1])
  dat_link_ext$ctl_eff_cm[i] <- (dat_link_ext$Cm_adj[i+1] - dat_link_ext$Cm_adj[i-1])/(dat_link_ext$dist[i+1] - dat_link_ext$dist[i-1])
}

## ------------------------------------------------------
## ---- Extract numbers referenced within the paper -----
## ------------------------------------------------------

#maximum summed error of wing shapes
max(dat_num$build_err_max)

## conditional R^2 
r.squaredGLMM(mod_con_cL_num)
r.squaredGLMM(mod_con_cm_num)
r.squaredGLMM(mod_cmcl_num)
r.squaredGLMM(mod_cm0_num)

# shift in static margin
max(dat_stab$cmcl*dat_stab$c_max)-min(dat_stab$cmcl*dat_stab$c_max) # absolute - each piece individually returns the x_np max and min (assumes that x_cg = 0)
max(dat_stab$cmcl)-min(dat_stab$cmcl) # relative
0.039/((max(dat_stab$cmcl*dat_stab$c_max)-min(dat_stab$cmcl*dat_stab$c_max))/(max(dat_stab$cmcl)-min(dat_stab$cmcl)))

# discuss the absolute range in lift and pitch
max(subset(dat_num, alpha == 0)$CL_adj)-min(subset(dat_num, alpha == 0)$CL_adj)
max(subset(dat_num, alpha == 0)$Cm_adj)-min(subset(dat_num, alpha == 0)$Cm_adj)

# trajectories
#check linearity for constant lift
test <- lm(cmcl~dist,data = contour_cl)
summary(test) # to pull out R^2
# check that it is over halfway
min(which(contour_cl$ctl_eff_cm > 0.0005))/length(contour_cl$ctl_eff_cm)
#check linearity for constant static margin
test <- lm(CL_adj~dist,data = contour_cmcl)
summary(test) # to pull out R^2
test <- lm(Cm_adj~dist,data = contour_cmcl)
summary(test) # to pull out R^2

# Check that the fit is non-linear for constant pitching moment
plot(contour_cm$dist,contour_cm$CL_adj)
plot(contour_cm$dist,contour_cm$cmcl)
plot(contour_cm$CL_adj,contour_cm$cmcl)

# Trajectory 4
#start
max(dat_link_ext$ctl_eff_cl)
dat_link_ext$ctl_eff_cm[which.max(dat_link_ext$ctl_eff_cl)]
#near end
max(dat_link_ext$ctl_eff_cm)
dat_link_ext$ctl_eff_cl[which.max(dat_link_ext$ctl_eff_cm)]                        

#max span
max(dat_num$b_max)*2 + 0.1044 #add in body width