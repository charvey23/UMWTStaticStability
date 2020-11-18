## -------------------------------------------------------------------
## -------------------- Load Required Info ---------------------------
## -------------------------------------------------------------------

## ---------------- Load libraries ----------------
library(ptinpoly) # need for determining points outside the convex hull
library(contoureR) # need for extracting the contour lines
library(alphahull) # needed for the convex hull
## ---------------- Load data ----------------
setwd('/Users/christinaharvey/Google Drive/DoctoralThesis/StaticStability/AvianWingLLT')

# ----- All numerical results -------
dat_num1        <- read.csv('2020_09_22_List_Converged_1e-8.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c(""),header = FALSE)
dat_num2        <- read.csv('2020_10_16_List_Converged_1e-6.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c(""),header = FALSE)
dat_num3        <- read.csv('2020_11_13_List_Converged_1e-6.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c(""),header = FALSE) # needs to be updated
dat_num         <- rbind(dat_num1,dat_num2,dat_num3)
names(dat_num) <- c("species","WingID","TestID","FrameID","elbow","manus","alpha","U","build_err_max","date","S","ref_c","b_MX","MAC","b","sweep","dihedral","twist",'relax',"CL","CD","Cm","FL","FD","Mm")
dat_num$FrameID <- paste("F", dat_num$FrameID, sep = "")
remove(dat_num1,dat_num2,dat_num3)
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

dat_num$CL_adj = dat_num$FL/(q*dat_num$S_max)
dat_num$CD_adj = dat_num$FD/(q*dat_num$S_max)
dat_num$Cm_adj = dat_num$Mm/(q*dat_num$S_max*dat_num$c_max)
# ----- Data to compare to wind tunnel data -------
wtwings = c("F4849","F4911","F6003","F2195","F4647","F4352","F3891","F1380","F4546")
# get to a number that is comparable to the experimental data
dat_num$L_comp = (0.5*dat_num$FL)/q
dat_num$D_comp = (0.5*dat_num$FD)/q
dat_num$m_comp = (0.5*dat_num$Mm)/q

# ----- Create dataframe with a single row per FrameID --------------
dat_wingspec <- unique(dat_num[c("WingID","TestID","FrameID","elbow","manus","species")])
no_testedconfigs = nrow(dat_wingspec)

# ----- All wing shapes read into the code -------------
dat_all <- read.csv('2020_05_25_OrientedWings.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_all <- subset(dat_all, species == "lar_gla" & sweep == 0 & dihedral == 0)

## -------------------------------------------------------------------
## ---------------- Compute the stability derivatives ----------------
## -------------------------------------------------------------------

## ---------------- Initialize matrices ----------------
dat_stab  <- data.frame(matrix(nrow = no_testedconfigs, ncol = 6))
names(dat_stab) <- c("species","WingID","TestID","FrameID","elbow","manus")

count = 1
compare = FALSE
# loop through each wing configuration
for (m in 1:no_testedconfigs){
  
  # subset data to be of one wing configuration at a time
  dat_curr <- subset(dat_num, species == dat_wingspec$species[m] & WingID == dat_wingspec$WingID[m] & TestID == dat_wingspec$TestID[m] & FrameID == dat_wingspec$FrameID[m])
  
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
    
    # save all info about the lnear model
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

## -------------------------------------------------------------------
## ----------------- Compute the control derivatives -----------------
## -------------------------------------------------------------------
# define control models - 
# NOTE: These models are selected using Aikaike Criterion assuming that they have polynomial relationships
mod_con_cL_num = lm(CL_adj ~ elbow*manus*alpha + I(alpha^2) + I(alpha^3) + 
                      I(elbow^2) + 
                      I(manus^2) + I(manus^3), data = dat_num)

mod_con_cm_num = lm(Cm_adj ~ elbow*manus*CL_adj + I(CL_adj^2) + 
                      I(elbow^2) + 
                      I(manus^2) + I(manus^3), data = dat_num)

mod_cmcl_num  = lm(cmcl ~ elbow*manus + 
                     I(elbow^2) + 
                     I(manus^2), data = dat_stab)

mod_cm0_num   = lm(cm0 ~ elbow*manus + 
                     I(elbow^2) + I(elbow^3) + 
                     I(manus^2) + I(manus^3), data = dat_stab)

## Remove points in the predicted that are outside of the convex hull
cut_trueshape <- function(dat,dat_geom,col_elbow,col_manus){
  # fit the convex hull with an alpha factor
  alphashape <- ahull(dat_geom, alpha = 30)
  # save all the given vertices
  vertices <- as.data.frame(dat_geom[alphashape$ashape.obj$alpha.extremes,])
  # Need to order the points appropriately
  # calculate the mean value
  centerpt <- c(mean(vertices[,1]),mean(vertices[,2]))
  # calculate the angle from the mean of each point
  vertices$angle <- atan2(vertices[,2]-centerpt[2],vertices[,1]-centerpt[1])
  # sort by the angle
  vertices <- vertices[order(vertices$angle),]
  
  # cut to be within the polygon
  filtele   <- pip2d(as.matrix(vertices[,c(1:2)]),as.matrix(dat[,c(col_elbow,col_manus)])) 
  dat_cut   <- dat[filtele==1,]  # filtele==1 retains only the points that are inside the hull
  dat_return <- list()
  dat_return$dat_cut  <- dat_cut
  dat_return$vertices <- vertices
  return(dat_return)
}



# Generate dataframe with every possible combination of elbow and wrist
xgrid <-  seq(floor(min(dat_num$elbow)), ceiling(max(dat_num$elbow)), 1)
ygrid <-  seq(floor(min(dat_num$manus)), ceiling(max(dat_num$manus)), 1)
data.fit       <- expand.grid(elbow = xgrid, manus = ygrid)
tmp            <- cut_trueshape(data.fit,unique(subset(dat_all, elbow > 85 & manus > 100)[4:5]),1,2) # cut elbow and wrist to the true shape of the data
data.fit       <- tmp$dat_cut
vertices       <- tmp$vertices
data.fit.stab  <- data.fit
data.fit$alpha <- alpha_select
remove(xgrid,ygrid)
# --- Predict data based on the control models --------
data.fit$CL_adj  <-  predict(mod_con_cL_num, newdata = data.fit)
data.fit$Cm_adj  <-  predict(mod_con_cm_num, newdata = data.fit)

data.fit.stab$cmcl  <-  predict(mod_cmcl_num, newdata = data.fit.stab)
data.fit.stab$cm0   <-  predict(mod_cm0_num, newdata = data.fit.stab)

## -------------------------------------------------------------------
## ----------------- Compute the pathway derivatives -----------------
## ------------------------------------------------------------------- 

setwd("/Users/Inman PC/Documents/UMWTStaticStability/Data") 

dat_link_ext  <- read.csv('Lar_gla.linkage.ext.loess.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
dat_link_flx  <- read.csv('Lar_gla.linkage.flx.loess.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )

# add in the base angle of attack 
dat_link_ext$alpha <- alpha_select
names(dat_link_ext)[names(dat_link_ext) == "elbowAngle"] <- "elbow"
names(dat_link_ext)[names(dat_link_ext) == "manusAngle"] <- "manus"
#limit the data
dat_link_ext  <- subset(dat_link_ext, manus > 100 & elbow > 85)
# predict the associated metrics at each point
dat_link_ext$CL_adj <- predict(mod_con_cL_num, newdata = dat_link_ext)
dat_link_ext$Cm_adj <- predict(mod_con_cm_num, newdata = dat_link_ext)

dat_link_ext$cmcl   <- predict(mod_cmcl_num, newdata = dat_link_ext)
dat_link_ext$cm0    <- predict(mod_cm0_num, newdata = dat_link_ext)

dat_link_ext$path <- "linkage"
# calculate the distance between each data point in the extension pathway
dat_link_ext$dist <- 0
for (i in 1:(length(dat_link_ext$elbow)-1)){
  dat_link_ext$dist[i+1] <- dat_link_ext$dist[i] + sqrt((dat_link_ext$elbow[i+1]-dat_link_ext$elbow[i])^2+(dat_link_ext$manus[i+1]-dat_link_ext$manus[i])^2)
}
# Scale the results to the size of the data
dat_link_ext$CL_scale   <- (dat_link_ext$CL_adj - min(subset(dat_num, alpha == alpha_select)$CL_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$CL_adj))
# want +ve to be 0 -- i.e. less moment
dat_link_ext$Cm_scale   <- -(dat_link_ext$Cm_adj - max(subset(dat_num, alpha == alpha_select)$Cm_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$Cm_adj))
dat_link_ext$cmcl_scale <- -(dat_link_ext$cmcl - max(dat_stab$cmcl))/abs(max(dat_stab$cmcl)-min(dat_stab$cmcl))
# want +ve to be 0 -- i.e. more stable
dat_link_ext$cm0_scale  <- (dat_link_ext$cm0 - min(dat_stab$cm0))/abs(max(dat_stab$cm0)-min(dat_stab$cm0))

# ----- Constant lift path -----
tmp        <- contourLinesR(x = data.fit$elbow, y = data.fit$manus, z = data.fit$CL_adj, nlevels = 5)
contour_cl <- as.data.frame(matrix(nrow = length(tmp[[3]]$x), ncol = 0))
contour_cl$elbow <- rev(tmp[[3]]$x)
contour_cl$manus <- rev(tmp[[3]]$y)
contour_cl$alpha <- alpha_select
tmp              <- cut_trueshape(contour_cl,unique(subset(dat_all, elbow > 85 & manus > 100)[4:5]),1,2) # cut elbow and wrist to the true shape of the data
contour_cl       <- tmp$dat_cut

# predict the associated metrics at each point
contour_cl$CL_adj   <- predict(mod_con_cL_num, newdata = contour_cl)
contour_cl$Cm_adj   <- predict(mod_con_cm_num, newdata = contour_cl)

contour_cl$cmcl <- predict(mod_cmcl_num, newdata = contour_cl)
contour_cl$cm0  <- predict(mod_cm0_num, newdata = contour_cl)

contour_cl$path <- "con_cl_low"
contour_cl$dist <- 0
for (i in 1:(length(contour_cl$elbow)-1)){
  contour_cl$dist[i+1] <- contour_cl$dist[i] + sqrt((contour_cl$elbow[i+1]-contour_cl$elbow[i])^2+(contour_cl$manus[i+1]-contour_cl$manus[i])^2)
}
# Scale the results to the size of the data
contour_cl$CL_scale   <- (contour_cl$CL_adj - min(subset(dat_num, alpha == alpha_select)$CL_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$CL_adj))
# want +ve to be 0 -- i.e. less moment
contour_cl$Cm_scale   <- -(contour_cl$Cm_adj - max(subset(dat_num, alpha == alpha_select)$Cm_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$Cm_adj))
contour_cl$cmcl_scale <- -(contour_cl$cmcl - max(dat_stab$cmcl))/abs(max(dat_stab$cmcl)-min(dat_stab$cmcl))
# want +ve to be 0 -- i.e. more stable
contour_cl$cm0_scale  <- (contour_cl$cm0 - min(dat_stab$cm0))/abs(max(dat_stab$cm0)-min(dat_stab$cm0))

# ----- Constant moment path -----
tmp        <- contourLinesR(x = data.fit$elbow, y = data.fit$manus, z = data.fit$Cm_adj)
contour_cm <- as.data.frame(matrix(nrow = length(tmp[[1]]$x), ncol = 0))
contour_cm$elbow <- rev(tmp[[1]]$x)
contour_cm$manus <- rev(tmp[[1]]$y)
contour_cm$alpha <- alpha_select
tmp              <- cut_trueshape(contour_cm,unique(subset(dat_all, elbow > 85 & manus > 100)[4:5]),1,2) # cut elbow and wrist to the true shape of the data
contour_cm       <- tmp$dat_cut
# predict the associated metrics at each point
contour_cm$CL_adj <- predict(mod_con_cL_num, newdata = contour_cm)
contour_cm$Cm_adj <- predict(mod_con_cm_num, newdata = contour_cm)

contour_cm$cmcl <- predict(mod_cmcl_num, newdata = contour_cm)
contour_cm$cm0  <- predict(mod_cm0_num, newdata = contour_cm)

contour_cm$path <- "con_cm"
contour_cm$dist <- 0
for (i in 1:(length(contour_cm$elbow)-1)){
  contour_cm$dist[i+1] <- contour_cm$dist[i] + sqrt((contour_cm$elbow[i+1]-contour_cm$elbow[i])^2+(contour_cm$manus[i+1]-contour_cm$manus[i])^2)
}

# Scale the results to the size of the data
contour_cm$CL_scale   <- (contour_cm$CL_adj - min(subset(dat_num, alpha == alpha_select)$CL_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$CL_adj))
# want +ve to be 0 -- i.e. less moment
contour_cm$Cm_scale   <- -(contour_cm$Cm_adj - max(subset(dat_num, alpha == alpha_select)$Cm_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$Cm_adj))
contour_cm$cmcl_scale <- -(contour_cm$cmcl - max(dat_stab$cmcl))/abs(max(dat_stab$cmcl)-min(dat_stab$cmcl))
# want +ve to be 0 -- i.e. more stable
contour_cm$cm0_scale  <- (contour_cm$cm0 - min(dat_stab$cm0))/abs(max(dat_stab$cm0)-min(dat_stab$cm0))
# ----- Constant dCm/dCL path -----
tmp        <- contourLinesR(x = data.fit.stab$elbow, y = data.fit.stab$manus, z = data.fit.stab$cmcl)
contour_cmcl <- as.data.frame(matrix(nrow = length(tmp[[1]]$x), ncol = 0))
contour_cmcl$elbow <- rev(tmp[[1]]$x)
contour_cmcl$manus <- rev(tmp[[1]]$y)
contour_cmcl$alpha <- alpha_select
tmp                <- cut_trueshape(contour_cmcl,unique(subset(dat_all, elbow > 85 & manus > 100)[4:5]),1,2) # cut elbow and wrist to the true shape of the data
contour_cmcl       <- tmp$dat_cut
# predict the associated metrics at each point
contour_cmcl$CL_adj <- predict(mod_con_cL_num, newdata = contour_cmcl)
contour_cmcl$Cm_adj <- predict(mod_con_cm_num, newdata = contour_cmcl)

contour_cmcl$cmcl <- predict(mod_cmcl_num, newdata = contour_cmcl)
contour_cmcl$cm0  <- predict(mod_cm0_num, newdata = contour_cmcl)

contour_cmcl$path <- "con_cmcl"
contour_cmcl$dist <- 0
for (i in 1:(length(contour_cmcl$elbow)-1)){
  contour_cmcl$dist[i+1] <- contour_cmcl$dist[i] + sqrt((contour_cmcl$elbow[i+1]-contour_cmcl$elbow[i])^2+(contour_cmcl$manus[i+1]-contour_cmcl$manus[i])^2)
}
remove(tmp)
# Scale the results to the size of the data
contour_cmcl$CL_scale   <- (contour_cmcl$CL_adj - min(subset(dat_num, alpha == alpha_select)$CL_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$CL_adj))
# want +ve to be 0 -- i.e. less moment
contour_cmcl$Cm_scale   <- -(contour_cmcl$Cm_adj - max(subset(dat_num, alpha == alpha_select)$Cm_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$Cm_adj))
contour_cmcl$cmcl_scale <- -(contour_cmcl$cmcl - max(dat_stab$cmcl))/abs(max(dat_stab$cmcl)-min(dat_stab$cmcl))
# want +ve to be 0 -- i.e. more stable
contour_cmcl$cm0_scale  <- (contour_cmcl$cm0 - min(dat_stab$cm0))/abs(max(dat_stab$cm0)-min(dat_stab$cm0))

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
      dat = dat_link_ext[2:10]
    }
    dat_contour$path[count]           <- path_list[i]
    dat_contour$type[count]           <- type_list[j]
    dist_diff                         <- dat$dist[which.max(dat$dist)] - dat$dist[which.min(dat$dist)]
    dat_contour$value_diff[count]     <- dat[which.max(dat$dist), (3+j)] - dat[which.min(dat$dist), (3+j)] 
    dat_contour$value_diff_deg[count] <- dat_contour$value_diff[count]/dist_diff
    count = count + 1
  }
}
remove(dat,type_list,path_list)
