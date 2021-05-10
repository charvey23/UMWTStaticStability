
## --------------------------------------------------------------------------------
## ----------------------------------- Functions ----------------------------------
## --------------------------------------------------------------------------------


## ---------------- Remove points in the predicted that are outside of the convex hull ---------------- 
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

# --------- Compute the contour line that the extension path will follow --------- 
contour_specific <- function(z_variable,fit_data,path,path_name,rev,cut){
  tmp        <- contourLinesR(x = fit_data$elbow, y = fit_data$manus, z = z_variable)
  contour <- as.data.frame(matrix(nrow = length(tmp[[path]]$x), ncol = 0))
  
  if (rev) {
    contour$elbow <- rev(tmp[[path]]$x)
    contour$manus <- rev(tmp[[path]]$y)
  }
  else{
    contour$elbow <- tmp[[path]]$x
    contour$manus <- tmp[[path]]$y
  }
  contour <- predict_path(contour,path_name,cut)
  return(contour)
}

# ---------- Predict all the variables along each extension path -----------
predict_path <- function(contour,path_name, cut){
  contour$elbow_scale <- contour$elbow/1000
  contour$manus_scale <- contour$manus/1000
  contour$alpha_scale <- alpha_select
  if (cut){
    tmp           <- cut_trueshape(contour,unique(subset(dat_all, elbow > 85 & manus > 100)[4:5]),1,2) # cut elbow and wrist to the true shape of the data
    contour       <- tmp$dat_cut
  }
  # predict the associated metrics at each point
  contour$CL_adj <- predict(mod_con_cL_num, newdata = contour, re.form = ~0)
  contour$Cm_adj <- predict(mod_con_cm_num, newdata = contour, re.form = ~0)
  
  contour$cmcl <- predict(mod_cmcl_num, newdata = contour, re.form = ~0)
  contour$cm0  <- predict(mod_cm0_num, newdata = contour, re.form = ~0)
  
  contour$path <- path_name
  
  # compute the distance of the path
  contour$dist <- 0
  for (i in 1:(length(contour$elbow)-1)){
    contour$dist[i+1] <- contour$dist[i] + sqrt((contour$elbow[i+1]-contour$elbow[i])^2+(contour$manus[i+1]-contour$manus[i])^2)
  }
  
  # Scale the results to the size of the data
  contour$CL_scale   <- (contour$CL_adj - min(subset(dat_num, alpha == alpha_select)$CL_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$CL_adj))
  # want +ve to be 0 -- i.e. less moment
  contour$Cm_scale   <- -(contour$Cm_adj - max(subset(dat_num, alpha == alpha_select)$Cm_adj))/abs(max(subset(dat_num, alpha == alpha_select)$CL_adj)-min(subset(dat_num, alpha == alpha_select)$Cm_adj))
  contour$cmcl_scale <- -(contour$cmcl - max(dat_stab$cmcl))/abs(max(dat_stab$cmcl)-min(dat_stab$cmcl))
  # want +ve to be 0 -- i.e. more stable
  contour$cm0_scale  <- (contour$cm0 - min(dat_stab$cm0))/abs(max(dat_stab$cm0)-min(dat_stab$cm0))
  return(contour)
}

