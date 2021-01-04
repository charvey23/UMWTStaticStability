
## ------------------------------------------------------------------
## --------------- Predefine themes and colours ---------------------
## ------------------------------------------------------------------
library(ggplot2)
library(ggthemes)  # need for geom_rangeframe
library(ggalt)     # for dumbbell plotting
library(gridExtra) # for using grid arrange
library(cowplot)   # need for plot_grid()
library(tidyr)
library(pdftools)
# -------- Main theme ------------
th <- theme_classic() +
  theme(
    # Text
    axis.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8, colour = "black"),
    axis.text.x = element_text(margin = margin(t = 10, unit = "pt")),
    axis.text.y = element_text(margin = margin(r = 10)),
    # Axis line
    axis.line = element_blank(),
    axis.ticks.length = unit(-5,"pt"),
    # Legend
    legend.position = 'none',
    # Background transparency
    # Background of panel
    panel.background = element_rect(fill = "transparent"),
    # Background behind actual data points
    plot.background = element_rect(fill = "transparent", color = NA)
  )

#--- Blank plot to fill space ---
blank_plot <- ggplot() + theme_void()

# -------- Labels ------------
lab_cm         = expression(paste("C" [m[morph]]))
lab_cl         = expression(paste("C" [L[morph]]))   
lab_cm0        = expression(paste("C" [m[morph, L = 0]]))
lab_cmcl       = expression(paste("dC" [m[morph]],"/", "dC" [L[morph]] ))
lab_cm_comp    = expression(paste("C" [m[comp]], "⋅ 10"^3))
lab_cl_comp    = expression(paste("C" [L[comp]], "⋅ 10"^2))  

lab_cm_path    = expression(paste("constant C" [m[morph]]))
lab_cl_path    = expression(paste("constant C" [L[morph]]))   
lab_cmcl_path  = expression(paste("constant dC" [m[morph]],"/", "dC" [L[morph]] ))
lab_link       = "Linkage trajectory"

lab_cmdel      = expression(paste("dC" [m],"/d", delta ))
lab_cldel      = expression(paste("dC" [L],"/d", delta ))
lab_clmanv     = expression(paste("dC" [L],"/d", delta [w] ))
lab_clelbv     = expression(paste("dC" [L],"/d", delta [e] ))
lab_cmmanv     = expression(paste("dC" [m],"/d", delta [w] ))
lab_cmelbv     = expression(paste("dC" [m],"/d", delta [e] ))
lab_elbow      = "Elbow angle (°)"
lab_manus      = "Wrist angle (°)"
lab_pitch      = "Coefficient of Pitching Moment"
lab_lift       = "Coefficient of Lift"
lab_aoa        = "Angle of Attack (°)"
lab_path       = "Trajectory (°)" 
lab_change     = expression(paste(Delta, " in quantity/°"))
# -------- Colour schemes ------------

#create function to retrieve the legend as an object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Angle of Attack
cc_aoa     <- scales::seq_gradient_pal("#ffd89b", "#19547b", "Lab")(seq(0,1,length.out=7))
#https://99designs.com/blog/creative-inspiration/color-combinations/
# Slopes
cc_blue1  <- scales::seq_gradient_pal("#4D8D95", "#013239", "Lab")(seq(0,1,length.out=25))
cc_blue2  <- scales::seq_gradient_pal("#AED6DC", "#4D8D95", "Lab")(seq(0,1,length.out=18))
cc_blue3  <- scales::seq_gradient_pal("#DBF0F3", "#AED6DC", "Lab")(seq(0,1,length.out=5))
cc_L      <- c(cc_blue3[2:5],cc_blue2[2:18],cc_blue1[2:25])
# Slopes
cc_pink1  <- scales::seq_gradient_pal("#B84031", "#690C00", "Lab")(seq(0,1,length.out=20))
cc_pink2  <- scales::seq_gradient_pal("#FF9A8D", "#B84031", "Lab")(seq(0,1,length.out=20))
cc_pink3  <- scales::seq_gradient_pal("#FFBCB3", "#FF9A8D", "Lab")(seq(0,1,length.out=11))
cc_cmcl   <- c(cc_pink3[2:11],cc_pink2[2:20],cc_pink1[2:20])
# Slopes
cc_green1  <- scales::seq_gradient_pal("#B87431", "#442200", "Lab")(seq(0,1,length.out=25))
cc_green2  <- scales::seq_gradient_pal("#FFC68D", "#B87431", "Lab")(seq(0,1,length.out=15))
cc_green3  <- scales::seq_gradient_pal("#FFD9B3", "#FFC68D", "Lab")(seq(0,1,length.out=11))
cc_cm0   <- c(cc_green3[2:11],cc_green2[2:15],cc_green1[2:25])
# Slopes
cc_navy1  <- scales::seq_gradient_pal("#1F2944", "#060D20", "Lab")(seq(0,1,length.out=8))
cc_navy2  <- scales::seq_gradient_pal("#4A536B", "#1F2944", "Lab")(seq(0,1,length.out=10))
cc_navy3  <- scales::seq_gradient_pal("#E2E3E6", "#4A536B", "Lab")(seq(0,1,length.out=20))
cc_m      <- c(cc_navy3[2:20],cc_navy2[2:10],cc_navy1[2:8])

col_num = "#6F9093"
col_exp = "#8B81A6"

col_ext      = "#4C6062"
col_con_cl   = "#6F9093"
col_con_cm   = "#9EB3B5"
col_con_cmcl = "#798081"

## ------------------------------------------------------------------
## -------------------------- Figure 1 ------------------------------
## ------------------------------------------------------------------

# Plot the airfoils for panel B
setwd("/Users/Inman PC/Google Drive/DoctoralThesis/StaticStability/AvianWingLLT/airfoildat/naca0020") 
naca0020  <- read.csv('naca0020_geometry.csv', header = FALSE, stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
naca0020[1,1] <- 1
naca0020[,1] <- as.numeric(naca0020[,1])
setwd("/Users/Inman PC/Google Drive/DoctoralThesis/StaticStability/AvianWingLLT/airfoildat/lius20") 
lius20  <- read.csv('lius20_geometry.csv', header = FALSE, stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
setwd("/Users/Inman PC/Google Drive/DoctoralThesis/StaticStability/AvianWingLLT/airfoildat/lius40") 
lius40  <- read.csv('lius40_geometry.csv', header = FALSE, stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
setwd("/Users/Inman PC/Google Drive/DoctoralThesis/StaticStability/AvianWingLLT/airfoildat/naca3603") 
naca3603  <- read.csv('naca3603_geometry.csv', header = FALSE, stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )
naca3603[1,1] <- 1
naca3603[,1] <- as.numeric(naca3603[,1])
plot_naca0020 <- ggplot()+
  geom_path(data = naca0020, aes(x = V1, y = V2))+
  # theme control
  th +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,1), name = "") + 
  scale_y_continuous(limits = c(-0.1,0.2), name = "") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) 

plot_lius20 <- ggplot()+
  geom_path(data = lius20, aes(x = V1, y = V2))+
  # theme control
  th +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,1), name = "") + 
  scale_y_continuous(limits = c(-0.1,0.2), name = "") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) 

plot_lius40 <- ggplot()+
  geom_path(data = lius40, aes(x = V1, y = V2))+
  # theme control
  th +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,1), name = "") + 
  scale_y_continuous(limits = c(-0.1,0.2), name = "") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) 

plot_naca3603 <- ggplot()+
  geom_path(data = naca3603, aes(x = V1, y = V2)) +
  # theme control
  th +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,1), name = "") + 
  scale_y_continuous(limits = c(-0.1,0.2), name = "") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) 
#exported as 7x7
plot_airfoils <- plot_grid(plot_naca0020,plot_lius20,plot_lius40,plot_naca3603,
                    #arrangement data
                    ncol = 1,nrow = 4)

# plot the control point distribution
setwd("/Users/christinaharvey/Documents/UMWTStaticStability/Data") 
controlpts  <- read.csv('lar_gla_WingID17_0285Test1_Frame4546_U10_alpha2.0_dist_1e-8.csv', stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("") )

# exported as 7x4
plot_controlpts <- ggplot()+
  geom_point(data = controlpts, aes(x = -Control_x, y = Control_y), size = 0.2)+
  # theme control
  th +
  # axis control
  coord_fixed()+
  scale_x_continuous(name = "") + 
  scale_y_continuous(name = "") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) 
## ------------------------------------------------------------------
## -------------------------- Figure 2 ------------------------------
## ------------------------------------------------------------------
# col_ext      = "#14031C"
# col_con_cl   = "#8B859E"
# col_con_cm   = "#504669"
# col_con_cmcl = "#271C43"

## --------------------------- Panel A ------------------------------
plot_rom <- ggplot() +
  # numerical data
  geom_point(data = subset(dat_num, !duplicated(dat_num$elbow) & !duplicated(dat_num$manus)),
             aes(x = elbow, y = manus),
             col = "#7b7c7f", pch = 16, alpha = 0.1, size = 1.5) +
  #experimental data
  geom_point(data = dat_exp,
             aes(x = elbow, y = manus),
             col = col_num, fill = "white", pch = 22, alpha = 1, size = 2.5) +
  # theme control 
  th +
  # axis control 
  coord_fixed() + 
  scale_y_continuous(limits = c(100,185), breaks = c(100,120,140,160,180), name = lab_manus) +
  scale_x_continuous(limits = c(85,182), breaks = c(90,110,130,150,170), name = lab_elbow) +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 90, xend = 170, y = log(0), yend = log(0))

##
dat_exp$FrameID <- factor(dat_exp$FrameID, levels = c("F6003","F4352","F4546","F4911","F4647","F1380","F4849","F2195","F3891"))
dat_num_simp$FrameID <- factor(dat_num_simp$FrameID, levels = c("F6003","F4352","F4546","F4911","F4647","F1380","F4849","F2195","F3891"))
dat_num$FrameID <- factor(dat_num$FrameID, levels = c("F6003","F4352","F4546","F4911","F4647","F1380","F4849","F2195","F3891"))
## --------------------------- Panel D ------------------------------
plot_cl_num_exp <- ggplot() +
  #experimental data
  geom_errorbar(data= subset(dat_exp, U < 14 & alpha < 11 & alpha > -11), 
                aes(x = alpha, ymin=L_comp-2*L_comp_std, ymax=L_comp+2*L_comp_std, col = FrameID), col = col_num, alpha = 0.6, width = 0) +
  geom_point(data = subset(dat_exp, U < 14 & alpha < 11 & alpha > -11), 
             aes(x = alpha, y=L_comp), col = col_num, fill = "white", pch = 22, size = 0.9) +
  # numerical data
  geom_line(data = dat_num_simp, aes(x = alpha, y=L_comp), col = "black", size =0.5, alpha = 0.9) + 
  # theme control
  facet_wrap(~FrameID, nrow = 3) + th +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+
  # axis control
  scale_x_continuous(name = lab_aoa) +
  scale_y_continuous(limits = c(-0.036,0.097), name = lab_cl_comp, breaks = c(-0.03,0,0.03,0.06,0.09), labels = c(-3,0,3,6,9)) +
  annotate(geom = "segment", x = -10,    xend = 10,     y = log(0), yend = log(0)) + 
  annotate(geom = "segment", x = log(0), xend = log(0), y = -0.03, yend = 0.09)
## --------------------------- Panel E ------------------------------
plot_cmcl_num_exp <- ggplot() +
  # data addition 
  geom_errorbar(data= subset(dat_exp, U < 14 & alpha < 11 & alpha > -11), 
                aes(x= L_comp, ymin=m_comp-2*m_comp_std, ymax=m_comp+2*m_comp_std, col = FrameID), col = col_num, alpha = 0.6) +
  geom_errorbarh(data= subset(dat_exp, U < 14 & alpha < 11 & alpha > -11), 
                aes(xmin=L_comp-2*L_comp_std, xmax=L_comp+2*L_comp_std, y=m_comp, col = FrameID), col = col_num, alpha = 0.6) +
  geom_point(data = subset(dat_exp, U < 14 & alpha < 11 & alpha > -11), 
             aes(x = L_comp, y=m_comp), col = col_num, fill = "white", pch = 22, size = 0.9) +
  geom_line(data = subset(dat_num, FrameID %in% wtwings & WingID == "17_0285"), aes(x = L_comp, y=m_comp), col = "black", size =0.5, alpha = 0.9) + 
  # theme control
  facet_wrap(~FrameID, nrow = 3) + th +
  theme(strip.background = element_blank(),
          strip.text.x = element_blank())+
  # axis control
  scale_x_continuous(limits = c(-0.036,0.097), name = lab_cl_comp, breaks = c(-0.03,0,0.03,0.06,0.09), labels = c(-3,0,3,6,9)) +
  scale_y_continuous(limits = c(-0.008,0.004), name = lab_cm_comp, breaks = c(-0.008,-0.004,0,0.004), labels = c(-8,-4,0,4)) +
  annotate(geom = "segment", x = -0.03,    xend = 0.09,     y = log(0), yend = log(0)) + 
  annotate(geom = "segment", x = log(0), xend = log(0), y = -0.008, yend = 0.004)


## --------------------------- Panel E ------------------------------
plot_cd_num_exp <- ggplot() +
  # data addition 
  geom_errorbar(data= subset(dat_exp, U > 14 & alpha < 11 & alpha > -11), 
                aes(x= D_comp, ymin=L_comp-2*L_comp_std, ymax=L_comp+2*L_comp_std, col = FrameID), col = col_num, alpha = 0.6) +
  geom_errorbarh(data= subset(dat_exp, U > 14 & alpha < 11 & alpha > -11), 
                 aes(xmin=D_comp-2*D_comp_std, xmax=D_comp+2*D_comp_std, y=L_comp, col = FrameID), col = col_num, alpha = 0.6) +
  geom_point(data = subset(dat_exp, U > 14 & alpha < 11 & alpha > -11), 
             aes(x = D_comp, y=L_comp), col = col_num, fill = "white", pch = 22, size = 0.9) +
  geom_path(data = subset(dat_num, FrameID %in% wtwings & WingID == "17_0285"), aes(x = D_comp, y=L_comp), col = "black", size =0.5, alpha = 0.9) + 
  # theme control
  facet_wrap(~FrameID, nrow = 3) + th +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+
  # axis control
  scale_y_continuous(limits = c(-0.036,0.097), name = lab_cl_comp, breaks = c(-0.03,0,0.03,0.06,0.09), labels = c(-3,0,3,6,9)) +
  scale_x_continuous(limits = c(-0.002,0.03)) +
  annotate(geom = "segment", y = -0.03,    yend = 0.09,     x= log(0), xend = log(0)) + 
  annotate(geom = "segment", y = log(0), yend = log(0), x = -0.001, xend = 0.03)

# ------------------------------ Wing shapes ----------------------------------
dat_all$FrameID <- paste("F", dat_all$frameID, sep = "")
dat_plotwingshape <- subset(dat_all, FrameID %in% wtwings & WingID == "17_0285") 
i = 9
dat_curr <- gather(dat_plotwingshape[i,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)
plot_experimental <- ggplot() + 
  geom_polygon(aes(x = dat_curr$value[grepl("Y",dat_curr$position)],y = dat_curr$value[grepl("X",dat_curr$position)]),
               fill = col_num, alpha = 0.8, size = 2) +
  # theme control 
  th +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank()) +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.4,0.1), name = "")
plot_experimental
dat_curr$frameID
# ----------------------------------- Combine data panels ------------------------------------------
toprow <- plot_grid(plot_rom,blank_plot,blank_plot,
                    #arrangement data
                    ncol = 3,
                    rel_widths = c(2,1,2),
                    #labels
                    labels = c("A","B","C"),
                    label_size = 10,
                    label_fontfamily = "sans")
bottomrow <- plot_grid(plot_cl_num_exp,plot_cmcl_num_exp,
                    #arrangement data
                    ncol = 2,
                    rel_widths = c(1,1),
                    #labels
                    labels = c("D","E"),
                    label_size = 10,
                    label_fontfamily = "sans")
figure2_plots <- plot_grid(toprow,bottomrow,
                           #arrangement data
                           ncol = 1, rel_heights = c(0.8,1))


setwd("/Users/christinaharvey/Google Drive/DoctoralThesis/StaticStability/AvianWingLLT/Paper/Figures") 

figure2_final <- ggdraw() + # export as 6.5x8
  draw_plot(figure2_plots) +
  draw_image(magick::image_read_pdf("3DprintvsSolidWorks.pdf", density = 600), x = 0, y =  0.3, scale = 0.3) +
  draw_image(magick::image_read_pdf("Planform_F4546.pdf", density = 200), x = 0.45, y =  0.42, scale = 0.15) +
  draw_image(magick::image_read_pdf("Planform_F4352.pdf", density = 200), x = 0.31, y =  0.42, scale = 0.15) +
  draw_image(magick::image_read_pdf("Planform_F6003.pdf", density = 200), x = 0.17, y =  0.42, scale = 0.15) +
  draw_image(magick::image_read_pdf("Planform_F1380.pdf", density = 200), x = 0.45, y =  0.3, scale = 0.15) + 
  draw_image(magick::image_read_pdf("Planform_F4647.pdf", density = 200), x = 0.31, y =  0.3, scale = 0.15) +
  draw_image(magick::image_read_pdf("Planform_F4911.pdf", density = 200), x = 0.17, y =  0.3, scale = 0.15) +
  draw_image(magick::image_read_pdf("Planform_F3891.pdf", density = 200), x = 0.45, y =  0.18, scale = 0.15) + 
  draw_image(magick::image_read_pdf("Planform_F2195.pdf", density = 200), x = 0.31, y =  0.18, scale = 0.15) +
  draw_image(magick::image_read_pdf("Planform_F4849.pdf", density = 200), x = 0.17, y =  0.18, scale = 0.15) 
## ------------------------------------------------------------------
## ------------------------- Figure 3 -------------------------------
## ------------------------------------------------------------------
## --------------------------- Panel A ------------------------------
plot_con_cL <- ggplot() +
  # data
  geom_point(data = subset(dat_num, alpha == 0), aes(x = elbow, y = manus, col = CL_adj), alpha = 0.8)+
  stat_contour(data = data.fit, aes(x = elbow, y = manus, z = CL_adj, colour = ..level..),
               breaks = quantile(data.fit$CL_adj, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradientn(colors = rev(cc_L), name = lab_cl) +
  # theme control
  th +
  theme(legend.position = 'right') +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) + 
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) + 
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0))
# save legend then remove
legend_cL <- g_legend(plot_con_cL)
plot_con_cL <- plot_con_cL + theme(legend.position = 'none')

## --------------------------- Panel B ------------------------------
plot_con_cm <- ggplot() +
  # data
  geom_point(data = subset(dat_num, alpha == 0), aes(x = elbow, y = manus, col = Cm_adj), alpha = 0.8)+
  stat_contour(data = data.fit, aes(x = elbow, y = manus, z = Cm_adj, colour = ..level..),
               breaks = quantile(data.fit$Cm_adj, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradientn(colors = cc_m, name = lab_cm) +
  # theme control
  th +
  theme(legend.position = 'right') +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) +
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0))
# save legend then remove
legend_cm <- g_legend(plot_con_cm)
plot_con_cm <- plot_con_cm + theme(legend.position = 'none')


## --------------------------- Panel C ------------------------------
plot_con_cmcl <- ggplot() +
  # data
  geom_point(data = dat_stab, aes(x = elbow, y = manus, col = cmcl), alpha = 0.8)+
  stat_contour(data = data.fit.stab, aes(x = elbow, y = manus, z = cmcl, colour = ..level..),
               breaks = quantile(data.fit.stab$cmcl, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradientn(colors = cc_cmcl, name = lab_cmcl) +
  # theme control
  th + 
  theme(legend.position = 'right') +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) + 
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) + 
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0))
# save legend then remove
legend_cmcl <- g_legend(plot_con_cmcl)
plot_con_cmcl <- plot_con_cmcl + theme(legend.position = 'none')

## --------------------------- Panel D ------------------------------
plot_con_cm0 <- ggplot() +
  # data
  geom_point(data = dat_stab, aes(x = elbow, y = manus, col = cm0), alpha = 0.8) +
  stat_contour(data = data.fit.stab, aes(x = elbow, y = manus, z = cm0, colour = ..level..),
               breaks = quantile(data.fit.stab$cm0, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradientn(colors = rev(cc_cm0), name = lab_cm0) +
  # theme control
  th + 
  theme(legend.position = 'right') + 
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) + 
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) + 
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0)) 
# save legend then remove
legend_cm0 <- g_legend(plot_con_cm0)
plot_con_cm0 <- plot_con_cm0 + theme(legend.position = 'none')

test <- subset(dat_num, alpha == 0)
test <- merge(test,dat_all_plot, by =c("FrameID","WingID","TestID","elbow","manus"))
test$finalload <- test$CL_adj/max(test$CL_adj) + test$Cm_adj/min(test$Cm_adj)

## Exported as 3x8
ggparcoord(test, columns = c(91,35,32,34,18,16,17), groupColumn = "CL_adj", alphaLines = 0.2, scale = "center", centerObsID = 148) + th +
  scale_colour_gradientn(colors = rev(cc_L), name = lab_cl) + 
  scale_y_continuous(limits = c(-0.72,0.6), breaks = c(-0.6,-0.3,0,0.3,0.6), name = "Normalized value") +
  annotate(geom = "segment", x = log(0), xend = log(0), y = -0.6, yend = 0.6) +
  annotate(geom = "segment", x = 5, xend = 5, y = -0.6, yend = 0.6) +
  annotate(geom = "segment", x = 4.95, xend = 5.05, y = 0, yend = 0)


# ----------------------------------- Combine data panels ------------------------------------------
toprow <- plot_grid(plot_con_cL,legend_cL,plot_con_cm, legend_cm, plot_S_total,legend_S_total,
                    #arrangement data
                    ncol = 6,
                    rel_widths = c(1,0.2,1,0.2,1,0.2),
                    #labels
                    labels = c("A","","B","","E",""),
                    label_size = 10,
                    label_fontfamily = "sans")

midrow <- plot_grid(plot_con_cmcl, legend_cmcl, plot_con_cm0,legend_cm0, plot_S_proj,legend_S_proj,
                       #arrangement data
                       ncol = 6,
                       rel_widths = c(1,0.2,1,0.2,1,0.2),
                       #labels
                       labels = c("C","","D","","F",""),
                       label_size = 10,
                       label_fontfamily = "sans")
bottomrow <- plot_grid(plot_twist, legend_twist, plot_sweep, legend_sweep, plot_dihedral,legend_dihedral,
                    #arrangement data
                    ncol = 6,
                    rel_widths = c(1,0.2,1,0.2,1,0.2),
                    #labels
                    labels = c("G","","H","","I",""),
                    label_size = 10,
                    label_fontfamily = "sans")
#exported as 7.5x9
figure3_final <- plot_grid(toprow,midrow, bottomrow,
                           #arrangement data
                           ncol = 1, nrow = 3, rel_heights = c(1,1,1))

# exported as 7.5x9 - when in the square arrangement
# exported as 3x9 when in the line arrangement
## ------------------------------------------------------------------
## ------------------------- Figure 4 -------------------------------
## ------------------------------------------------------------------
#purple scheme


## --------------------------- Panel A ------------------------------
plot_rom_path <- ggplot() +
  # numerical data
  geom_polygon(data = vertices, aes(x = elbow, y = manus),
             col = "#7b7c7f", alpha = 0.1, size = 1.5, linetype = 0) +
  # extension path line
  geom_path(data = dat_link_ext, aes(x = elbow, y = manus), col = col_ext, size = 0.8) +
  # constant CL path line
  geom_path(data = contour_cl, aes(x = elbow, y = manus), col = col_con_cl, size = 0.8, linetype = 1) +
  # constant Cm path line
  geom_path(data = contour_cm, aes(x = elbow, y = manus), col = col_con_cm, size = 0.8, linetype = 1) +
  # constant CmCL path line
  geom_path(data = contour_cmcl, aes(x = elbow, y = manus), col = col_con_cmcl, size = 0.8, linetype = 1, alpha = 0.9) +
  # theme control 
  th +
  # axis control 
  coord_fixed() + 
  scale_y_continuous(limits = c(100,185), breaks = c(100,120,140,160,180), name = lab_manus) +
  scale_x_continuous(limits = c(85,182), breaks = c(90,110,130,150,170), name = lab_elbow) +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 90, xend = 170, y = log(0), yend = log(0))

## --------------------------- Panel B ------------------------------
pathcomp <- ggplot() +
  geom_bar(data = dat_contour, aes(x = path, y = value_diff_deg, fill = type), 
           stat = "identity", width = 0.5, position = "dodge") +
  # colour control
  scale_fill_manual(values = c(cc_L[20],cc_m[20], cc_cmcl[18],cc_cm0[16])) +
  scale_linetype_manual(values = c(4,2,5,1)) + 
  # theme control
  th +
  # axis control
  scale_x_discrete(labels =c(lab_cl_path,lab_cm_path,lab_cmcl_path,lab_link)) + 
  scale_y_continuous(limits = c(-0.0025, 0.005), breaks= c(-0.0025,0,0.0025,0.005),name = lab_change) + 
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = -0.0025, yend = 0.005) +
  annotate(geom = "segment", x = 1, xend = 4, y = log(0), yend = log(0)) + theme(legend.position = 'none')

## --------------------------------------------------------------------
## ----------------------------- Paths --------------------------------
## --------------------------------------------------------------------
## --------------------------- Panel C ------------------------------
plot_ext_path <- ggplot() +
  # Lift
  geom_path(data = dat_link_ext, aes(x = dist, y = CL_scale), 
            col = cc_L[20], linetype = 1, size = 1.1) +
  # Moment
  geom_path(data = dat_link_ext, aes(x = dist, y = Cm_scale), 
            col = cc_m[20], linetype = 1, size = 1.1) +
  # Pitch Stability Derivative
  geom_path(data = dat_link_ext, aes(x = dist, y = cmcl_scale), 
            col = cc_cmcl[18], linetype = 1, size = 1.1) +
  # Zero Lift Pitch
  geom_path(data = dat_link_ext, aes(x = dist, y = cm0_scale), 
            col = cc_cm0[16], linetype = 1, size = 1.1) +
  # theme control
  th +
  # axis control
  scale_x_continuous(limits = c(0,90), breaks = c(0,20,40,60,80), name = lab_path) + 
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25,0.5,0.75,1), labels = c(0,25,50,75,100), name = "Scaled results (% of maximum)") +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 0, yend = 1) +
  annotate(geom = "segment", x = 0, xend = 80, y = log(0), yend = log(0)) + theme(legend.position = 'none')

## --------------------------- Panel D ------------------------------
plot_con_cl_path <- ggplot() +
  # Lift
  geom_path(data = contour_cl, aes(x = dist, y = CL_scale), 
            col = cc_L[20], linetype = 1, size = 1.1) +
  # Moment
  geom_path(data = contour_cl, aes(x = dist, y = Cm_scale), 
            col = cc_m[20], linetype = 1, size = 1.1) +
  # Pitch Stability Derivative
  geom_path(data = contour_cl, aes(x = dist, y = cmcl_scale), 
            col = cc_cmcl[18], linetype = 1, size = 1.1) +
  # Zero Lift Pitch
  geom_path(data = contour_cl, aes(x = dist, y = cm0_scale), 
            col = cc_cm0[16], linetype = 1, size = 1.1) +
  # theme control
  th +
  # axis control
  scale_x_continuous(limits = c(0,90), breaks = c(0,20,40,60,80), name = lab_path) + 
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25,0.5,0.75,1), labels = c(0,25,50,75,100), name = "Scaled results (% of maximum)") +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 0, yend = 1) +
  annotate(geom = "segment", x = 0, xend = 80, y = log(0), yend = log(0)) + theme(legend.position = 'none')
## --------------------------- Panel E ------------------------------
plot_con_cm_path <- ggplot() +
  # Lift
  geom_path(data = contour_cm, aes(x = dist, y = CL_scale), 
            col = cc_L[20], linetype = 1, size = 1.1) +
  # Moment
  geom_path(data = contour_cm, aes(x = dist, y = Cm_scale), 
            col = cc_m[20], linetype = 1, size = 1.1) +
  # Pitch Stability Derivative
  geom_path(data = contour_cm, aes(x = dist, y = cmcl_scale), 
            col = cc_cmcl[18], linetype = 1, size = 1.1) +
  # Zero Lift Pitch
  geom_path(data = contour_cm, aes(x = dist, y = cm0_scale), 
            col = cc_cm0[16], linetype = 1, size = 1.1) +
  # theme control
  th +
  # axis control
  scale_x_continuous(limits = c(0,90), breaks = c(0,20,40,60,80), name = lab_path) + 
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25,0.5,0.75,1), labels = c(0,25,50,75,100), name = "Scaled results (% of maximum)") +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 0, yend = 1) +
  annotate(geom = "segment", x = 0, xend = 80, y = log(0), yend = log(0)) + theme(legend.position = 'none')

## --------------------------- Panel F ------------------------------
plot_con_cmcl_path <- ggplot() +
  # Lift
  geom_path(data = contour_cmcl, aes(x = dist, y = CL_scale), 
            col = cc_L[20], linetype = 1, size = 1.1) +
  # Moment
  geom_path(data = contour_cmcl, aes(x = dist, y = Cm_scale), 
            col = cc_m[20], linetype = 1, size = 1.1) +
  # Pitch Stability Derivative
  geom_path(data = contour_cmcl, aes(x = dist, y = cmcl_scale), 
            col = cc_cmcl[18], linetype = 1, size = 1.1) +
  # Zero Lift Pitch
  geom_path(data = contour_cmcl, aes(x = dist, y = cm0_scale), 
            col = cc_cm0[16], linetype = 1, size = 1.1) +
  # theme control
  th +
  # axis control
  scale_x_continuous(limits = c(0,90), breaks = c(0,20,40,60,80), name = lab_path) + 
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25,0.5,0.75,1), labels = c(0,25,50,75,100), name = "Scaled results (% of maximum)") +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 0, yend = 1) +
  annotate(geom = "segment", x = 0, xend = 80, y = log(0), yend = log(0)) + theme(legend.position = 'none')



## --------------------------- Panel B ------------------------------
# ----------------------- Planform View of Linkage path --------------------------------
pic_ext_high <- subset(dat_all, elbow > dat_link_ext$elbow[which.max(dat_link_ext$dist)]-1.5 & 
                        elbow < dat_link_ext$elbow[which.max(dat_link_ext$dist)]+1.5 & 
                        manus > dat_link_ext$manus[which.max(dat_link_ext$dist)]-1.5 & 
                        manus < dat_link_ext$manus[which.max(dat_link_ext$dist)]+1.5)
pic_ext_high <- gather(pic_ext_high[1,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)

pic_ext_low <- subset(dat_all, elbow > dat_link_ext$elbow[which.min(dat_link_ext$dist)]-1.5 & 
                         elbow < dat_link_ext$elbow[which.min(dat_link_ext$dist)]+1.5 & 
                         manus > dat_link_ext$manus[which.min(dat_link_ext$dist)]-1.5 & 
                         manus < dat_link_ext$manus[which.min(dat_link_ext$dist)]+1.5)
pic_ext_low <- gather(pic_ext_low[1,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)

plot_ext_yx <- ggplot() + 
  geom_polygon(aes(x = pic_ext_high$value[grepl("Y",pic_ext_high$position)],y = pic_ext_high$value[grepl("X",pic_ext_high$position)]),
               fill = col_ext, alpha = 0.7) + 
  geom_polygon(aes(x = pic_ext_low$value[grepl("Y",pic_ext_low$position)],y = pic_ext_low$value[grepl("X",pic_ext_low$position)]),
               fill = col_ext, alpha = 0.4) + th + 
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.4,0.1), name = "") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) +
  annotate(geom = "segment", x = log(0), xend = log(0), y = -0.4, yend = 0.1) +
  annotate(geom = "segment", x = 0, xend = 0.6, y = log(0), yend = log(0)) 

plot_ext_yz <- ggplot() + 
  geom_polygon(aes(x = pic_ext_high$value[grepl("Y",pic_ext_high$position)],y = -pic_ext_high$value[grepl("Z",pic_ext_high$position)]),
               fill = col_ext, alpha = 0.7) + 
  geom_polygon(aes(x = pic_ext_low$value[grepl("Y",pic_ext_low$position)],y = -pic_ext_low$value[grepl("Z",pic_ext_low$position)]),
               fill = col_ext, alpha = 0.4) + th + 
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.3,0.2), name = "") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) +
  annotate(geom = "segment", x = log(0), xend = log(0), y = -0.3, yend = 0.2) +
  annotate(geom = "segment", x = 0, xend = 0.6, y = log(0), yend = log(0)) 


# ----------------------- Planform View of Constant CL path --------------------------------
pic_cl_high <- subset(dat_all, elbow > contour_cl$elbow[which.max(contour_cl$dist)]-3 & 
                         elbow < contour_cl$elbow[which.max(contour_cl$dist)]+3 & 
                         manus > contour_cl$manus[which.max(contour_cl$dist)]-3 & 
                         manus < contour_cl$manus[which.max(contour_cl$dist)]+3)
pic_cl_high <- gather(pic_cl_high[1,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)

pic_cl_low <- subset(dat_all, elbow > contour_cl$elbow[which.min(contour_cl$dist)]-1.5 & 
                        elbow < contour_cl$elbow[which.min(contour_cl$dist)]+1.5 & 
                        manus > contour_cl$manus[which.min(contour_cl$dist)]-1.5 & 
                        manus < contour_cl$manus[which.min(contour_cl$dist)]+1.5)
pic_cl_low <- gather(pic_cl_low[1,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)

plot_con_cl_yx <- ggplot() + 
  geom_polygon(aes(x = pic_cl_high$value[grepl("Y",pic_cl_high$position)],y = pic_cl_high$value[grepl("X",pic_cl_high$position)]),
               fill = col_con_cl, alpha = 0.7) + 
  geom_polygon(aes(x = pic_cl_low$value[grepl("Y",pic_cl_low$position)],y = pic_cl_low$value[grepl("X",pic_cl_low$position)]),
               fill = col_con_cl, alpha = 0.4) + 
  #theme control
  th +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank()) +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.4,0.1), name = "")

plot_con_cl_yz <- ggplot() + 
  geom_polygon(aes(x = pic_cl_high$value[grepl("Y",pic_cl_high$position)],y = -pic_cl_high$value[grepl("Z",pic_cl_high$position)]),
               fill = col_con_cl, alpha = 0.7) + 
  geom_polygon(aes(x = pic_cl_low$value[grepl("Y",pic_cl_low$position)],y = -pic_cl_low$value[grepl("Z",pic_cl_low$position)]),
               fill = col_con_cl, alpha = 0.4) + 
  #theme control
  th +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank()) +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.3,0.2), name = "")

# ----------------------- Planform View of Constant Cm path --------------------------------

pic_cm_high <- subset(dat_all, elbow > contour_cm$elbow[which.max(contour_cm$dist)]-5 & 
                         elbow < contour_cm$elbow[which.max(contour_cm$dist)]+5 & 
                         manus > contour_cm$manus[which.max(contour_cm$dist)]-5 & 
                         manus < contour_cm$manus[which.max(contour_cm$dist)]+5)
pic_cm_high <- gather(pic_cm_high[2,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)

pic_cm_low <- subset(dat_all, elbow > contour_cm$elbow[which.min(contour_cm$dist)]-1.5 & 
                        elbow < contour_cm$elbow[which.min(contour_cm$dist)]+1.5 & 
                        manus > contour_cm$manus[which.min(contour_cm$dist)]-1.5 & 
                        manus < contour_cm$manus[which.min(contour_cm$dist)]+1.5)
pic_cm_low <- gather(pic_cm_low[1,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)

plot_con_cm_yx <- ggplot() + 
  geom_polygon(aes(x = pic_cm_high$value[grepl("Y",pic_cm_high$position)],y = pic_cm_high$value[grepl("X",pic_cm_high$position)]),
               fill = col_con_cm, alpha = 0.7) + 
  geom_polygon(aes(x = pic_cm_low$value[grepl("Y",pic_cm_low$position)],y = pic_cm_low$value[grepl("X",pic_cm_low$position)]),
               fill = col_con_cm, alpha = 0.4) + 
  #theme control
  th +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank()) +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.4,0.1), name = "")

plot_con_cm_yz <- ggplot() + 
  geom_polygon(aes(x = pic_cm_high$value[grepl("Y",pic_cm_high$position)],y = -pic_cm_high$value[grepl("Z",pic_cm_high$position)]),
               fill = col_con_cm, alpha = 0.7) + 
  geom_polygon(aes(x = pic_cm_low$value[grepl("Y",pic_cm_low$position)],y = -pic_cm_low$value[grepl("Z",pic_cm_low$position)]),
               fill = col_con_cm, alpha = 0.4) + 
  #theme control
  th +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank()) +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.3,0.2), name = "")

# ----------------------- Planform View of Constant CmCL path --------------------------------

pic_cmcl_high <- subset(dat_all, elbow > contour_cmcl$elbow[which.max(contour_cmcl$dist)]-2.5 & 
                         elbow < contour_cmcl$elbow[which.max(contour_cmcl$dist)]+2.5 & 
                         manus > contour_cmcl$manus[which.max(contour_cmcl$dist)]-2.5& 
                         manus < contour_cmcl$manus[which.max(contour_cmcl$dist)]+2.5)
pic_cmcl_high <- gather(pic_cmcl_high[1,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)

pic_cmcl_low <- subset(dat_all, elbow > contour_cmcl$elbow[which.min(contour_cmcl$dist)]-1.5 & 
                        elbow < contour_cmcl$elbow[which.min(contour_cmcl$dist)]+1.5 & 
                        manus > contour_cmcl$manus[which.min(contour_cmcl$dist)]-1.5 & 
                        manus < contour_cmcl$manus[which.min(contour_cmcl$dist)]+1.5)
pic_cmcl_low <- gather(pic_cmcl_low[1,-c(8:22)], position, value,Pt6X:Pt12Z, factor_key = TRUE)

plot_con_cmcl_yx <- ggplot() + 
  geom_polygon(aes(x = pic_cmcl_high$value[grepl("Y",pic_cmcl_high$position)],y = pic_cmcl_high$value[grepl("X",pic_cmcl_high$position)]),
               fill = col_con_cmcl, alpha = 0.7) + 
  geom_polygon(aes(x = pic_cmcl_low$value[grepl("Y",pic_cmcl_low$position)],y = pic_cmcl_low$value[grepl("X",pic_cmcl_low$position)]),
               fill = col_con_cmcl, alpha = 0.4) + 
  #theme control
  th +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank()) +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.4,0.1), name = "") 

plot_con_cmcl_yz <- ggplot() + 
  geom_polygon(aes(x = pic_cmcl_high$value[grepl("Y",pic_cmcl_high$position)],y = -pic_cmcl_high$value[grepl("Z",pic_cmcl_high$position)]),
               fill = col_con_cmcl, alpha = 0.7) + 
  geom_polygon(aes(x = pic_cmcl_low$value[grepl("Y",pic_cmcl_low$position)],y = -pic_cmcl_low$value[grepl("Z",pic_cmcl_low$position)]),
               fill = col_con_cmcl, alpha = 0.4) + 
  #theme control
  th +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank()) +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(0,0.6), name = "") + 
  scale_y_continuous(limits = c(-0.3,0.2), name = "") 

#---------------------------------------------------------------------------

firstcolumn <- plot_grid(plot_rom_path,pathcomp,
                         #arrangement data
                         ncol = 1,nrow=2,
                         rel_widths = c(1),
                         #labels
                         labels = c("A","B"),
                         label_size = 10,
                         label_fontfamily = "sans")
planformcolumn <- plot_grid(plot_con_cl_yx,plot_con_cl_yz,plot_con_cm_yx,plot_con_cm_yz,plot_con_cmcl_yx,plot_con_cmcl_yz,plot_ext_yx,plot_ext_yz,
                    #arrangement data
                    ncol = 2,nrow=4,
                    rel_widths = c(1,1),
                    #labels
                    labels = c("C","D","",""),
                    label_size = 10,
                    label_fontfamily = "sans")
pathcolumn <- plot_grid(plot_con_cl_path,plot_con_cm_path,plot_con_cmcl_path,plot_ext_path,
                         #arrangement data
                         ncol = 1,nrow=4,
                         rel_widths = c(1),
                         #labels
                         labels = c("E",""),
                         label_size = 10,
                         label_fontfamily = "sans")
# export as 7x9
figure4_final <- plot_grid(firstcolumn,planformcolumn,pathcolumn,
                           #arrangement data
                           ncol = 3, rel_heights = c(2.5,0.5,1))
## ----------------------------------------------------------------
## ---------------------- Supplemental figures --------------------
## ----------------------------------------------------------------


plot_twist <- ggplot() +
  # data
  #geom_point(data = dat_wingspec, aes(x = elbow, y = manus, col = twist), alpha = 0, pch = 21)+
  geom_point(data = dat_wingspec, aes(x = elbow, y = manus, fill = twist), col = "gray80", alpha = 1, pch = 21)+
  #stat_contour(data = data.fit, aes(x = elbow, y = manus, z = twist, colour = ..level..),
  #             breaks = quantile(data.fit$twist, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradient2(low = "#18D5C8", mid = "white", high = "#BE3483", midpoint = 0, name = "Wingtip twist") +
  scale_fill_gradient2(low = "#18D5C8", mid = "white", high = "#BE3483", midpoint = 0, name = "Wingtip twist") +
  # theme control
  th +
  theme(legend.position = 'right') +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) +
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0))
# save legend then remove
legend_twist <- g_legend(plot_twist)
plot_twist <- plot_twist + theme(legend.position = 'none')

plot_sweep <- ggplot() +
  # data
  geom_point(data = dat_wingspec, aes(x = elbow, y = manus, col = sweep), alpha = 0.8)+
  stat_contour(data = data.fit, aes(x = elbow, y = manus, z = sweep, colour = ..level..),
               breaks = quantile(data.fit$sweep, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradient2(low = "#18D5C8", mid = "white", high = "#BE3483", midpoint = 0, name = "Wingtip sweep") +
  scale_fill_gradient2(low = "#18D5C8", mid = "white", high = "#BE3483", midpoint = 0, name = "Wingtip sweep") +
  # theme control
  th +
  theme(legend.position = 'right') +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) +
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0))
legend_sweep <- g_legend(plot_sweep)
plot_sweep <- plot_sweep + theme(legend.position = 'none')

plot_dihedral <- ggplot() +
  # data
  geom_point(data = dat_wingspec, aes(x = elbow, y = manus, col = dihedral), alpha = 0.8)+
  stat_contour(data = data.fit, aes(x = elbow, y = manus, z = dihedral, colour = ..level..),
               breaks = quantile(data.fit$dihedral, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradient2(low = "#18D5C8", mid = "white", high = "#BE3483", midpoint = 0, name = "Wingtip dihedral") +
  scale_fill_gradient2(low = "#18D5C8", mid = "white", high = "#BE3483", midpoint = 0, name = "Wingtip dihedral") +
  # theme control
  th +
  theme(legend.position = 'right') +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) +
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0))
legend_dihedral <- g_legend(plot_dihedral)
plot_dihedral <- plot_dihedral + theme(legend.position = 'none')

plot_S_total <- ggplot() +
  # data
  geom_point(data = dat_wingspec, aes(x = elbow, y = manus, col = S_ref), alpha = 0.8) +
  stat_contour(data = data.fit, aes(x = elbow, y = manus, z = S_ref, colour = ..level..),
               breaks = quantile(data.fit$S_ref, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradient(name = bquote(atop("Total wing area", "(% of maximum)"))) +
  # theme control
  th +
  theme(legend.position = 'right') +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) +
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0))
legend_S_total <- g_legend(plot_S_total)
plot_S_total <- plot_S_total + theme(legend.position = 'none')

plot_S_proj <- ggplot() +
  # data
  geom_point(data = dat_all_plot, aes(x = elbow, y = manus, col = S_proj_ref), alpha = 0.8) +
  stat_contour(data = data.fit, aes(x = elbow, y = manus, z = S_proj_ref, colour = ..level..),
               breaks = quantile(data.fit$S_proj_ref, seq(0, 1, 0.1)), size = 0.8) +
  # colour control 
  scale_colour_gradientn(colors = cc_m, name = bquote(atop("Projected wing area","(% of maximum)"))) +
  # theme control
  th +
  theme(legend.position = 'right') +
  # axis control
  coord_fixed()+
  scale_x_continuous(limits = c(80,168), breaks = c(80,100,120,140,160), name = lab_elbow) +
  scale_y_continuous(limits = c(100,180), breaks = c(100,120,140,160,180), name = lab_manus) +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 100, yend = 180) +
  annotate(geom = "segment", x = 80, xend = 160, y = log(0), yend = log(0))
legend_S_proj <- g_legend(plot_S_proj)
plot_S_proj <- plot_S_proj + theme(legend.position = 'none')
