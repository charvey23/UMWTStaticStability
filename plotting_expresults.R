
## ------------------------------------------------------------------
## --------------- Predefine themes and colours ---------------------
## ------------------------------------------------------------------

# -------- Main theme ------------

#--- Predfine the main theme ----
th <- theme_classic() +
  theme(
    # Text
    axis.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10, colour = "black"),
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


# -------- Labels ------------
lab_cm         = expression(paste("dC" [M],"/", "dC" [L] ))
lab_cmdel      = expression(paste("dC" [M],"/d", delta ))
lab_cldel      = expression(paste("dC" [L],"/d", delta ))
lab_cl         = expression(paste("dC" [L],"/d", alpha ))
lab_clmanv     = expression(paste("dC" [L],"/d", delta [m] ))
lab_clelbv     = expression(paste("dC" [L],"/d", delta [e] ))
lab_cmmanv     = expression(paste("dC" [M],"/d", delta [m] ))
lab_cmelbv     = expression(paste("dC" [M],"/d", delta [e] ))
lab_elbow      = "Elbow Angle (°)"
lab_manus      = "Manus Angle (°)"
lab_pitch      = "Coefficient of Pitching Moment"
lab_lift       = "Coefficient of Lift"
lab_aoa        = "Angle of Attack (°)"

# -------- Colour schemes ------------

# Angle of Attack
cc_aoa     <- scales::seq_gradient_pal("#ffd89b", "#19547b", "Lab")(seq(0,1,length.out=7))

# Slopes
cc_slopes  <- scales::seq_gradient_pal("#1D2B64", "#1D2B64", "Lab")(seq(0,1,length.out=7))

# Elbow and manus angles
cc4  <- scales::seq_gradient_pal("#6A1B9A", "black", "Lab")(seq(0,1,length.out=45))
cc3  <- scales::seq_gradient_pal("#E85285", "#6A1B9A", "Lab")(seq(0,1,length.out=65))
cc2  <- scales::seq_gradient_pal("#FFE69A", "#E85285", "Lab")(seq(0,1,length.out=65))
cc1  <- scales::seq_gradient_pal("white", "#FFE69A", "Lab")(seq(0,1,length.out=29))
cc_full   <- c(cc1[25:29],cc2[2:65],cc3[2:65],cc4[2:44])

cc      <- c("100" = cc_full[66],"105" = cc_full[71],"110" = cc_full[76],
             "115" = cc_full[81],"120" = cc_full[86],"125" = cc_full[91],
             "130" = cc_full[96],"135" = cc_full[101],"140" = cc_full[106],
             "145" = cc_full[111],"150" = "black")

cc_wind <- c("30" = cc_full[6], "35" = cc_full[8], "40" = cc_full[11],
             "45" = cc_full[13],"50" = cc_full[16],"55" = cc_full[21],
             "60" = cc_full[26],"65" = cc_full[31],"70" = cc_full[36],
             "75" = cc_full[41],"80" = cc_full[46],"85" = cc_full[51],
             "90" = cc_full[56],"95" = cc_full[51],"100" = cc_full[66],
             "105" = cc_full[61],"110" = cc_full[76],"115" = cc_full[81],
             "120" = cc_full[86],"125" = cc_full[91],"130" = cc_full[96],
             "135" = cc_full[101],"140" = cc_full[106],"145" = cc_full[111],
             "150" = cc_full[116],"155" = cc_full[121],"160" = cc_full[126],
             "165" = cc_full[131],"170" = cc_full[136],"175" = cc_full[141],
             "180" = cc_full[146],"185" = cc_full[151],"190" = cc_full[156],
             "195" = cc_full[161],"200" = cc_full[166],"205" = cc_full[171],
             "210" = cc_full[176],"215" = "black","220" = "black")

## ------------------------------------------------------------------
## ------------------- Pitch Stability ---------------------------
## ------------------------------------------------------------------

plot_cmcl <- ggplot() +
  geom_point(data = dat_stab_exp,
             aes(x = L_dim, y = m_dim, col =  as.character(10*round(manus/10))),
             pch = 16, alpha = 0.3,size = 1) +
  scale_fill_manual(values = cc_wind)  +
  scale_colour_manual(values = cc_wind) +
  scale_y_continuous(limits = c(-0.005,0.0012), breaks = c(-0.005,-0.004,-0.003,-0.002,-0.001,0,0.001), name = expression(paste(frac("M", paste(rho, "U"^2))))) +
  scale_x_continuous(limits = c(-0.03,0.09), breaks = c(-0.03,0,0.03,0.06,0.09), name = expression(paste(frac("L", paste(rho, "U"^2))))) + th +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = -0.005, yend = 0.001) +
  annotate(geom = "segment", x = -0.03, xend = 0.09, y = log(0), yend = log(0))

## ------------------------------------------------------------------------------------------------------
## --------------- Elbow/Wrist effectiveness for longitudinal stability control -------------------------
## ------------------------------------------------------------------------------------------------------

plot_cmcl_effect <- ggplot() +
  geom_point(data = dat_stab_exp,
             aes(x = elbow, y = manus, col =  cmcl),
             pch = 21, alpha = 0.7, size = 1) +
  geom_point(data = dat_stab_exp,
             aes(x = elbow, y = manus, col =  cmcl),
             pch = 1, alpha = 1, size = 1) +
  scale_fill_manual(values = cc_wind)  +
  scale_colour_manual(values = cc_wind) +
  scale_y_continuous(limits = c(110,171), breaks = c(110,140,170),  name = lab_manus) +
  scale_x_continuous(limits = c(90,151), breaks = c(90,120,150), name = lab_elbow) + th +
  geom_rangeframe() +
  annotate(geom = "segment", x = log(0), xend = log(0), y = 110, yend = 170) +
  annotate(geom = "segment", x = 90, xend = 150, y = log(0), yend = log(0))

## ------------------------------------------------------------------
## ------------------- Lift effectiveness ---------------------------
## ------------------------------------------------------------------

plot_cleffect_elbow <- ggplot() +
  geom_point(data = clmin,
             aes(x = elbow, y = cl_adj, col =  as.character(10*round(manus/10))),
             pch = 16, alpha = 0.25,size = 1) +
  geom_line(data = dat.clmorphdel.elb, aes(x = elbow, y = yvar, group = as.character(manus), col = as.character(manus)),
            size = 1.1) +
  geom_ribbon(data = dat.clmorphdel.elb, aes(x = elbow, ymin = LCL, ymax = UCL, group = as.character(manus), fill = as.character(manus)),
              alpha = 0.25) + th +
  facet_wrap(~species, ncol= 4) + th +
  scale_fill_manual(values = cc_wind)  +
  scale_colour_manual(values = cc_wind) +
  scale_y_continuous(limits = c(-0.55,0.75), breaks = c(-0.5,-0.25,0,0.25,0.5,0.75),name = "Elbow Lift Control Effectiveness") +
  scale_x_continuous(limits = c(30,182), breaks = c(30,50,70,90,110,130,150,170), name = lab_elbow) + theme(legend.position = "none")

plot_cleffect_manus <- ggplot() +
  geom_point(data = clmin,
             aes(x = manus, y = cl_adj, col =  as.character(10*round(elbow/10))),
             pch = 16, alpha = 0.25,size = 1) +
  geom_line(data = dat.clmorphdel.man, aes(x = manus, y = yvar, group = as.character(elbow), col = as.character(elbow)),
            size = 1.1) +
  geom_ribbon(data = dat.clmorphdel.man, aes(x = manus, ymin = LCL, ymax = UCL, group = as.character(elbow), fill = as.character(elbow)),
              alpha = 0.25) + th +
  facet_wrap(~species, ncol= 4) + th +
  scale_fill_manual(values = cc_wind)  +
  scale_colour_manual(values = cc_wind) +
  scale_y_continuous(limits = c(-0.55,0.75), breaks = c(-0.5,-0.25,0,0.25,0.5,0.75),name = "Wrist Lift Control Effectiveness") +
  scale_x_continuous(limits = c(100,211), breaks = c(100,120,140,160,180,200), name = lab_manus) + theme(legend.position = "none")

## ------------------------------------------------------------------
## ------------------- Pitch effectiveness ---------------------------
## ------------------------------------------------------------------

plot_cmeffect_elbow <- ggplot() +
  geom_point(data = clmin,
             aes(x = elbow, y = cm_adj, col =  as.character(10*round(manus/10))),
             pch = 16, alpha = 0.25,size = 1) +
  geom_line(data = dat.cmmorphdel.elb, aes(x = elbow, y = yvar, group = as.character(manus), col = as.character(manus)),
            size = 1.1) +
  geom_ribbon(data = dat.cmmorphdel.elb, aes(x = elbow, ymin = LCL, ymax = UCL, group = as.character(manus), fill = as.character(manus)),
              alpha = 0.25) + th +
  facet_wrap(~species, ncol= 4) + th +
  scale_fill_manual(values = cc_wind)  +
  scale_colour_manual(values = cc_wind) +
  scale_y_continuous(limits = c(-0.27,0.2), breaks = c(-0.2,-0.1,0,0.1,0.15,0.2), name = "Elbow Pitch Control Effectiveness") +
  scale_x_continuous(limits = c(30,182), breaks = c(30,50,70,90,110,130,150,170), name = lab_elbow) + theme(legend.position = "none")

plot_cmeffect_manus <- ggplot() +
  geom_point(data = clmin,
             aes(x = manus, y = cm_adj, col =  as.character(10*round(elbow/10))),
             pch = 16, alpha = 0.25,size = 1) +
  geom_line(data = dat.cmmorphdel.man, aes(x = manus, y = yvar, group = as.character(elbow), col = as.character(elbow)),
            size = 1.1) +
  geom_ribbon(data = dat.cmmorphdel.man, aes(x = manus, ymin = LCL, ymax = UCL, group = as.character(elbow), fill = as.character(elbow)),
              alpha = 0.25) + th +
  facet_wrap(~species, ncol= 4) + th +
  scale_fill_manual(values = cc_wind)  +
  scale_colour_manual(values = cc_wind) +
  scale_y_continuous(limits = c(-0.27,0.2), breaks = c(-0.2,-0.1,0,0.1,0.15,0.2), name = "Wrist Pitch Control Effectiveness") +
  scale_x_continuous(limits = c(100,211), breaks = c(100,120,140,160,180,200), name = lab_manus) + theme(legend.position = "none")

grid.arrange(plot_cleffect_elbow,plot_cleffect_manus, plot_cmeffect_elbow,plot_cmeffect_manus, nrow=2, ncol = 2, heights=c(2,2), widths = c(4,3))