## -----------------------------------------------------------------------------
## ------------------------- Main Processing Data Script -----------------------
## -----------------------------------------------------------------------------

# Created: 22-Oct-20 (Christina Harvey)

## ---------------- Load libraries ----------------
library(R.matlab)
library(phonTools)
source("calc_basefunctions.R")
## ---------------- Pre-define folder names ----------------------
folder_names <- c("09_28_F1380_high", "09_28_F1380_low", "09_28_F4352_high", "09_29_F2195_high","09_29_F2195_low",
                  "09_29_F4546_high", "09_29_F4546_low", "09_29_F4849_high", "09_29_F4849_low", "09_30_F3891_high",
                  "09_30_F3891_low", "09_30_F4647_high", "09_30_F4647_low", "09_30_F4911_high", "10_01_F4911_low",
                  "10_01_F4352_low", "10_01_F6003_low", "10_01_F6003_high")
folder_data <- c("/Users/Inman PC/Google Drive/DoctoralThesis/WindTunnel/2020_Gull_PassiveLongitudinalStudy/Data") #For Windows

## ---------------- Pre-define storage matrix ----------------------
completedata        <- as.data.frame(matrix(nrow = 800, ncol = 3))
names(completedata) <- c("FrameID","U_des","alpha")
count = 1

## ---------------- Loop through each test folder ----------------------
for (i in 5:18){

  ## ---------------- Set Current Directory ----------------
  curr_folder <- paste(folder_data,"/2020_",folder_names[i],"_full",sep="")
  setwd(curr_folder) 
  
  # save all file names in the folder
  filelist <- list.files(pattern = ".mat")
  
  for (j in 1:length(filelist)){
    # read in the .mat files one at a time
    rundata  <- readMat(filelist[j])
    
    completedata$FrameID[count] <- unlist(rundata$WingID)
    
    if (rundata$U[1,1] > 14){
      completedata$U_des[count] <- "high"
    } else {
      completedata$U_des[count] <- "low"
    }
    # define the current angle of attack
    completedata$alpha[count]    <- rundata$curr.angle[1,1]
    completedata$P_atm[count]    <- rundata$P.atm[1,1]
    
    completedata$PT_V_off[count]     <- rundata$P.V.offset[1,1]
    completedata$LC_FX_V_off[count]  <- rundata$LC.V.offset[1,1]
    completedata$LC_FY_V_off[count]  <- rundata$LC.V.offset[1,2]
    completedata$LC_FZ_V_off[count]  <- rundata$LC.V.offset[1,3]
    completedata$LC_MX_V_off[count]  <- rundata$LC.V.offset[1,4]
    completedata$LC_MY_V_off[count]  <- rundata$LC.V.offset[1,5]
    completedata$LC_MZ_V_off[count]  <- rundata$LC.V.offset[1,6]
    
    completedata$PT_V[count]     <- mean(rundata$data.P.V[,1])
    completedata$LC_FX_V[count]  <- mean(rundata$data.LC.V[,1])
    completedata$LC_FY_V[count]  <- mean(rundata$data.LC.V[,2])
    completedata$LC_FZ_V[count]  <- mean(rundata$data.LC.V[,3])
    completedata$LC_MX_V[count]  <- mean(rundata$data.LC.V[,4])
    completedata$LC_MY_V[count]  <- mean(rundata$data.LC.V[,5])
    completedata$LC_MZ_V[count]  <- mean(rundata$data.LC.V[,6])
    completedata$T_atm[count]    <- mean(rundata$data.T[,1])
    
    # number of samples
    no_samples   <- length(rundata$data.LC.V[,1])
    no_samples_T <- length(rundata$data.T[,1])
    
    ## ------------ Save the standard deviation (uncertainty) and the effective degrees of freedom -------------
    info_PT_V     <- timeseries_std(rundata$data.P.V[,1], no_samples)
    info_LC_FX_V  <- timeseries_std(rundata$data.LC.V[,1], no_samples)
    info_LC_FY_V  <- timeseries_std(rundata$data.LC.V[,2], no_samples)
    info_LC_FZ_V  <- timeseries_std(rundata$data.LC.V[,3], no_samples)
    info_LC_MX_V  <- timeseries_std(rundata$data.LC.V[,4], no_samples)
    info_LC_MY_V  <- timeseries_std(rundata$data.LC.V[,5], no_samples)
    info_LC_MZ_V  <- timeseries_std(rundata$data.LC.V[,6], no_samples)
    info_T_atm    <- timeseries_std(rundata$data.T[,1], no_samples_T)

    completedata$PT_V_std[count]     <- info_PT_V$data_std_true
    completedata$LC_FX_V_std[count]  <- info_LC_FX_V$data_std_true
    completedata$LC_FY_V_std[count]  <- info_LC_FY_V$data_std_true
    completedata$LC_FZ_V_std[count]  <- info_LC_FZ_V$data_std_true
    completedata$LC_MX_V_std[count]  <- info_LC_MX_V$data_std_true
    completedata$LC_MY_V_std[count]  <- info_LC_MY_V$data_std_true
    completedata$LC_MZ_V_std[count]  <- info_LC_MZ_V$data_std_true
    completedata$T_atm_std[count]    <- info_T_atm$data_std_true
    
    completedata$PT_V_dof[count]     <- info_PT_V$no_dof
    completedata$LC_FX_V_dof[count]  <- info_LC_FX_V$no_dof
    completedata$LC_FY_V_dof[count]  <- info_LC_FY_V$no_dof
    completedata$LC_FZ_V_dof[count]  <- info_LC_FZ_V$no_dof
    completedata$LC_MX_V_dof[count]  <- info_LC_MX_V$no_dof
    completedata$LC_MY_V_dof[count]  <- info_LC_MY_V$no_dof
    completedata$LC_MZ_V_dof[count]  <- info_LC_MZ_V$no_dof
    completedata$T_atm_dof[count]    <- info_T_atm$no_dof
    
    ## --------------- Save the cross correlation data ----------------
    completedata$cor_PT_LC1[count]     <- cor(rundata$data.P.V[,1],rundata$data.LC.V[,1]) # PT and lc channel 1
    completedata$cor_PT_LC2[count]     <- cor(rundata$data.P.V[,1],rundata$data.LC.V[,2]) # PT and lc channel 2
    completedata$cor_PT_LC3[count]     <- cor(rundata$data.P.V[,1],rundata$data.LC.V[,3]) # PT and lc channel 3
    completedata$cor_PT_LC4[count]     <- cor(rundata$data.P.V[,1],rundata$data.LC.V[,4]) # PT and lc channel 4
    completedata$cor_PT_LC5[count]     <- cor(rundata$data.P.V[,1],rundata$data.LC.V[,5]) # PT and lc channel 5
    completedata$cor_PT_LC6[count]     <- cor(rundata$data.P.V[,1],rundata$data.LC.V[,6]) # PT and lc channel 6
    
    completedata$cor_LC1_LC2[count]    <- cor(rundata$data.LC.V[,1],rundata$data.LC.V[,2]) # lc channel 1 and lc channel 2
    completedata$cor_LC1_LC3[count]    <- cor(rundata$data.LC.V[,1],rundata$data.LC.V[,3]) # lc channel 1 and lc channel 3
    completedata$cor_LC1_LC4[count]    <- cor(rundata$data.LC.V[,1],rundata$data.LC.V[,4]) # lc channel 1 and lc channel 4
    completedata$cor_LC1_LC5[count]    <- cor(rundata$data.LC.V[,1],rundata$data.LC.V[,5]) # lc channel 1 and lc channel 5
    completedata$cor_LC1_LC6[count]    <- cor(rundata$data.LC.V[,1],rundata$data.LC.V[,6]) # lc channel 1 and lc channel 6
    
    completedata$cor_LC2_LC3[count]    <- cor(rundata$data.LC.V[,2],rundata$data.LC.V[,3]) # lc channel 2 and lc channel 3
    completedata$cor_LC2_LC4[count]    <- cor(rundata$data.LC.V[,2],rundata$data.LC.V[,4]) # lc channel 2 and lc channel 4
    completedata$cor_LC2_LC5[count]    <- cor(rundata$data.LC.V[,2],rundata$data.LC.V[,5]) # lc channel 2 and lc channel 5
    completedata$cor_LC2_LC6[count]    <- cor(rundata$data.LC.V[,2],rundata$data.LC.V[,6]) # lc channel 2 and lc channel 6
    
    completedata$cor_LC3_LC4[count]    <- cor(rundata$data.LC.V[,3],rundata$data.LC.V[,4]) # lc channel 3 and lc channel 4
    completedata$cor_LC3_LC5[count]    <- cor(rundata$data.LC.V[,3],rundata$data.LC.V[,5]) # lc channel 3 and lc channel 5
    completedata$cor_LC3_LC6[count]    <- cor(rundata$data.LC.V[,3],rundata$data.LC.V[,6]) # lc channel 3 and lc channel 6
    
    completedata$cor_LC4_LC5[count]    <- cor(rundata$data.LC.V[,4],rundata$data.LC.V[,5]) # lc channel 4 and lc channel 5
    completedata$cor_LC4_LC6[count]    <- cor(rundata$data.LC.V[,4],rundata$data.LC.V[,6]) # lc channel 4 and lc channel 5
    
    completedata$cor_LC5_LC6[count]    <- cor(rundata$data.LC.V[,5],rundata$data.LC.V[,6]) # lc channel 5 and lc channel 6
    
    count = count + 1
  }
}

write.csv(completedata, file="2020_10_24_ConcatenatedRawData.csv")

