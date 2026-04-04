#First script for sound analysis

library(seewave)
library(tuneR)
library(soundecology)
#import audio for analysis:
audio1 <- readWave("D:/M.Sc/4th Sem AEWB/Dissertation/sound1_06label.wav")

#Acoustic Complexity index
aci <- acoustic_complexity(audio1) #execute for dataset
  ##average values taken for calculation
    aci_avg<-mean(c(aci$AciTotAll_left,aci$AciTotAll_right))
    aci_bymin_avg<-mean(c(aci$AciTotAll_left_bymin,aci$AciTotAll_right_bymin))

#Bioacoustic index
bi <- bioacoustic_index(audio1) #execute for dataset
  ##average values taken for calculation:
    bi_avg<-mean(c(bi$left_area,bi$right_area))

#Normalized Difference Soundscape Index
ndsi_value <- ndsi(audio1) #execute for dataset
  ##average values taken for calculation:
    ndsi_avg<-mean(c(ndsi_value$ndsi_left,ndsi_value$ndsi_right))
    antph_avg<-mean(c(ndsi_value$anthrophony_left,ndsi_value$anthrophony_right))
    bioph_avg<-mean(c(ndsi_value$biophony_left,ndsi_value$biophony_right))
  
# Combine the averages into a single table
    sound_results <- data.frame(
      File_Name = "sound1_06label.wav",
      ACI_avg = aci_avg,
      ACI_bymin_avg = aci_bymin_avg,
      BI_avg = bi_avg,
      NDSI_avg = ndsi_avg,
      Anthrophony_avg = antph_avg,
      Biophony_avg = bioph_avg
    )
    
View(sound_results)

#This script is executed for multiple labels from a single recording
#Multiple labels from the single location would then be analyzed to calculate average indices values of a single sample site

#Finalized code for acoustic indices analysis

#code2#-----------

library(seewave)
library(tuneR)
library(soundecology)

#Setting up filepaths
#Each entry in the list is a vector of the 5 labels for that replicate
replicates <- list(
  "Rep1" = c("D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/01Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/03Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/05Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/07Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/10Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/12Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/15Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/18Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/21Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden1/24Label.wav"),
  "Rep2" = c("D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/01Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/03Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/05Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/07Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/10Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/12Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/15Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/18Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/21Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden3/24Label.wav"),
  "Rep3" = c("D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/01Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/03Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/05Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/07Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/10Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/12Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/15Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/18Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/21Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/botgarden4/24Label.wav")
)

#Empty list to store results from analysis
all_data_list <- list()

#Nested Loop -- inner loop with functions for each replicate
for (rep_name in names(replicates)) {
  message(paste("--- Starting Replicate:", rep_name, "---"))
  
  #Callback for specific replicate
  current_files <- replicates[[rep_name]]
  
  for (f in current_files) {
    message(paste("Processing file:", basename(f)))
    
    #Import and Calculate
    temp_audio <- readWave(f)
    aci <- acoustic_complexity(temp_audio)
    bi  <- bioacoustic_index(temp_audio)
    ndsi_val <- ndsi(temp_audio)
    
    #Create the row
    temp_df <- data.frame(
      Replicate = rep_name,  # This tags the data so you don't lose track
      File_Name = basename(f),
      ACI_avg = mean(c(aci$AciTotAll_left, aci$AciTotAll_right)),
      BI_avg = mean(c(bi$left_area, bi$right_area)),
      NDSI_avg = mean(c(ndsi_val$ndsi_left, ndsi_val$ndsi_right)),
      Anthrophony_avg = mean(c(ndsi_val$anthrophony_left, ndsi_val$anthrophony_right)),
      Biophony_avg = mean(c(ndsi_val$biophony_left, ndsi_val$biophony_right))
    )
    
    #Combination of data into the bigger list for replicate
    all_data_list[[paste0(rep_name, "_", basename(f))]] <- temp_df
  }
}

#Master table for entire site
master_sound_results <- do.call(rbind, all_data_list)

#Cleanup!!!
rm(temp_audio, aci, bi, ndsi_val, temp_df, all_data_list, current_files, f, rep_name)

View(master_sound_results)

#Create csv for further analysis
library(tidyverse)
write.csv(master_sound_results, file = "botanicalgarden.csv", row.names = FALSE)


