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



