#First script for sound analysis

library(seewave)
library(tuneR)
library(soundecology)
#import audio for analysis:
audio1 <- readWave("D:/M.Sc/4th Sem AEWB/Dissertation/sound1_06label.wav")

#Acoustic Complexity index
aci <- acoustic_complexity(audio1) #execute for dataset
  aci_t_l<-aci$AciTotAll_left
  aci_t_r<-aci$AciTotAll_right
  aci_l_bymin<-aci$AciTotAll_left_bymin
  aci_r_bymin<-aci$AciTotAll_right_bymin
  ##average values taken for calculation
    aci_avg<-mean(c(aci_t_l,aci_t_r))
    aci_bymin_avg<-mean(c(aci_l_bymin,aci_r_bymin))
      
#Bioacoustic index
bi <- bioacoustic_index(audio1) #execute for dataset
  bi_l<-bi$left_area
  bi_r<-bi$right_area
  ##average values taken for calculation:
    bi_avg<-mean(c(bi_l,bi_r))

#Normalized Difference Soundscape Index
ndsi_value <- ndsi(audio1) #execute for dataset
  ndsi_l<-ndsi_value$ndsi_left
  ndsi_r<-ndsi_value$ndsi_right
  antph_l<-ndsi_value$anthrophony_left
  antph_r<-ndsi_value$anthrophony_right
  bioph_l<-ndsi_value$biophony_left
  bioph_r<-ndsi_value$biophony_right
  ##average values taken for calculation:
    ndsi_avg<-mean(c(ndsi_l,ndsi_r))
    antph_avg<-mean(c(antph_l,antph_r))
    bioph_avg<-mean(c(bioph_l,bioph_r))
  
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

#This script is executed for multiple labels from a single recording; multiple labels from the single location would then be analyzed to calculate average indices values


