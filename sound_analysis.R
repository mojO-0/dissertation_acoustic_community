#First script for sound analysis

library(seewave)
library(tuneR)
library(soundecology)

audio1 <- readWave("D:/M.Sc/4th Sem AEWB/Dissertation/sound1_06label.wav")

aci <- acoustic_complexity(audio1)
bi <- bioacoustic_index(audio1)
ndsi_value <- ndsi(audio1)


