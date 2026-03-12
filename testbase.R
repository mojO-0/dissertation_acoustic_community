#test base for testing different code

setwd("C:/SOUNDSCAPE")
list.files()

install.packages("soundecology")
install.packages("seewave")
install.packages("tuneR")


library(soundecology)
library(seewave)
library(tuneR)


sound <- readWave("Site1(1)_DeeporBeel.WAV")
sound_mono <- Wave(left = sound@.Data[,1],
                   samp.rate = sound@samp.rate,
                   bit = sound@bit)
spectro(sound_mono)

filtered <- fir(sound_mono, from=1000, to=10000, bandpass=TRUE)

spectro(filtered, f = sound_mono@samp.rate)

aci <- acoustic_complexity(sound_mono)

aci

adi <- acoustic_diversity(sound_mono)
adi

bi <- bioacoustic_index(sound_mono)

ndsi_value <- ndsi(sound_mono)

aei <- acoustic_evenness(sound_mono)
aei 















