#test base for testing different code
# 1. Define your files
file_paths <- c("D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/04Label.wav",
                "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/10Label.wav",
                "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/16Label.wav",
                "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/19Label.wav",
                "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/23Label.wav")

# 2. Create an empty list to store results
results_list <- list()

# 3. The Loop
for (f in file_paths) {
  message(paste("Processing:", f)) #message function with the "Processing" provides a visual outlook on the task being executed from the script 
  #here f is an iterator or loop variable; here f would be assigned to the first item encountered within file_paths
  #whenever R would encounter f within the for-loop, it would treat it as the item, in this case, first 04Label
  
  # Import
  temp_audio <- readWave(f)

    # Calculations
  aci <- acoustic_complexity(temp_audio)
  bi  <- bioacoustic_index(temp_audio)
  ndsi_val <- ndsi(temp_audio)
  
  # Create temporary row
  temp_df <- data.frame(
    File_Name = basename(f),
    ACI_avg = mean(c(aci$AciTotAll_left, aci$AciTotAll_right)),
    ACI_bymin_avg = mean(c(aci$AciTotAll_left_bymin, aci$AciTotAll_right_bymin)),
    BI_avg = mean(c(bi$left_area, bi$right_area)),
    NDSI_avg = mean(c(ndsi_val$ndsi_left, ndsi_val$ndsi_right)),
    Anthrophony_avg = mean(c(ndsi_val$anthrophony_left, ndsi_val$anthrophony_right)),
    Biophony_avg = mean(c(ndsi_val$biophony_left, ndsi_val$biophony_right))
  )
  
  # Store in list
  results_list[[f]] <- temp_df
}

# 4. Combine all 5 into ONE master dataframe
final_sound_results <- do.call(rbind, results_list)

# 5. Clean up the small loop variables
rm(temp_audio, aci, bi, ndsi_val, temp_df, f, results_list)

View(final_sound_results)


#code2#-----------
  
# 1. Setup your paths
# Create a named list where each entry is a vector of the 5 labels for that replicate
replicates <- list(
  "Rep1" = c("path/to/Rep1_04Label.wav", "path/to/Rep1_05Label.wav", ...),
  "Rep2" = c("path/to/Rep2_04Label.wav", "path/to/Rep2_05Label.wav", ...),
  "Rep3" = c("path/to/Rep3_04Label.wav", "path/to/Rep3_05Label.wav", ...)
)

# 2. Create an empty list to store all results
all_data_list <- list()

# 3. The Nested Loop
for (rep_name in names(replicates)) {
  message(paste("--- Starting Replicate:", rep_name, "---"))
  
  # Get the files for this specific replicate
  current_files <- replicates[[rep_name]]
  
  for (f in current_files) {
    message(paste("Processing file:", basename(f)))
    
    # Import and Calculate
    temp_audio <- readWave(f)
    aci <- acoustic_complexity(temp_audio)
    bi  <- bioacoustic_index(temp_audio)
    ndsi_val <- ndsi(temp_audio)
    
    # Create the row
    temp_df <- data.frame(
      Replicate = rep_name,  # This tags the data so you don't lose track
      File_Name = basename(f),
      ACI_avg = mean(c(aci$AciTotAll_left, aci$AciTotAll_right)),
      BI_avg = mean(c(bi$left_area, bi$right_area)),
      NDSI_avg = mean(c(ndsi_val$ndsi_left, ndsi_val$ndsi_right)),
      Anthrophony_avg = mean(c(ndsi_val$anthrophony_left, ndsi_val$anthrophony_right)),
      Biophony_avg = mean(c(ndsi_val$biophony_left, ndsi_val$biophony_right))
    )
    
    # Add to our big list
    all_data_list[[paste0(rep_name, "_", basename(f))]] <- temp_df
  }
}

# 4. Combine EVERYTHING into one master table
master_sound_results <- do.call(rbind, all_data_list)

# 5. Clean the environment
rm(temp_audio, aci, bi, ndsi_val, temp_df, all_data_list, current_files, f, rep_name)

View(master_sound_results)