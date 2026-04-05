# ============================================================
# SITE-LEVEL ACOUSTIC ANALYSIS SCRIPT
# ============================================================
# Workflow:
#   1. Run analysis for each site (3 replicates per site)
#   2. Combine replicates -> single site-level summary
#   3. Combine all 4 sites -> master comparison table
# ============================================================

library(seewave)
library(tuneR)
library(soundecology)
library(tidyverse)

# ------------------------------------------------------------
# HELPER FUNCTION: Run analysis for one site
# Takes a named list of replicates and a site name,
# returns a per-file dataframe tagged with Site and Replicate
# ------------------------------------------------------------
run_site_analysis <- function(replicates, site_name) {
  
  all_data_list <- list()
  
  for (rep_name in names(replicates)) {
    message(paste("--- Site:", site_name, "| Replicate:", rep_name, "---"))
    
    current_files <- replicates[[rep_name]]
    
    for (f in current_files) {
      message(paste("  Processing file:", basename(f)))
      
      temp_audio <- readWave(f)
      aci       <- acoustic_complexity(temp_audio)
      bi        <- bioacoustic_index(temp_audio)
      ndsi_val  <- ndsi(temp_audio)
      
      temp_df <- data.frame(
        Site             = site_name,
        Replicate        = rep_name,
        File_Name        = basename(f),
        ACI_avg          = mean(c(aci$AciTotAll_left,aci$AciTotAll_right)),
        BI_avg           = mean(c(bi$left_area,bi$right_area)),
        NDSI_avg         = mean(c(ndsi_val$ndsi_left,ndsi_val$ndsi_right)),
        Anthrophony_avg  = mean(c(ndsi_val$anthrophony_left,ndsi_val$anthrophony_right)),
        Biophony_avg     = mean(c(ndsi_val$biophony_left,ndsi_val$biophony_right))
      )
      
      all_data_list[[paste0(rep_name, "_", basename(f))]] <- temp_df
    }
  }
  
  site_raw <- do.call(rbind, all_data_list)
  return(site_raw)
}


# ------------------------------------------------------------
# HELPER FUNCTION: Aggregate replicates -> site-level summary
# Averages all per-file rows into a SINGLE row per site
# ------------------------------------------------------------
aggregate_site <- function(site_raw_df) {
  
  site_summary <- site_raw_df %>%
    group_by(Site) %>%
    summarise(
      N_files          = n(),                      # total files processed
      ACI_site         = mean(ACI_avg,         na.rm = TRUE),
      BI_site          = mean(BI_avg,          na.rm = TRUE),
      NDSI_site        = mean(NDSI_avg,        na.rm = TRUE),
      Anthrophony_site = mean(Anthrophony_avg, na.rm = TRUE),
      Biophony_site    = mean(Biophony_avg,    na.rm = TRUE),
      # Standard deviations - useful for inter-site comparison
      ACI_sd           = sd(ACI_avg,         na.rm = TRUE),
      BI_sd            = sd(BI_avg,          na.rm = TRUE),
      NDSI_sd          = sd(NDSI_avg,        na.rm = TRUE),
      Anthrophony_sd   = sd(Anthrophony_avg, na.rm = TRUE),
      Biophony_sd      = sd(Biophony_avg,    na.rm = TRUE),
      .groups = "drop"
    )
  
  return(site_summary)
}

# Site specific replicates
replicates_site1 <- list(
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


replicates_site2 <- list(
  "Rep1" = c("D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/01Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/03Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/05Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/07Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/10Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/12Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/15Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/18Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/21Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter1/24Label.wav"),
  "Rep2" = c("D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/01Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/03Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/05Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/07Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/10Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/12Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/15Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/18Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/21Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter2/24Label.wav"),
  "Rep3" = c("D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/01Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/03Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/05Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/07Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/10Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/12Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/15Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/18Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/21Label.wav",
             "D:/M.Sc/4th Sem AEWB/Dissertation/audiolabels/guquarter3/23Label.wav")
)


replicates_site3 <- list(
  "Rep1" = c("D:/path/to/site3/rep1/01Label.wav"),
  "Rep2" = c("D:/path/to/site3/rep2/01Label.wav"),
  "Rep3" = c("D:/path/to/site3/rep3/01Label.wav")
)


replicates_site4 <- list(
  "Rep1" = c("D:/path/to/site4/rep1/01Label.wav"),
  "Rep2" = c("D:/path/to/site4/rep2/01Label.wav"),
  "Rep3" = c("D:/path/to/site4/rep3/01Label.wav")
)


# ============================================================
# RUN ANALYSIS FOR ALL 4 SITES
# ============================================================

# -- Step 1: Raw per-file results for each site --
raw_site1 <- run_site_analysis(replicates_site1, site_name = "botanicalgarden")
raw_site2 <- run_site_analysis(replicates_site2, site_name = "guquarter")
raw_site3 <- run_site_analysis(replicates_site3, site_name = "aecwetland")
raw_site4 <- run_site_analysis(replicates_site4, site_name = "lankeshwar")

# -- Step 2: Combine raw results into one master file-level table --
master_raw_all_sites <- bind_rows(raw_site1, raw_site2, raw_site3, raw_site4)

# -- Step 3: Aggregate each site's replicates into a single summary row --
summary_site1 <- aggregate_site(raw_site1)
summary_site2 <- aggregate_site(raw_site2)
summary_site3 <- aggregate_site(raw_site3)
summary_site4 <- aggregate_site(raw_site4)

# -- Step 4: Combine all 4 site summaries -> final comparison table --
site_comparison_table <- bind_rows(summary_site1, summary_site2,
                                   summary_site3, summary_site4)

# ============================================================
# VIEW & EXPORT
# ============================================================

View(master_raw_all_sites)      # All raw file-level data, all sites
View(site_comparison_table)     # One row per site, ready for comparison

# Export raw data (one row per file)
write.csv(master_raw_all_sites,
          file = "all_sites_raw.csv", row.names = FALSE)

# Export site comparison table (one row per site)
write.csv(site_comparison_table,
          file = "site_comparison.csv", row.names = FALSE)