# ============================================================
# Fig1/code/01_load_data.R
# ============================================================
library(dplyr)

load_figure1_data <- function(path) {
  
  dat <- readr::read_csv(path)  # 
  
  dat <- dat %>%
    rename(arm = `Treament arm`) %>%          # fixing the source typo in the column name
    mutate(
      arm = factor(arm, levels = c("Nivo-Ipi", "Nivo", "TKI")),
      
      MG = recode(`Molecular Group`,
                  "ccrcc 1" = "G1", "ccrcc 2" = "G2",
                  "ccrcc 3" = "G3", "ccrcc 4" = "G4"),
      MG = factor(MG, levels = c("G1", "G2", "G3", "G4")),
      
      IMDC = factor(IMDC, levels = c("Good", "Intermediate", "Poor")),
      
      PRISMA = ifelse(PRISMA_num > 0, "PRISMA-high", "PRISMA-low"),
      PRISMA = factor(PRISMA, levels = c("PRISMA-high", "PRISMA-low")),
      
      Sarcomatoid = factor(recode(Sarcomatoid_binary, "NO" = "Absent", "YES" = "Present"),
                           levels = c("Absent", "Present")),
      Rhabdoid    = factor(recode(Rhabdoid_binary, "NO" = "Absent", "YES" = "Present"),
                           levels = c("Absent", "Present")),
      
      # note:  corrected here to "Sarcomatoid"
      Both_components = recode(Components_combined_binary, "Sarmacotoid" = "Sarcomatoid"),
      Both_components = factor(Both_components,
                               levels = c("Both_components", "None", "Rhabdoid", "Sarcomatoid"))
    )
  
  dat
}

