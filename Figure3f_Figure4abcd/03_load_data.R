#============================================================
  # Fig3/code/03_load_data.R
  # ============================================================
library(dplyr)

load_figure3_data <- function(path) {
  dat <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  
  blank_name_cols <- which(colnames(dat) == "" | is.na(colnames(dat)))
  if (length(blank_name_cols) > 0) dat <- dat[, -blank_name_cols, drop = FALSE]
  
  dat <- dat %>%
    mutate(arm = factor(arm, levels = c("Nivo-Ipi", "Nivo", "TKI"))) %>%
    as.data.frame()
  
  if (anyDuplicated(dat$ID) > 0) {
    stop("Duplicate IDs found in Figure3_metaDat — resolve before proceeding (see Figure 2's ",
         "id/Patients key-mismatch bug for the likely cause if this recurs).")
  }
  
  dat
}