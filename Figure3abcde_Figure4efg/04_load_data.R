# ============================================================
# Fig3/code/04_load_data.R
# Loader for the event-level AE metatable (one row per adverse
# event occurrence) — used by Figure 3 panels a-e.
# ============================================================
library(dplyr)

load_figure3_events_data <- function(path) {
  dat <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  
  blank_name_cols <- which(colnames(dat) == "" | is.na(colnames(dat)))
  if (length(blank_name_cols) > 0) dat <- dat[, -blank_name_cols, drop = FALSE]
  
  dat <- dat %>%
    dplyr::mutate(
      Tx    = factor(Tx, levels = c("Nivo-Ipi", "Nivo", "TKI")),
      Grade = factor(Grade, levels = c(1, 2, 3, 4, 5), ordered = TRUE)
    ) %>%
    as.data.frame()
  
  # NOTE: do NOT check for duplicate IDs here — this is event-level
  # data, so the same patient ID legitimately appears in multiple
  # rows (one per AE). The uniqueness check from other loaders
  # (Fig2/Fig3 patient-level tables) does not apply to this table.
  
  dat
}