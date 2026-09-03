# ------------------------------------------------------------
# 01_load_data.R — Figure 2 loader
# ------------------------------------------------------------

load_figure2_data <- function(path) {
  dat <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  
  # drop the leftover row-index column from write.csv() (unnamed / blank header)
  blank_name_cols <- which(colnames(dat) == "" | is.na(colnames(dat)))
  if (length(blank_name_cols) > 0) {
    dat <- dat[, -blank_name_cols, drop = FALSE]
  }
  dat <- dat %>%
    mutate(
      PRISMA = factor(PRISMA, levels = c("PRISMA-high", "PRISMA-low")),
      CollectionType = factor(CollectionType, levels = names(collection_type_colors)),
      LesionCategory = factor(LesionCategory, levels = names(lesion_category_colors)),
      arm = factor(arm, levels = c("Nivo-Ipi", "Nivo", "TKI")),
      Agg_response = factor(Agg_response, levels = c("R","NR")),
    ) %>%
    as.data.frame()   # guard against tibble propagation, per the earlier Fig1 fix
  dat
}