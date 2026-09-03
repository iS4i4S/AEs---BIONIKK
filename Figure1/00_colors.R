# ============================================================
# Fig1/code/00_colors.R
# Central color definitions — EDIT THESE to your real hex codes.
# Every panel script sources this file so colors stay consistent
# and are changed in exactly one place.
# ============================================================
# ---- Treatment arm ----
arm_colors <- c(
  "Nivo"     = "#9EBCDA",
  "Nivo-Ipi" = "#ADDD8E",
  "TKI"      = "#FEB24C"
)

# ---- Best response (RECIST) ----
response_colors <- c(
  "CR" = "#089392",
  "PR" = "#6CC382",
  "SD" = "#EAE29C",
  "PD" = "#CF597E",
  "NE" = "grey80"
)

rep_2_colors<-c("#E6866A", "#40B48B")
# ---- ccRCC molecular group ----
mg_colors <- c(
  "G1" = "#7FC97F",
  "G2" = "#BEAED4",
  "G3" = "#FDC086",
  "G4" = "#FFFF99"
)

# ---- IMDC risk group ----
imdc_colors <- c(
  "Good"         = "#009B9F",
  "Intermediate" = "#F1F1F1",
  "Poor"         = "#C75DAA"
)

# ---- Sarcomatoid / Rhabdoid component (identical scheme for both) ----
sarcomatoid_binary_colors <- c(
  "Present" = "#79ABE2",
  "Absent"  = "#F9F9F9"
 # "NR"  = "gray"
)
rhabdoid_binary_colors <- sarcomatoid_binary_colors


# ---- TLS count category ----
tls_cat_colors <- c(
  "0"   = "#2B83BA",
  "1-2" = "#ABDDA4",
  "3-5" = "#FDAE61",
  ">5"  = "#D7191C"
)

# ---- "Both components" (Sarcomatoid/Rhabdoid combined) — STILL UNRESOLVED ----
# Not present in your annoCol — used only in panel C's 5th sub-panel
# (Both / None / Rhabdoid / Sarcomatoid). Replace before using panel C.
both_components_colors <- c(
  "Both_components"        = "#FFE419",  # PLACEHOLDER — replace
  "None"        = "#F2A65A",  # PLACEHOLDER — replace
  "Rhabdoid"    = "#E8734A",  # PLACEHOLDER — replace
  "Sarcomatoid" = "#C0392B"   # PLACEHOLDER — replace
)

# ---- Survival cutpoint (High/Low) colors, per arm — panels D-H ----
# Derived from brewer.pal(n+1, <arm-specific palette>): first color = High,
# second = Low, matching your original comp-based assignment logic.
survival_hilo_colors <- list(
  "Nivo"     = c("High" = "#8856A7", "Low" = "#9EBCDA"),
  "Nivo-Ipi" = c("High" = "#31A354", "Low" = "#ADDD8E"),
  "TKI"      = c("High" = "#F03B20", "Low" = "#FEB24C")
)

# --- Combined pheatmap annotation color list (matches your original annoCol) ---
# Full combined list (single source of truth, includes entries used
# elsewhere in the repo — e.g. TLS_cat, platform — not just Fig1 panel B)
annoCol_all <- list(
  arm         = arm_colors,
  Response    = response_colors,
  Sarcomatoid = sarcomatoid_binary_colors,
  IMDC        = imdc_colors,
  Rhabdoid    = rhabdoid_binary_colors,
  MG          = mg_colors,
  TLS_cat     = tls_cat_colors
)

# Panel-B-specific subset: only the annotations actually plotted here
annoCol_panel_b <- annoCol_all[c("arm", "Response", "Sarcomatoid", "IMDC", "Rhabdoid", "MG")]


#############For FIgure 2
# ============================================================

collection_type_colors <- c(
  "whole section" = "#8FCCB4",
  "biopsy"        = "#579C97"
)

prisma_ihc_colors <- c(
  "PRISMA-high" = "#7CBF7C",
  "PRISMA-low"  = "#6A8DB7"
)

lesion_category_colors <- c(
  "primary tumor" = "#3E4C6E",
  "meta"          = "#BFB170",
  "reccurence"    = "#FFE93F"
)

# fixed assignment, NOT order-derived from unique() anymore — safer,
# since this hardcodes the exact mapping you confirmed rather than
# relying on unique(meta$AnatomicalOrigin) returning tissues in a
# particular order
anatomical_origin_colors <- c(
  "kidney"    = "#1F78C8",
  "stomach"   = "#ff0000",
  "bone"      = "#33a02c",
  "ovary"     = "#6A33C2",
  "pleurae"   = "#ff7f00",
  "colon"     = "#565656",
  "brain"     = "#FFD700",
  "lung"      = "#a6cee3",
  "endocrine" = "#FB6496",
  "skin"      = "#b2df8a",
  "testes"    = "#CAB2D6"
)

# used only for the intermediate per-marker boxplots that generate
# Log2FC/p-values below — not rendered in the final panel A heatmap,
# but kept here in case you want to render them standalone later
immunesign_boxplot_colors <- c("PRISMA-high" = "#40B48B", "PRISMA-low" = "#E6866A")

###
gaba_group_colors <- c(
  "TLS positive Low_GABA"  = "#2E9FDF",
  "TLS positive High_GABA" = "#E7B800",
  "TLS negative"           = "#FF9F1C"
)
##
volcano_arm_colors <- c(
  "Nivo-Ipi" = "#1B9E77",
  "Nivo"     = "#53A3F4",
  "TKI"      = "#FF6347",
  "N.S."     = "gray"
)

#
color_groups_TLSb_total <- c(
  "TLS_total_Presence" = "#D7191C",
  "TLS_total_Absence"  = "#2B83BA"
)

##
color_groups_TLS_total_GABA <-gaba_group_colors
color_groups_TLSb_total_GABA <- setNames(
  adjustcolor(color_groups_TLS_total_GABA, alpha.f = 0.3),
  names(color_groups_TLS_total_GABA)
)

##
ae_colors <- c(
  "Nervous system disorders"                = "darkblue",
  "Renal and urinary disorders"              = "darkgreen",
  "Skin and subcutaneous tissue disorders"   = "pink",
  "Fatigue"                                   = "orange",
  "Endocrine disorders"                       = "purple"
)
