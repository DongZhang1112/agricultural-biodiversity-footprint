# ============================================================================
# Nature Food | Figure 2-5 R scripts
# ============================================================================
# Manuscript: Growing biodiversity footprints despite agricultural efficiency gains
# HOW TO USE:
# 1. Put this R script and the following TWO input files in the same directory:
#       TFP_BIIFP_analysis_ready.xlsx  (sheets: Annual_Panel, Country_Summary)
#       Strict_commodity_summary.csv
# 2. Set data_dir below if inputs are stored elsewhere, then run the full script.
# 3. PDFs/PNG are written to output_dir (created automatically).
#
# REQUIREMENTS:
#   R, packages: readxl, dplyr, tidyr, ggplot2, sf, rnaturalearth,
#   rnaturalearthdata, scales, patchwork; optional: ragg for PNG export.
# ============================================================================

data_dir <- Sys.getenv("NF_DATA_DIR", unset = ".")
output_dir <- file.path(data_dir, "figures")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
panel_candidates <- file.path(data_dir, c("TFP_BIIFP_analysis_ready.xlsx",
                                          "TFP_BIIFP_analysis_ready.xlsx"))
panel_exists <- panel_candidates[file.exists(panel_candidates)]
if (length(panel_exists) == 0L) {
  stop("Cannot find TFP_BIIFP_analysis_ready.xlsx in data_dir")
}
panel_xlsx <- panel_exists[1]
input_path <- file.path(data_dir, "Strict_commodity_summary.csv")
for (required_file in c(panel_xlsx, input_path)) {
  if (!file.exists(required_file)) {
    stop("Missing input: ", required_file,
         "\nPlease place it inside data_dir or edit data_dir at top of script.")
  }
}

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "sf",
  "rnaturalearth", "rnaturalearthdata", "scales", "patchwork"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "),
       "\nInstall with install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(patchwork)
library(grid)

message("Input directory: ", normalizePath(data_dir))
message("Output directory: ", normalizePath(output_dir))


# ============================================================================
# PART I: FIGURES 2-4 
# ============================================================================

# ============================================================
# COMMON SETUP FOR FIG. 2–4
# ============================================================
# Dependencies and paths are configured in the shared header.

# ------------------------------------------------------------
# 1. Read data
# ------------------------------------------------------------

annual <- read_excel(
  panel_xlsx,
  sheet = "Annual_Panel"
)

country <- read_excel(
  panel_xlsx,
  sheet = "Country_Summary"
)

# Main analytical sample
main_iso <- country$ISO3

annual_main <- annual %>%
  filter(ISO3 %in% main_iso)

n_country <- nrow(country)


# ------------------------------------------------------------
# 2. Unified colour system
# ------------------------------------------------------------

# Scale / output expansion
col_output <- "#E47B5B"

# Efficiency improvement
col_eff <- "#3E91A3"

# Observed aggregate outcome
col_outcome <- "#4A5560"

# Individual countries
col_country <- "#D6D9DC"

# No data
col_nodata <- "#E7E7E7"

# Neutral map midpoint
col_neutral <- "#F4F1EA"


# Decoupling classes
col_strong <- "#1F7184"
col_relative <- "#78B6BF"
col_no <- "#E47B5B"
col_other <- "#C9CDD1"


# ------------------------------------------------------------
# 3. Build country-level indices: 2000 = 100
# ------------------------------------------------------------

country_trend <- annual_main %>%
  select(
    ISO3, Country, Year,
    Output_CL_Q,
    Agricultural_land_km2,
    LI_ha_per_mUSD,
    BF_total_km2eq,
    BFI_haeq_per_mUSD
  ) %>%
  group_by(ISO3) %>%
  mutate(
    Output_idx =
      Output_CL_Q /
      Output_CL_Q[Year == 2000] * 100,
    
    Land_idx =
      Agricultural_land_km2 /
      Agricultural_land_km2[Year == 2000] * 100,
    
    LI_idx =
      LI_ha_per_mUSD /
      LI_ha_per_mUSD[Year == 2000] * 100,
    
    BF_idx =
      BF_total_km2eq /
      BF_total_km2eq[Year == 2000] * 100,
    
    BFI_idx =
      BFI_haeq_per_mUSD /
      BFI_haeq_per_mUSD[Year == 2000] * 100
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 4. Global trajectories
# ------------------------------------------------------------

global_trend <- annual_main %>%
  group_by(Year) %>%
  summarise(
    Output = sum(Output_CL_Q, na.rm = TRUE),
    Land = sum(Agricultural_land_km2, na.rm = TRUE),
    BF = sum(BF_total_km2eq, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    LI = Land / Output,
    BFI = BF / Output,
    
    Output_idx =
      Output / Output[Year == 2000] * 100,
    
    Land_idx =
      Land / Land[Year == 2000] * 100,
    
    LI_idx =
      LI / LI[Year == 2000] * 100,
    
    BF_idx =
      BF / BF[Year == 2000] * 100,
    
    BFI_idx =
      BFI / BFI[Year == 2000] * 100
  )


# ------------------------------------------------------------
# 5. World map
# ------------------------------------------------------------

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  filter(admin != "Antarctica") %>%
  mutate(
    map_iso3 = ifelse(
      is.na(iso_a3) | iso_a3 == "-99",
      adm0_a3,
      iso_a3
    )
  )


# ------------------------------------------------------------
# 6. Common theme
# ------------------------------------------------------------

theme_pub <- theme_classic(
  base_family = "sans",
  base_size = 10
) +
  theme(
    axis.text = element_text(
      colour = "black",
      size = 9
    ),
    
    axis.title = element_text(
      colour = "black",
      size = 10
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 11,
      hjust = 0
    ),
    
    plot.tag = element_text(
      face = "bold",
      size = 13
    ),
    
    legend.title = element_text(
      size = 9
    ),
    
    legend.text = element_text(
      size = 8.5
    )
  )
####################################
# ============================================================
# Annual change in land-use intensity
# ============================================================

map_LI <- world %>%
  left_join(
    country %>%
      select(
        ISO3,
        LI_trend_pctyr
      ),
    by = c("map_iso3" = "ISO3")
  )


n_LI_decline <- sum(
  country$LI_trend_pctyr < 0,
  na.rm = TRUE
)

pct_LI_decline <-
  100 * n_LI_decline / n_country


p2A <- ggplot(map_LI) +
  
  geom_sf(
    aes(fill = LI_trend_pctyr),
    linewidth = 0.12,
    colour = "white"
  ) +
  
  # ±4% beyond the range is saturated;
  # legend explicitly indicates this
  scale_fill_gradient2(
    low = "#1F7184",
    mid = col_neutral,
    high = "#C64F3F",
    
    midpoint = 0,
    
    limits = c(-4, 4),
    
    oob = squish,
    
    breaks = c(-4, -2, 0, 2, 4),
    
    labels = c(
      "\u2264\u22124",
      "\u22122",
      "0",
      "2",
      "\u22654"
    ),
    
    na.value = col_nodata,
    
    name =
      "Annual change in\nland-use intensity (%)"
  ) +
  
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  
  annotate(
    "text",
    x = -172,
    y = -50,
    label = paste0(
      n_LI_decline, "/",
      n_country, " countries (",
      round(pct_LI_decline), "%)\n",
      "showed declining land-use intensity"
    ),
    hjust = 0,
    family = "sans",
    size = 3.2,
    colour = col_eff
  ) +
  
  labs(
    tag = "A",
    title =
      "Annual change in land-use intensity, 2000\u20132020"
  ) +
  
  theme_void(
    base_family = "sans"
  ) +
  
  theme(
    plot.tag = element_text(
      face = "bold",
      size = 13
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.position = "bottom",
    
    legend.direction = "horizontal",
    
    legend.key.width = unit(
      25,
      "mm"
    ),
    
    legend.title = element_text(
      size = 9
    ),
    
    legend.text = element_text(
      size = 8
    ),
    
    plot.margin =
      margin(5, 5, 2, 5)
  )

# ============================================================
# Agricultural output
# ============================================================

n_output_up <- sum(
  country$Output_trend_pctyr > 0,
  na.rm = TRUE
)

pct_output_up <-
  100 * n_output_up / n_country


p2B1 <- ggplot() +
  
  geom_hline(
    yintercept = 100,
    linewidth = 0.35,
    linetype = "dashed",
    colour = "grey65"
  ) +
  
  # Individual countries
  geom_line(
    data = country_trend,
    aes(
      x = Year,
      y = Output_idx,
      group = ISO3
    ),
    colour = col_country,
    linewidth = 0.28,
    alpha = 0.40
  ) +
  
  # Global trajectory
  geom_line(
    data = global_trend,
    aes(
      x = Year,
      y = Output_idx
    ),
    colour = col_output,
    linewidth = 1.2
  ) +
  
  annotate(
    "text",
    x = 2020.35,
    y = global_trend$
      Output_idx[
        global_trend$Year == 2020
      ],
    
    label = "+54.9%",
    
    hjust = 0,
    family = "sans",
    fontface = "bold",
    size = 3.5,
    colour = col_output
  ) +
  
  annotate(
    "text",
    x = 2000.7,
    y = 285,
    
    label = paste0(
      n_output_up, "/",
      n_country, " countries (",
      round(pct_output_up), "%)\n",
      "showed positive output trends"
    ),
    
    hjust = 0,
    vjust = 1,
    
    family = "sans",
    size = 3,
    colour = "grey35"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      2000,
      2020,
      5
    ),
    limits = c(
      2000,
      2022
    )
  ) +
  
  coord_cartesian(
    ylim = c(
      45,
      310
    )
  ) +
  
  labs(
    title = "B1  Agricultural output",
    x = "Year",
    y = "Index (2000 = 100)"
  ) +
  
  theme_pub +
  
  theme(
    legend.position = "none"
  )

# ============================================================
# Agricultural land
# ============================================================

p2B2 <- ggplot() +
  
  geom_hline(
    yintercept = 100,
    linewidth = 0.35,
    linetype = "dashed",
    colour = "grey65"
  ) +
  
  geom_line(
    data = country_trend,
    aes(
      x = Year,
      y = Land_idx,
      group = ISO3
    ),
    colour = col_country,
    linewidth = 0.28,
    alpha = 0.40
  ) +
  
  geom_line(
    data = global_trend,
    aes(
      x = Year,
      y = Land_idx
    ),
    colour = col_outcome,
    linewidth = 1.2
  ) +
  
  annotate(
    "text",
    x = 2020.35,
    y = global_trend$
      Land_idx[
        global_trend$Year == 2020
      ],
    
    label = "\u22121.6%",
    
    hjust = 0,
    family = "sans",
    fontface = "bold",
    size = 3.5,
    colour = col_outcome
  ) +
  
  annotate(
    "text",
    x = 2000.7,
    y = 142,
    
    label =
      "Global land area\nremained broadly stable",
    
    hjust = 0,
    vjust = 1,
    
    family = "sans",
    size = 3,
    colour = "grey35"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      2000,
      2020,
      5
    ),
    limits = c(
      2000,
      2022
    )
  ) +
  
  coord_cartesian(
    ylim = c(
      65,
      145
    )
  ) +
  
  labs(
    title =
      "B2  Agricultural land",
    x = "Year",
    y = NULL
  ) +
  
  theme_pub +
  
  theme(
    legend.position = "none"
  )

# ============================================================
# Land-use intensity
# ============================================================

p2B3 <- ggplot() +
  
  geom_hline(
    yintercept = 100,
    linewidth = 0.35,
    linetype = "dashed",
    colour = "grey65"
  ) +
  
  geom_line(
    data = country_trend,
    aes(
      x = Year,
      y = LI_idx,
      group = ISO3
    ),
    colour = col_country,
    linewidth = 0.28,
    alpha = 0.40
  ) +
  
  geom_line(
    data = global_trend,
    aes(
      x = Year,
      y = LI_idx
    ),
    colour = col_eff,
    linewidth = 1.2
  ) +
  
  annotate(
    "text",
    x = 2020.35,
    y = global_trend$
      LI_idx[
        global_trend$Year == 2020
      ],
    
    label = "\u221236.5%",
    
    hjust = 0,
    
    family = "sans",
    fontface = "bold",
    
    size = 3.5,
    colour = col_eff
  ) +
  
  annotate(
    "text",
    x = 2000.7,
    y = 148,
    
    label = paste0(
      n_LI_decline,
      "/",
      n_country,
      " countries (",
      round(pct_LI_decline),
      "%)\n",
      "showed declining LI trends"
    ),
    
    hjust = 0,
    vjust = 1,
    
    family = "sans",
    size = 3,
    colour = "grey35"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      2000,
      2020,
      5
    ),
    limits = c(
      2000,
      2022
    )
  ) +
  
  coord_cartesian(
    ylim = c(
      30,
      155
    )
  ) +
  
  labs(
    title =
      "B3  Land-use intensity",
    x = "Year",
    y = NULL
  ) +
  
  theme_pub +
  
  theme(
    legend.position = "none"
  )
# ============================================================
# COMBINE
# ============================================================

p2B <- p2B1 + p2B2 + p2B3 +
  plot_layout(
    ncol = 3
  )


Fig2 <-
  p2A /
  p2B +
  
  plot_layout(
    heights = c(
      1.55,
      1
    )
  )


Fig2
ggsave(
  file.path(output_dir, "Fig2_final.pdf"),
  plot = Fig2,
  width = 11,
  height = 9,
  units = "in",
  device = "pdf"
)

#############################
# ============================================================
# Observed versus counterfactual biodiversity footprint,
#          with crop and livestock production pathways (2000-2020)
# All calculations below use the same 167-country annual panel as Fig. 2.
# ============================================================

required_fig3_cols <- c("Year", "ISO3", "Outcrop_Q", "Outanim_Q",
                        "BF_crop_km2eq", "BF_livestock_km2eq")
if (!all(required_fig3_cols %in% names(annual_main))) {
  stop("Fig. 3: missing Annual_Panel columns: ",
       paste(setdiff(required_fig3_cols, names(annual_main)), collapse = ", "))
}

# Aggregate sector totals BEFORE calculating footprint intensity; this avoids
# unintentionally averaging country-specific intensities.
fig3_sectors <- annual_main %>%
  group_by(Year) %>%
  summarise(
    Crop_output = sum(Outcrop_Q),
    Livestock_output = sum(Outanim_Q),
    Crop_BF = sum(BF_crop_km2eq),
    Livestock_BF = sum(BF_livestock_km2eq),
    .groups = "drop"
  ) %>%
  arrange(Year) %>%
  mutate(
    Crop_BFI = Crop_BF / Crop_output,
    Livestock_BFI = Livestock_BF / Livestock_output,
    Total_output = Crop_output + Livestock_output,
    Total_BF = Crop_BF + Livestock_BF,
    Total_BFI = Total_BF / Total_output
  )

if (nrow(fig3_sectors) != 21L ||
    !identical(as.integer(fig3_sectors$Year), 2000:2020)) {
  stop("Fig. 3 requires 21 complete annual observations (2000-2020).")
}
if (any(!is.finite(as.matrix(fig3_sectors[, -1]))) ||
    any(as.matrix(fig3_sectors[, c("Crop_output", "Livestock_output")]) <= 0)) {
  stop("Fig. 3 contains non-finite values or non-positive sector output.")
}

# Endpoint and panel checks: Fig. 3 totals must reconcile to the Fig. 2 totals.
if (max(abs(fig3_sectors$Total_output - global_trend$Output)) >
    1e-8 * max(global_trend$Output)) {
  stop("Fig. 3 total output does not reconcile to global_trend.")
}
if (max(abs(fig3_sectors$Total_BF - global_trend$BF)) >
    1e-8 * max(global_trend$BF)) {
  stop("Fig. 3 total BF does not reconcile to global_trend.")
}

base2000 <- fig3_sectors[fig3_sectors$Year == 2000, , drop = FALSE]
idx100 <- function(x) as.numeric(x / x[1] * 100)

# Panel A: fixed-BFI counterfactual (not a causal estimate)
fig3_global <- fig3_sectors %>%
  transmute(
    Year,
    Actual = idx100(Total_BF),
    Expected = idx100(Total_output),
    Lower = pmin(Actual, Expected),
    Upper = pmax(Actual, Expected)
  )
# Expected BF index is equivalent to (Y(t)*BFI(2000))/BF(2000)*100.
# This equality follows from BF(2000) = Y(2000)*BFI(2000).

fig3_check <- function(actual, target, metric) {
  if (abs(actual - target) > 0.25)
    stop("Fig. 3 check failed for ", metric, ": ",
         round(actual, 3), "% vs expected ", target, "%")
}
fig3_check(100*(tail(fig3_sectors$Total_output, 1)/base2000$Total_output - 1),
           54.9, "total agricultural output change")
fig3_check(100*(tail(fig3_sectors$Total_BF, 1)/base2000$Total_BF - 1),
           18.7, "total BF change")
fig3_check(100*(tail(fig3_sectors$Crop_BF, 1)/base2000$Crop_BF - 1),
           35.6, "crop BF change")
fig3_check(100*(tail(fig3_sectors$Livestock_BF, 1)/base2000$Livestock_BF - 1),
           1.9, "livestock BF change")

fig3_colors <- c("Agricultural output" = col_output,
                 "Actual biodiversity footprint" = col_outcome,
                 "Biodiversity footprint intensity" = col_eff)

p3A <- ggplot(fig3_global, aes(x = Year)) +
  geom_hline(yintercept = 100, colour = "grey75", linetype = "dashed",
             linewidth = .4) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              fill = "#DCEAF0", alpha = 0.85) +
  geom_line(aes(y = Expected, colour = "Expected biodiversity footprint"),
            linewidth = 1.0) +
  geom_line(aes(y = Actual, colour = "Actual biodiversity footprint"),
            linewidth = 1.0) +
  scale_colour_manual(values = c("Expected biodiversity footprint" = col_output,
                                 "Actual biodiversity footprint" = col_outcome),
                      breaks = c("Expected biodiversity footprint",
                                 "Actual biodiversity footprint")) +
  scale_x_continuous(breaks = seq(2000, 2020, 5), limits = c(2000, 2020)) +
  scale_y_continuous(breaks = seq(100, 160, 20), limits = c(98, 160)) +
  labs(title = "A", x = NULL, y = "Biodiversity-footprint index (2000 = 100)",
       colour = NULL) +
  theme_pub +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0),
        legend.position = c(.19, .87),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 8.5),
        axis.title.x = element_blank())

make_sector_panel <- function(sector = c("Crop", "Livestock"), title) {
  sector <- match.arg(sector)
  s_out <- fig3_sectors[[paste0(sector, "_output")]]
  s_bf <- fig3_sectors[[paste0(sector, "_BF")]]
  s_bfi <- fig3_sectors[[paste0(sector, "_BFI")]]
  d <- data.frame(
    Year = rep(fig3_sectors$Year, 3),
    Index = c(idx100(s_out), idx100(s_bf), idx100(s_bfi)),
    Metric = factor(rep(c("Agricultural output", "Actual biodiversity footprint",
                          "Biodiversity footprint intensity"), each = 21),
                    levels = names(fig3_colors))
  )
  ggplot(d, aes(x = Year, y = Index, colour = Metric)) +
    geom_hline(yintercept = 100, colour = "grey75",
               linetype = "dashed", linewidth = .4) +
    geom_line(linewidth = 1.0) +
    scale_colour_manual(values = fig3_colors, breaks = names(fig3_colors)) +
    scale_x_continuous(breaks = seq(2000, 2020, 5), limits = c(2000, 2020)) +
    scale_y_continuous(breaks = seq(60, 160, 20), limits = c(55, 165)) +
    labs(title = title, x = "Year", y = "Index (2000 = 100)", colour = NULL) +
    theme_pub +
    theme(plot.title = element_text(size = 11, hjust = 0),
          legend.position = "top", legend.direction = "vertical",
          legend.text = element_text(size = 7.4),
          legend.key.height = unit(0.25, "cm"))
}

p3B <- make_sector_panel("Crop", "B  Crop production")
p3C <- make_sector_panel("Livestock", "C  Livestock production")

Fig3 <- p3A / (p3B | p3C) + plot_layout(heights = c(0.80, 1.05))
print(Fig3)
ggsave(file.path(output_dir, "Fig3_final.pdf"), plot = Fig3,
       width = 11, height = 7.5, units = "in", device = "pdf")
if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_png(file.path(output_dir, "Fig3_final.png"),
                width = 11, height = 7.5, units = "in", res = 350)
  print(Fig3)
  dev.off()
}
message("Fig. 3 rebuilt from annual sector data: output, BF and BFI. ",
        "Reference: Fig. 3 in final manuscript (A-C).")

# ============================================================
# Biodiversity decoupling pathway map
# ============================================================

country <- country %>%
  mutate(
    Decoupling_trend =
      factor(
        Decoupling_trend,
        
        levels = c(
          "Strong",
          "Relative",
          "No",
          "Other"
        )
      )
  )


class_colours <- c(
  "Strong" = col_strong,
  "Relative" = col_relative,
  "No" = col_no,
  "Other" = col_other
)


counts <- table(
  country$Decoupling_trend
)

pct_class <-
  100 *
  counts /
  sum(counts)


legend_labels <- c(
  
  Strong = paste0(
    "Strong decoupling  ",
    round(
      pct_class["Strong"],
      1
    ),
    "% (n=",
    counts["Strong"],
    ")"
  ),
  
  Relative = paste0(
    "Relative decoupling  ",
    round(
      pct_class["Relative"],
      1
    ),
    "% (n=",
    counts["Relative"],
    ")"
  ),
  
  No = paste0(
    "No decoupling  ",
    round(
      pct_class["No"],
      1
    ),
    "% (n=",
    counts["No"],
    ")"
  ),
  
  Other = paste0(
    "Other transition  ",
    round(
      pct_class["Other"],
      1
    ),
    "% (n=",
    counts["Other"],
    ")"
  )
)


map_decoup <- world %>%
  
  left_join(
    country %>%
      select(
        ISO3,
        Decoupling_trend
      ),
    
    by =
      c(
        "map_iso3" =
          "ISO3"
      )
  )


p4A <- ggplot(
  map_decoup
) +
  
  geom_sf(
    aes(
      fill =
        Decoupling_trend
    ),
    
    linewidth = 0.12,
    colour = "white"
  ) +
  
  scale_fill_manual(
    
    values =
      class_colours,
    
    labels =
      legend_labels,
    
    na.value =
      col_nodata,
    
    drop = FALSE,
    
    name = NULL
  ) +
  
  coord_sf(
    xlim = c(
      -180,
      180
    ),
    
    ylim = c(
      -60,
      85
    ),
    
    expand = FALSE
  ) +
  
  labs(
    tag = "A",
    
    title =
      "Country-level biodiversity decoupling pathways, 2000\u20132020"
  ) +
  
  theme_void(
    base_family = "sans"
  ) +
  
  theme(
    plot.tag =
      element_text(
        face = "bold",
        size = 13
      ),
    
    plot.title =
      element_text(
        face = "bold",
        size = 11
      ),
    
    legend.position =
      "bottom",
    
    legend.direction =
      "horizontal",
    
    legend.text =
      element_text(
        size = 7.8
      )
  )
# ============================================================
# Biodiversity decoupling phase space
# ============================================================

# Plot ranges
xmin <- min(
  country$Output_trend_pctyr,
  na.rm = TRUE
)

xmax <- max(
  country$Output_trend_pctyr,
  na.rm = TRUE
)

ymin <- min(
  country$BF_trend_pctyr,
  na.rm = TRUE
)

ymax <- max(
  country$BF_trend_pctyr,
  na.rm = TRUE
)


# Add margins
xmin <- xmin - 0.3
xmax <- xmax + 0.3

ymin <- ymin - 0.3
ymax <- ymax + 0.3


# ------------------------------------------------------------
# Relative-decoupling polygon
#
# x > 0
# 0 < BF trend < Output trend
# ------------------------------------------------------------

diag_max <- min(
  xmax,
  ymax
)


relative_polygon <- data.frame(
  x = c(
    0,
    diag_max,
    xmax,
    xmax,
    0
  ),
  
  y = c(
    0,
    diag_max,
    diag_max,
    0,
    0
  )
)


# ------------------------------------------------------------
# No-decoupling polygon
#
# BF trend >= Output trend
# ------------------------------------------------------------

no_polygon <- data.frame(
  x = c(
    0,
    diag_max,
    xmax,
    xmax,
    0
  ),
  
  y = c(
    0,
    diag_max,
    diag_max,
    ymax,
    ymax
  )
)


p4B <- ggplot() +
  
  # ----------------------------------------------
# Background: Other transition
# ----------------------------------------------
annotate(
  "rect",
  
  xmin = xmin,
  xmax = 0,
  
  ymin = ymin,
  ymax = ymax,
  
  fill = col_other,
  alpha = 0.18
) +
  
  # ----------------------------------------------
# Strong decoupling
# Output > 0, BF < 0
# ----------------------------------------------
annotate(
  "rect",
  
  xmin = 0,
  xmax = xmax,
  
  ymin = ymin,
  ymax = 0,
  
  fill = col_strong,
  alpha = 0.12
) +
  
  # ----------------------------------------------
# Relative decoupling
# ----------------------------------------------
geom_polygon(
  data =
    relative_polygon,
  
  aes(
    x = x,
    y = y
  ),
  
  fill =
    col_relative,
  
  alpha =
    0.17
) +
  
  # ----------------------------------------------
# No decoupling
# ----------------------------------------------
geom_polygon(
  data =
    no_polygon,
  
  aes(
    x = x,
    y = y
  ),
  
  fill =
    col_no,
  
  alpha =
    0.13
) +
  
  # ----------------------------------------------
# Reference lines
# ----------------------------------------------
geom_vline(
  xintercept = 0,
  linewidth = 0.4,
  colour = "grey45"
) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    colour = "grey45"
  ) +
  
  geom_abline(
    intercept = 0,
    slope = 1,
    
    linewidth = 0.6,
    linetype = "dashed",
    colour = "grey45"
  ) +
  
  # ----------------------------------------------
# Countries
# ----------------------------------------------
geom_point(
  data = country,
  
  aes(
    x = Output_trend_pctyr,
    y = BF_trend_pctyr,
    colour = Decoupling_trend
  ),
  
  size = 2,
  alpha = 0.85,
  
  stroke = 0.2
) +
  
  scale_colour_manual(
    values =
      class_colours,
    
    guide = "none"
  ) +
  
  # ----------------------------------------------
# Region labels
# ----------------------------------------------
annotate(
  "text",
  
  x =
    xmin +
    0.12 *
    (0 - xmin),
  
  y =
    ymax -
    0.12 *
    (ymax - ymin),
  
  label =
    paste0(
      "OTHER TRANSITION\n",
      "Output not increasing\n",
      "n = ",
      counts["Other"],
      " (",
      round(
        pct_class["Other"],
        1
      ),
      "%)"
    ),
  
  hjust = 0,
  
  family = "sans",
  fontface = "bold",
  size = 3.2,
  
  colour = "grey40"
) +
  
  annotate(
    "text",
    
    x =
      xmax * 0.55,
    
    y =
      ymin +
      0.18 *
      (0 - ymin),
    
    label =
      paste0(
        "STRONG DECOUPLING\n",
        "Output \u2191, BF \u2193\n",
        "n = ",
        counts["Strong"],
        " (",
        round(
          pct_class["Strong"],
          1
        ),
        "%)"
      ),
    
    family = "sans",
    fontface = "bold",
    size = 3.2,
    
    colour =
      col_strong
  ) +
  
  annotate(
    "text",
    
    x =
      xmax * 0.52,
    
    y =
      ymax * 0.25,
    
    label =
      paste0(
        "RELATIVE DECOUPLING\n",
        "Output \u2191, BF \u2191 more slowly\n",
        "n = ",
        counts["Relative"],
        " (",
        round(
          pct_class["Relative"],
          1
        ),
        "%)"
      ),
    
    family = "sans",
    fontface = "bold",
    size = 3.2,
    
    colour =
      "#4D9299"
  ) +
  
  annotate(
    "text",
    
    x =
      xmax * 0.72,
    
    y =
      ymax * 0.78,
    
    label =
      paste0(
        "NO DECOUPLING\n",
        "BF grows as fast as or faster than output\n",
        "n = ",
        counts["No"],
        " (",
        round(
          pct_class["No"],
          1
        ),
        "%)"
      ),
    
    family = "sans",
    fontface = "bold",
    size = 3.2,
    
    colour =
      col_no
  ) +
  
  annotate(
    "text",
    
    x =
      diag_max * 0.72,
    
    y =
      diag_max * 0.79,
    
    label =
      "BF trend = output trend",
    
    family = "sans",
    size = 2.8,
    
    angle = 35,
    colour = "grey40"
  ) +
  
  coord_cartesian(
    xlim = c(
      xmin,
      xmax
    ),
    
    ylim = c(
      ymin,
      ymax
    )
  ) +
  
  labs(
    tag = "B",
    
    title =
      "Decoupling phase space across countries",
    
    x =
      "Annual change in agricultural output (%)",
    
    y =
      "Annual change in biodiversity footprint (%)"
  ) +
  
  theme_pub +
  
  theme(
    panel.grid =
      element_blank(),
    
    legend.position =
      "none"
  )
# ============================================================
# COMBINE 
# ============================================================

Fig4 <-
  p4A /
  p4B +
  
  plot_layout(
    heights = c(
      1.45,
      1
    )
  )


Fig4



# Added for reproducible export; original Fig2-4.R displayed Fig. 4 only.
ggsave(file.path(output_dir, "Fig4_final.pdf"),
       plot = Fig4, width = 11, height = 9, units = "in", device = "pdf")


# ============================================================================
# PART II: FIGURE 5 
# ============================================================================

# Commodity-specific biodiversity footprint change (strict matching)
# Input: Strict_commodity_summary.csv (in data_dir)
# Packages: ggplot2, patchwork. No Cairo/X11 device required.

# Dependencies and paths are configured in the shared header.
if (!file.exists(input_path)) stop('Cannot find: ', input_path, '\nChange data_dir above.')
s <- read.csv(input_path, fileEncoding = 'UTF-8-BOM', check.names = FALSE)

crops <- c('Maize', 'Wheat', 'Rice, paddy', 'Soybeans')
nice <- c('Maize', 'Wheat', 'Rice', 'Soybeans')
needed <- c('Commodity', 'N', 'BF_all_167_change', 'BF_2000', 'BF_change',
            'Production_change_pct', 'Intensity_change_pct', 'quantity_effect',
            'within_country_intensity_effect', 'between_country_production_share_effect')
if (!all(needed %in% names(s))) stop('Missing columns: ', paste(setdiff(needed, names(s)), collapse = ', '))
if (!all(crops %in% s$Commodity)) stop('Could not find all four crop names in input file.')
s <- s[match(crops, s$Commodity), , drop = FALSE]
stopifnot(!anyNA(s[, needed]))
residual <- s$BF_change - (s$quantity_effect + s$within_country_intensity_effect +
                            s$between_country_production_share_effect)
if (max(abs(residual)) > 1e-4) stop('Decomposition fails numerical reconciliation.')
expected_n <- c(137L, 117L, 106L, 85L)
actual_n <- as.integer(s$N)
if (anyNA(actual_n) || !all(actual_n == expected_n)) {
  stop("Matched sample sizes differ. Actual: ",
       paste(actual_n, collapse = ", "))
}

message("Sample sizes verified: ",
        paste(actual_n, collapse = ", "))

# Muted, print-friendly palette shared between B and C
col_production <- '#425B68'
col_footprint  <- '#B36B57'
col_intensity  <- '#43888A'
col_shares     <- '#BD9A62'
col_ink        <- '#29343B'
col_muted      <- '#68747B'

base_theme <- theme_classic(base_size = 10.5, base_family = 'sans') +
  theme(
    plot.title = element_text(face = 'bold', size = 11.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 9, colour = col_muted, margin = margin(b = 9)),
    axis.title = element_text(size = 9.5),
    axis.text = element_text(size = 9.5, colour = col_ink),
    axis.line = element_line(linewidth = .4, colour = col_ink),
    axis.ticks = element_line(linewidth = .35, colour = col_ink),
    legend.title = element_blank(),
    legend.text = element_text(size = 8.5),
    plot.margin = margin(8, 12, 10, 8)
  )

# A: full 167-country BF contributions; units: 10^3 km2-equivalent.
a <- data.frame(
  Crop = factor(nice, levels = rev(nice)),
  Change = s$BF_all_167_change / 1000
)
pA <- ggplot(a, aes(x = Change, y = Crop)) +
  geom_col(width = .58, fill = col_footprint) +
  geom_text(aes(label = sprintf('%.1f', Change)), hjust = -.18, size = 3.3,
            colour = col_ink) +
  scale_x_continuous(limits = c(0, 145), breaks = c(0, 40, 80, 120),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = 'A  Net biodiversity footprint increase',
       subtitle = 'Four crops account for 50.8% of global agricultural BF net growth',
       x = expression('Net BF increase ('*10^3*' km'^2*'-equivalent)'), y = NULL) +
  base_theme + theme(legend.position = 'none')

# B: strict-matching (137/117/106/85 countries), with a zero baseline.
b <- data.frame(
  Crop = rep(nice, 3),
  Metric = factor(rep(c('Production', 'BF', 'BF per tonne'), each = 4),
                  levels = c('Production', 'BF', 'BF per tonne')),
  Change = c(s$Production_change_pct,
             100 * s$BF_change / s$BF_2000,
             s$Intensity_change_pct)
)
b$Crop <- factor(b$Crop, levels = rev(nice))
b$Metric <- factor(b$Metric, levels = c('Production', 'BF', 'BF per tonne'))
pos <- position_dodge(width = .68)
pB <- ggplot(b, aes(x = Change, y = Crop, colour = Metric)) +
  geom_vline(xintercept = 0, linewidth = .5, colour = '#939BA0') +
  geom_point(position = pos, size = 2.7) +
  # Short horizontal stems improve visibility of small changes without clutter.
  geom_segment(aes(x = 0, xend = Change, yend = Crop),
               position = pos, linewidth = .55, alpha = .6) +
  scale_color_manual(values = c('Production' = col_production,
                                'BF' = col_footprint,
                                'BF per tonne' = col_intensity)) +
  scale_x_continuous(limits = c(-48, 136), breaks = c(-40, 0, 40, 80, 120),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = 'B  Production and footprint changes',
       subtitle = 'Strictly matched country samples, 2000–2020',
       x = 'Change (%)', y = NULL) +
  base_theme + theme(legend.position = 'bottom',
                     legend.direction = 'horizontal',
                     legend.text = element_text(size = 8)) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE))

# C: diverging grouped bars, with zero at the center.
cdat <- data.frame(
  Crop = rep(nice, 3),
  Component = factor(rep(c('Production quantity', 'Within-country intensity',
                           'Between-country share'), each = 4),
                     levels = c('Production quantity', 'Within-country intensity',
                                'Between-country share')),
  Value = c(s$quantity_effect, s$within_country_intensity_effect,
            s$between_country_production_share_effect) / 1000
)
cdat$Crop <- factor(cdat$Crop, levels = rev(nice))
pC <- ggplot(cdat, aes(x = Value, y = Crop, fill = Component)) +
  geom_vline(xintercept = 0, colour = '#5D666B', linewidth = .5) +
  geom_col(position = position_dodge(width = .72), width = .21) +
  scale_fill_manual(values = c('Production quantity' = col_production,
                               'Within-country intensity' = col_intensity,
                               'Between-country share' = col_shares)) +
  scale_x_continuous(limits = c(-66, 156), breaks = c(-50, 0, 50, 100, 150),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = 'C  Accounting decomposition of BF change',
       subtitle = 'Positive and negative contributions',
       x = expression('Contribution ('*10^3*' km'^2*'-equivalent)'), y = NULL) +
  base_theme + theme(legend.position = 'bottom', legend.direction = 'horizontal',
                     legend.text = element_text(size = 7.4)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

caption <- paste0(
  'A: full 167-country BF panel. B–C: positive production and recorded BF in both endpoints; ',
  'n = 137 (maize), 117 (wheat), 106 (rice), 85 (soybeans). ',
  'Decomposition is accounting, not causal.'
)
fig5 <- (pA / (pB | pC)) +
  plot_layout(heights = c(.83, 1.15)) +
  plot_annotation(caption = caption,
                  theme = theme(plot.caption = element_text(size = 8.1,
                                colour = col_muted, hjust = 0,
                                margin = margin(t = 12))))

print(fig5)
# pdf() avoids Cairo / XQuartz dependencies on many macOS R setups.
pdf(file.path(output_dir, 'Fig5_strict_revised.pdf'), width = 12.2,
    height = 8.0, useDingbats = FALSE, family = 'Helvetica')
print(fig5)
dev.off()

# Portable PNG: use ragg if installed, otherwise base png with Quartz on macOS.
if (requireNamespace('ragg', quietly = TRUE)) {
  ragg::agg_png(file.path(output_dir, 'Fig5_strict_revised.png'),
                width = 12.2, height = 8.0, units = 'in', res = 350)
  print(fig5)
  dev.off()
} else {
  tryCatch({
    png(file.path(output_dir, 'Fig5_strict_revised.png'),
        width = 12.2, height = 8.0, units = 'in', res = 350,
        type = if (identical(.Platform$OS.type, 'unix') && identical(Sys.info()[['sysname']], 'Darwin')) 'quartz' else 'cairo')
    print(fig5)
    dev.off()
  }, error = function(e) {
    message('PNG export unavailable. PDF saved. Install ragg: install.packages("ragg")')
    if (dev.cur() > 1) try(dev.off(), silent = TRUE)
  })
}
message('Saved figure files in: ', normalizePath(output_dir))
