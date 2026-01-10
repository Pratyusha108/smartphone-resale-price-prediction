# CSDA 6010 ANALYTICS PRACTICUM - PROJECT 1
# Project: Used Smartphone Price Analytics
# Student: SAI PRATYUSHA GORAPALLI (ID: 4246209)
#
# This R script follows the project approach required as:
# 1) Project introduction
# 2) Business and analytics goals
# 3) Data preprocessing (definitions, exploration, missing, zeros)
# 4) Predictor analysis and relevancy
# 5) Dimension reduction (if needed)
# 6) Data transformation (if needed)
# 7) Data partitioning methods
# 8) Model selection
# 9) Model fitting (validation accuracy and test accuracy)
# 10) Report models performance
# 11) Model evaluation (selected models)
# 12) Observation and conclusion (printed tables for the report)

#=============================================================================================
# PART - 3  :   DATA PREPROCESSING
#=============================================================================================
# Loading the required libraries
library(ggplot2)
library(tidyverse)
library(forecast)

# COLLECTING/LOADING AND READING THE DATA

data <- read.csv("C:/Users/saipr/OneDrive/Desktop/Practicum/Project1/used_device_data.csv")
head(data)
str(data)
summary(data)

# Making it into a data frame
usedphones.df <- data
head(usedphones.df)
summary(usedphones.df)

# Choosing our target aligned with our business goal to find prices for the smartphones.
# target is our normalized_used_price because it aligns with our goal to find prices for used smartphones. 

#------------------------------------------------------------------------------------------------------------
# Data Quality Check

## 3.1 Missing-value audit across ALL variables  
data.qc <- data.frame(
  mean    = sapply(usedphones.df, mean,    na.rm = TRUE),
  sd      = sapply(usedphones.df, sd,      na.rm = TRUE),
  min     = sapply(usedphones.df, min,     na.rm = TRUE),
  max     = sapply(usedphones.df, max,     na.rm = TRUE),
  median  = sapply(usedphones.df, median,  na.rm = TRUE),
  length  = sapply(usedphones.df, length),
  miss.val= sapply(usedphones.df, function(x) sum(length(which(is.na(x)))))
)
data.qc

#--------------------------------------------------------------------------------------------------------
# Handling missing values
## 3.2 Drop rows that contain ANY missing (NA/NaN) values  

rows_all_na <- apply(usedphones.df, 1, function(z) all(is.na(z)))
cols_all_na <- sapply(usedphones.df, function(z) all(is.na(z)))
usedphones.clean <- usedphones.df[!rows_all_na, !cols_all_na]

# By keeping real zeros like front_camera_mp==0; remove only truly empty rows/columns.
# As front_camera_mp means the used smartphone doesn't have front camera, not that information is missing. 


#--------------------------------------------------------------------------------------------------------
## 3.3 Finding duplicated rows, then remove them.
dup.idx          <- which(duplicated(usedphones.clean))
usedphones.dups  <- usedphones.clean[dup.idx, ]
nrow(usedphones.dups)     # how many duplicates
usedphones.nodup <- usedphones.clean[!duplicated(usedphones.clean), ]

#----------------------------------------------------------------------------------------------------------

## 3.4 Identify numeric columns (sapply pattern from Tbl 4.3)
num.cols <- names(usedphones.nodup)[sapply(usedphones.nodup, is.numeric)]
num.cols


#----------------------------------------------------------------------------------------------------------

## 3.5 Zero counts in numeric columns 
zero.counts <- sapply(usedphones.nodup[, num.cols, drop = FALSE],
                      function(x) sum(x == 0, na.rm = TRUE))
zero.counts    # keep zeros (e.g., no front camera) as valid values


#----------------------------------------------------------------------------------------------------------


## 3.6 (optional, quick type check you’ll use later)  — factor/numeric scan
str(usedphones.nodup)


#----------------------------------------------------------------------------------------------------------

##DOING SOME VALUABLE EDA ABOVE BASIC EXPLORATION
library(tidyverse)

# numeric columns
num.cols <- names(usedphones.nodup)[sapply(usedphones.nodup, is.numeric)]

# long format for “one figure” faceting/stacked plots
long_num <- usedphones.nodup %>%
  select(all_of(num.cols)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

# --- Boxplots of all numeric variables (one graph) ---
ggplot(long_num, aes(x = variable, y = value)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  labs(title = "Boxplots of Numeric Variables", x = "", y = "")

# --- Unique values (count per column) ---
unique_counts <- sapply(usedphones.nodup, dplyr::n_distinct)
unique_counts
unique_one <- names(unique_counts[unique_counts == 1])  # columns with a single unique value
unique_one


## Setup
rm(list = ls())
options(stringsAsFactors = FALSE)

## Adjust the path if needed:

df <- data

cat("Columns found:\n")
print(names(df))
cat("\nPreview:\n")
print(utils::head(df, 5))

## Small helper to safely grab a column name from a list of candidates
pick_col <- function(candidates, data_names) {
  cand <- candidates[candidates %in% data_names]
  if (length(cand) == 0) return(NA_character_)
  cand[1]
}

## Convert likely categorical columns to factor if present
cat_cols <- c(
  pick_col(c("device_brand","brand"), names(df)),
  pick_col(c("os","operating_system"), names(df)),
  pick_col(c("4g","four_g","supports_4g"), names(df)),
  pick_col(c("5g","five_g","supports_5g"), names(df))
)
cat_cols <- cat_cols[!is.na(cat_cols)]
for (cc in cat_cols) df[[cc]] <- as.factor(df[[cc]])

## Identify numeric columns (continuous)
is_num <- sapply(df, is.numeric)
num_cols <- names(df)[is_num]

## Create an output folder for charts
out_dir <- "eda_charts"
if (!dir.exists(out_dir)) dir.create(out_dir)

## Simple function to save PNG plots
save_png <- function(filename, width=900, height=600, expr) {
  png(file.path(out_dir, filename), width=width, height=height)
  on.exit(dev.off(), add=TRUE)
  eval.parent(substitute(expr))
}

## ----- Color mapping (use your palette, variable-specific) -----
var_colors <- list(
  screen_size            = "lightblue",
  rear_camera_mp         = "orange",
  front_camera_mp        = "brown",
  internal_memory        = "aquamarine",
  ram                    = "red",
  battery                = "lightblue",
  weight                 = "orange",
  release_year           = "brown",
  days_used              = "aquamarine",
  normalized_used_price  = "red",
  normalized_new_price   = "aquamarine",
  used_price             = "red",
  new_price              = "aquamarine"
)

## =========================
## 1) Summary Tables (global)
## =========================
cat("\n===== SUMMARY (numeric) =====\n")
if (length(num_cols) > 0) {
  print(summary(df[num_cols]))
} else {
  cat("No numeric columns detected.\n")
}

cat("\n===== SUMMARY (categorical) =====\n")
if (length(cat_cols) > 0) {
  for (cc in cat_cols) {
    cat("\n--", cc, "--\n", sep="")
    tb <- table(df[[cc]], useNA="ifany")
    print(tb)
    cat("Proportions:\n")
    print(round(prop.table(tb), 3))
  }
} else {
  cat("No categorical columns detected.\n")
}

## =========================
## 2) Categorical Attributes
## =========================

## Device Brand  -> ORANGE bars
brand_col <- pick_col(c("device_brand","brand"), names(df))
if (!is.na(brand_col)) {
  cat("\n[Device Brand] Frequency & Proportions\n")
  tb <- table(df[[brand_col]], useNA="ifany")
  print(tb); print(round(prop.table(tb), 3))
  save_png("Fig_brand_bar.png", expr={
    par(mar=c(6,4,2,1))
    barplot(tb, las=2, main="Device Brand Distribution", ylab="Count",
            col="orange", border="black")
  })
}

## OS -> AQUAMARINE bars
os_col <- pick_col(c("os","operating_system"), names(df))
if (!is.na(os_col)) {
  cat("\n[OS] Frequency & Proportions\n")
  tb <- table(df[[os_col]], useNA="ifany")
  print(tb); print(round(prop.table(tb), 3))
  save_png("Fig_os_bar.png", expr={
    barplot(tb, main="OS Distribution", ylab="Count",
            col="aquamarine", border="black")
  })
}

## 4G -> RED bars
g4_col <- pick_col(c("4g","four_g","supports_4g"), names(df))
if (!is.na(g4_col)) {
  cat("\n[4G] Frequency & Proportions\n")
  tb <- table(df[[g4_col]], useNA="ifany")
  print(tb); print(round(prop.table(tb), 3))
  save_png("Fig_4g_bar.png", expr={
    barplot(tb, main="4G Support", ylab="Count",
            col="red", border="black")
  })
}

## 5G -> BROWN bars
g5_col <- pick_col(c("5g","five_g","supports_5g"), names(df))
if (!is.na(g5_col)) {
  cat("\n[5G] Frequency & Proportions\n")
  tb <- table(df[[g5_col]], useNA="ifany")
  print(tb); print(round(prop.table(tb), 3))
  save_png("Fig_5g_bar.png", expr={
    barplot(tb, main="5G Support", ylab="Count",
            col="brown", border="black")
  })
}

## =========================
## 3) Continuous Attributes
## =========================
cand_numeric <- c(
  "screen_size","rear_camera_mp","front_camera_mp",
  "internal_memory","ram","battery","weight",
  "release_year","days_used",
  "normalized_used_price","normalized_new_price",
  "used_price","new_price"
)
cont_cols <- cand_numeric[cand_numeric %in% num_cols]

if (length(cont_cols) > 0) {
  cat("\n[Continuous Attributes] Descriptive Stats\n")
  for (cc in cont_cols) {
    cat("\n--", cc, "--\n", sep="")
    x <- df[[cc]]
    stats <- c(
      n = sum(!is.na(x)),
      min = suppressWarnings(min(x, na.rm=TRUE)),
      q1 = suppressWarnings(quantile(x, 0.25, na.rm=TRUE)),
      median = suppressWarnings(median(x, na.rm=TRUE)),
      mean = suppressWarnings(mean(x, na.rm=TRUE)),
      q3 = suppressWarnings(quantile(x, 0.75, na.rm=TRUE)),
      max = suppressWarnings(max(x, na.rm=TRUE)),
      sd = suppressWarnings(sd(x, na.rm=TRUE))
    )
    print(round(stats, 3))
    
    ## choose color for this variable
    col_choice <- if (!is.null(var_colors[[cc]])) var_colors[[cc]] else "lightblue"
    
    ## Histogram
    save_png(paste0("Fig_hist_", cc, ".png"), expr={
      hist(x, main=paste("Distribution of", cc), xlab=cc,
           col=col_choice, border="black")
    })
    
    ## Boxplot
    save_png(paste0("Fig_box_", cc, ".png"), expr={
      boxplot(x, horizontal=TRUE, main=paste("Boxplot of", cc), xlab=cc,
              col=col_choice, border="black")
    })
  }
} else {
  cat("\nNo recognized continuous attributes found among common candidates.\n")
}

## =========================
## 4) Relationship Plots
## =========================

## Boxplots of price by OS (if present)
price_used_col <- pick_col(c("normalized_used_price","used_price"), names(df))
if (!is.na(price_used_col) && !is.na(os_col)) {
  save_png("Fig_box_usedprice_by_os.png", expr={
    ## color per group
    cols_os <- c("aquamarine","red","brown","orange")
    boxplot(df[[price_used_col]] ~ df[[os_col]],
            xlab="OS", ylab=price_used_col,
            main=paste("Used Price by", os_col),
            col=cols_os)
  })
}

## Boxplots of price by 5G (if present)
if (!is.na(price_used_col) && !is.na(g5_col)) {
  save_png("Fig_box_usedprice_by_5g.png", expr={
    boxplot(df[[price_used_col]] ~ df[[g5_col]],
            xlab="5G", ylab=price_used_col,
            main=paste("Used Price by", g5_col),
            col=c("red","aquamarine"))
  })
}

## Correlation matrix (numeric only)
if (length(num_cols) >= 2) {
  cat("\n[Correlation Matrix - numeric variables]\n")
  cm <- stats::cor(df[num_cols], use = "pairwise.complete.obs")
  print(round(cm, 3))
  ## Pairs plot (clean points; lightblue stands out well)
  save_png("Fig_pairs_numeric.png", width=1200, height=1200, expr={
    pairs(df[num_cols], main="Pairs Plot (Numeric Variables)",
          pch=19, col="lightblue")
  })
}

cat("\nDone. Charts saved to folder:", normalizePath(out_dir), "\n")


#==========================================================================================================
# PART -4  : Predictor Analysis and relevance 
#============================================================================================================
data <- read.csv("C:/Users/saipr/OneDrive/Desktop/Practicum/Project1/used_device_data.csv")

usedphones.df <-data
df <- usedphones.df

# scatter plot with trendlines and correlation 
plot_scatter <- function(
    x, y,
    xlab, ylab,
    fig_num,
    filename_stub,
    point_cex = 0.6
) {
  # keep complete pairs only
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  
  # correlation (Pearson)
  r <- suppressWarnings(cor(x, y, method = "pearson"))
  r_lab <- ifelse(is.finite(r), sprintf("r = %.3f", r), "r = NA")
  
  # open device
  fname <- file.path(out_dir, sprintf("Fig%02d_%s.png", fig_num, filename_stub))
  png(fname, width = 1400, height = 1000, res = 150)
  par(mar = c(5, 5, 3.5, 2))
  
  # base scatter
  plot(
    x, y,
    pch = 16, cex = point_cex,
    col = rgb(0,0,0,0.5),
    xlab = xlab, ylab = ylab,
    main = sprintf("Fig %d: %s vs %s", fig_num, xlab, ylab)
  )
  
  # add linear fit
  fit_lm <- lm(y ~ x)
  abline(fit_lm, lwd = 2)
  
  # add LOESS smooth
  lines(stats::lowess(x, y, f = 2/3, iter = 3), lwd = 2)
  
  # add correlation annotation
  usr <- par("usr")
  text(x = usr[1] + 0.02*(usr[2]-usr[1]),
       y = usr[4] - 0.05*(usr[4]-usr[3]),
       labels = r_lab, adj = 0, cex = 1.1)
  
  dev.off()
  message("Saved: ", fname)
}
# Saving all the acquired results

# Fig1: Screen Size vs Normalized Used Price
plot_scatter(
  x = df$screen_size,
  y = df$normalized_used_price,
  xlab = "Screen Size (inches)",
  ylab = "Normalized Used Price",
  fig_num = 10,
  filename_stub = "screen_size_vs_norm_used_price"
)

# Fig2: Battery vs Normalized Used Price
plot_scatter(
  x = df$battery,
  y = df$normalized_used_price,
  xlab = "Battery Capacity (mAh)",
  ylab = "Normalized Used Price",
  fig_num = 11,
  filename_stub = "battery_vs_norm_used_price"
)

# Fig3: Front Camera MP vs Normalized Used Price
plot_scatter(
  x = df$front_camera_mp,
  y = df$normalized_used_price,
  xlab = "Front Camera (MP)",
  ylab = "Normalized Used Price",
  fig_num = 12,
  filename_stub = "front_cam_vs_norm_used_price"
)

# Fig4: Rear Camera MP vs Normalized Used Price
plot_scatter(
  x = df$rear_camera_mp,
  y = df$normalized_used_price,
  xlab = "Rear Camera (MP)",
  ylab = "Normalized Used Price",
  fig_num = 13,
  filename_stub = "rear_cam_vs_norm_used_price"
)

# Fig5: RAM vs Normalized Used Price
plot_scatter(
  x = df$ram,
  y = df$normalized_used_price,
  xlab = "RAM (GB)",
  ylab = "Normalized Used Price",
  fig_num = 14,
  filename_stub = "ram_vs_norm_used_price"
)

# Fig6: Release Year vs Normalized Used Price
plot_scatter(
  x = df$release_year,
  y = df$normalized_used_price,
  xlab = "Release Year",
  ylab = "Normalized Used Price",
  fig_num = 15,
  filename_stub = "release_year_vs_norm_used_price"
)

# Fig7: Weight vs Normalized Used Price
plot_scatter(
  x = df$weight,
  y = df$normalized_used_price,
  xlab = "Weight (grams)",
  ylab = "Normalized Used Price",
  fig_num = 16,
  filename_stub = "weight_vs_norm_used_price"
)

# Fig8: Internal Memory vs Normalized Used Price
plot_scatter(
  x = df$internal_memory,
  y = df$normalized_used_price,
  xlab = "Internal Memory (GB)",
  ylab = "Normalized Used Price",
  fig_num = 17,
  filename_stub = "internal_memory_vs_norm_used_price"
)

# Fig9: Days Used vs Normalized Used Price
plot_scatter(
  x = df$days_used,
  y = df$normalized_used_price,
  xlab = "Days Used",
  ylab = "Normalized Used Price",
  fig_num = 18,
  filename_stub = "days_used_vs_norm_used_price"
)

# Fig10: Normalized New Price vs Normalized Used Price
plot_scatter(
  x = df$normalized_new_price,
  y = df$normalized_used_price,
  xlab = "Normalized New Price",
  ylab = "Normalized Used Price",
  fig_num = 19,
  filename_stub = "norm_new_vs_norm_used_price"
)

# Save a correlations table to CSV ----------------------
cors <- sapply(
  c("screen_size","battery","front_camera_mp","rear_camera_mp","ram",
    "release_year","weight","internal_memory","days_used","normalized_new_price"),
  function(v) suppressWarnings(cor(df[[v]], df$normalized_used_price, use="complete.obs"))
)
cors_df <- data.frame(predictor = names(cors), correlation_with_target = as.numeric(cors))
write.csv(cors_df, file.path(out_dir, "correlations_with_target.csv"), row.names = FALSE)
message("Saved: ", file.path(out_dir, "correlations_with_target.csv"))



#==========================================================================================================
#      PART - 5 : DIMENSION REDUCTION
#==========================================================================================================

#==========================================================================================================
#      PART - 5 : DIMENSION REDUCTION
#==========================================================================================================

suppressPackageStartupMessages({
  library(corrplot)
  library(dplyr)
})

# Choosing the working data frame, this is to avoid loss of data which already happened previously with errors. 

# If `df` is not a data.frame, fall back to the latest cleaned versions making sure it keeps on. 
if (!exists("df") || !is.data.frame(df)) {
  if (exists("usedphones.nodup") && is.data.frame(usedphones.nodup)) {
    df <- usedphones.nodup
  } else if (exists("usedphones.clean") && is.data.frame(usedphones.clean)) {
    df <- usedphones.clean
  } else if (exists("usedphones.df") && is.data.frame(usedphones.df)) {
    df <- usedphones.df
  } else {
    stop("No working data frame found. Load `usedphones.df` earlier in the script.")
  }
}

# --- Build the numeric-variable list for THIS df ---
num_vars <- names(df)[sapply(df, is.numeric)]

# Remove numeric columns that are all-NA or zero-variance (prevents NaN / singularities)
good_num <- sapply(df[, num_vars, drop = FALSE], function(x) {
  sum(is.finite(x)) >= 2 && isTRUE(var(x, na.rm = TRUE) > 0)
})
num_vars <- names(good_num)[good_num]
stopifnot(length(num_vars) >= 2)  # need at least two numeric variables

# --- Correlation matrix + heatmap ---
cor_matrix <- cor(df[, num_vars, drop = FALSE], use = "pairwise.complete.obs")

png("Fig_correlation_matrix.png", width = 1200, height = 1000, res = 150)
corrplot(
  cor_matrix,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  tl.srt      = 45,
  addCoef.col = "black",
  number.cex  = 0.6,
  col         = colorRampPalette(c("red","white","blue"))(200)
)
title("Correlation Matrix of Smartphone Attributes", line = 2.5)
dev.off()

# --- Rank predictors vs target and keep those above a correlation threshold ---
target <- "normalized_used_price"
stopifnot(target %in% names(df))
preds  <- setdiff(num_vars, target)

cors <- sapply(preds, function(v)
  suppressWarnings(cor(df[[v]], df[[target]], use = "pairwise.complete.obs"))
)

cor_tbl <- data.frame(
  predictor               = preds,
  correlation_with_target = as.numeric(cors)
)
cor_tbl$abs_corr <- abs(cor_tbl$correlation_with_target)
cor_tbl <- cor_tbl[order(-cor_tbl$abs_corr), ]

# Keep predictors with |r| > 0.20 (and always keep the anchor, if present)
thresh   <- 0.20
keep_reg <- cor_tbl$predictor[!is.na(cor_tbl$abs_corr) & cor_tbl$abs_corr > thresh]
anchor   <- "normalized_new_price"
if (anchor %in% names(df) && !(anchor %in% keep_reg)) keep_reg <- c(anchor, keep_reg)

# Reduced frame for regression steps that follow
df_reg <- df[, c(keep_reg, target), drop = FALSE]

# --- Save helper artifacts (optional, useful for the report) ---
write.csv(cor_tbl, "dimension_reduction_correlations_ranked.csv", row.names = FALSE)
write.csv(df_reg, "df_reduced_for_regression.csv",          row.names = FALSE)

# --- Quick console prints for traceability ---
cat("\n=== Numeric variables used ===\n"); print(num_vars)
cat("\n=== Top correlations with target ===\n"); print(utils::head(cor_tbl, 10))
cat("\n=== Predictors kept for regression (|r| > ", thresh, ") ===\n", sep = ""); print(keep_reg)



#============================================================================================
# PART -6 : DATA TRANSFORMATION
#============================================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

#Choosing the working data frame, this is to avoid loss of data which already happened previously with errors. 
# Priority is usedphones.nodup -> usedphones.clean -> usedphones.df -> data
df_work <-
  if (exists("usedphones.nodup") && is.data.frame(usedphones.nodup)) {
    usedphones.nodup
  } else if (exists("usedphones.clean") && is.data.frame(usedphones.clean)) {
    usedphones.clean
  } else if (exists("usedphones.df") && is.data.frame(usedphones.df)) {
    usedphones.df
  } else if (exists("data") && is.data.frame(data)) {
    data
  } else {
    stop("No suitable data frame found. Make sure the earlier preprocessing created usedphones.nodup or usedphones.df.")
  }

# Building the list of usable numeric columns 
# Keeping numeric columns with at least 2 finite values and non-zero variance
num.cols <- names(df_work)[sapply(df_work, is.numeric)]
ok_num <- sapply(df_work[, num.cols, drop = FALSE], function(x) {
  sum(is.finite(x)) >= 2 && isTRUE(var(x, na.rm = TRUE) > 0)
})
num.cols <- names(ok_num)[ok_num]
stopifnot(length(num.cols) > 0)

#  Long format for facet plots 
long_num <- df_work %>%
  select(all_of(num.cols)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  filter(is.finite(value))

# ------------------------------------------------------------------------------------------
# A) Kernel density (KDE) for all numeric variables in one figure
# ------------------------------------------------------------------------------------------
p_kde <- ggplot(long_num, aes(x = value)) +
  geom_density() +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Kernel Density of Numeric Variables", x = NULL, y = "Density") +
  theme_bw(base_size = 11) +
  theme(strip.background = element_blank(),
        panel.grid = element_blank())

print(p_kde)
ggsave("EDA_kde_all.png", p_kde, width = 12, height = 10, dpi = 300)

# ------------------------------------------------------------------------------------------
# B) Distributions of Histogram + Density for all variables in one figure
# ------------------------------------------------------------------------------------------
p_hist <- ggplot(long_num, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey85", color = "white") +
  geom_density() +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Distributions of Numeric Variables", x = NULL, y = "Density") +
  theme_bw(base_size = 11) +
  theme(strip.background = element_blank(),
        panel.grid = element_blank())

print(p_hist)
ggsave("EDA_hist_density_all.png", p_hist, width = 12, height = 10, dpi = 300)

# ------------------------------------------------------------------------------------------
# C) Normality check of Q–Q plots for all variables in one figure
# ------------------------------------------------------------------------------------------
p_qq <- ggplot(long_num, aes(sample = value)) +
  stat_qq(size = 0.7, alpha = 0.6) +
  stat_qq_line(color = "red") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Q–Q Plots of Numeric Variables",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bw(base_size = 11) +
  theme(strip.background = element_blank(),
        panel.grid = element_blank())

print(p_qq)
ggsave("EDA_qq_all.png", p_qq, width = 12, height = 10, dpi = 300)

# ------------------------------------------------------------------------------------------
# Saving copies to a specific folder 
# ------------------------------------------------------------------------------------------
fig_dir <- "C:/Users/saipr/OneDrive/Desktop/Practicum/Project1/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

ggsave(file.path(fig_dir, "EDA_kde_all.png"),           p_kde,  width = 12, height = 10, dpi = 300)
ggsave(file.path(fig_dir, "EDA_hist_density_all.png"),  p_hist, width = 12, height = 10, dpi = 300)
ggsave(file.path(fig_dir, "EDA_qq_all.png"),            p_qq,   width = 12, height = 10, dpi = 300)

# Verify and open the folder (Windows only)
print(list.files(fig_dir))
if (.Platform$OS.type == "windows") {
  shell.exec(fig_dir)
}


## =============================================================================================================
## PART 7: DATA PARTIONING
## ============================================================================================================
library(dplyr)

stopifnot(exists("usedphones.df"))
target <- "normalized_used_price"

# make sure categoricals are factors (lm will dummy-code them automatically)
usedphones.df <- usedphones.df %>%
  mutate(across(where(is.character), as.factor))

set.seed(1)  

# 70% train, 15% validation, 15% test
n <- nrow(usedphones.df)
train.rows <- sample(row.names(usedphones.df), round(0.70 * n))
remain.rows <- setdiff(row.names(usedphones.df), train.rows)
valid.rows  <- sample(remain.rows, round(0.15 * n))
test.rows   <- setdiff(remain.rows, valid.rows)

train.df <- usedphones.df[train.rows, ]
valid.df <- usedphones.df[valid.rows, ]
test.df  <- usedphones.df[test.rows, ]

c(train = nrow(train.df), valid = nrow(valid.df), test = nrow(test.df))


#===========================================================================
####PART-9 MODEL FITTING######
#===========================================================================

#### MULTIPLE LINEAR REGRESSION 

library(dplyr)

stopifnot(exists("train.df"), exists("valid.df"), exists("test.df"))
outcome <- "normalized_used_price"

# Keeping the NUMERIC predictors only avoids factor-contrast issues.
num_cols_train <- names(train.df)[sapply(train.df, is.numeric)]
stopifnot(outcome %in% num_cols_train)
pred_cols <- setdiff(num_cols_train, outcome)

# Buildling the numeric only frames with the same columns across splits
keep_cols_valid <- intersect(pred_cols, names(valid.df))
keep_cols_test  <- intersect(pred_cols, names(test.df))

train.num <- train.df[, c(outcome, pred_cols), drop = FALSE]
valid.num <- valid.df[, c(outcome, keep_cols_valid), drop = FALSE]
test.num  <- test.df[,  c(outcome, keep_cols_test),  drop = FALSE]

# Make columns consistent across all three (in case valid/test lack a few)
common_pred <- Reduce(intersect, list(names(train.num), names(valid.num), names(test.num)))
common_pred <- setdiff(common_pred, outcome)

train.num <- train.num[, c(outcome, common_pred), drop = FALSE]
valid.num <- valid.num[, c(outcome, common_pred), drop = FALSE]
test.num  <- test.num[,  c(outcome, common_pred),  drop = FALSE]

# Remove only true NAs for the variables in use (zeros are valid values)
train.num <- train.num[complete.cases(train.num), , drop = FALSE]
valid.num <- valid.num[complete.cases(valid.num), , drop = FALSE]
test.num  <- test.num [complete.cases(test.num ), , drop = FALSE]

#Metrics
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))
mse  <- function(actual, pred) mean((actual - pred)^2)
r2   <- function(actual, pred) 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)

# Baseline outcome of normalized_new_price (anchor)
stopifnot("normalized_new_price" %in% names(train.num))
lm_base <- lm(as.formula(paste(outcome, "~ normalized_new_price")), data = train.num)

pred_base_valid <- predict(lm_base, newdata = valid.num)
# guard against any non-finite predictions (shouldn't happen, but safe)
keep_b <- which(is.finite(pred_base_valid) & is.finite(valid.num[[outcome]]))
yb <- valid.num[[outcome]][keep_b]; pb <- pred_base_valid[keep_b]

base_valid_MAE  <- mae (yb, pb)
base_valid_MSE  <- mse (yb, pb)
base_valid_RMSE <- rmse(yb, pb)
base_valid_R2   <- r2  (yb, pb)

# Full numeric MLR: outcome ~ .  (all numeric predictors)
lm_full <- lm(as.formula(paste(outcome, "~ .")), data = train.num)

pred_full_valid <- predict(lm_full, newdata = valid.num)
keep_f <- which(is.finite(pred_full_valid) & is.finite(valid.num[[outcome]]))
yf <- valid.num[[outcome]][keep_f]; pf <- pred_full_valid[keep_f]

full_valid_MAE  <- mae (yf, pf)
full_valid_MSE  <- mse (yf, pf)
full_valid_RMSE <- rmse(yf, pf)
full_valid_R2   <- r2  (yf, pf)

# Picking the model by lower VALIDATION RMSE, then evaluate on TEST once
use_full   <- (full_valid_RMSE <= base_valid_RMSE)
final_lm   <- if (use_full) lm_full else lm_base
chosen_lab <- if (use_full) "Full numeric MLR" else "Baseline (new price only)"

pred_test <- predict(final_lm, newdata = test.num)
keep_t <- which(is.finite(pred_test) & is.finite(test.num[[outcome]]))
yt <- test.num[[outcome]][keep_t]; pt <- pred_test[keep_t]

test_MAE  <- mae (yt, pt)
test_MSE  <- mse (yt, pt)
test_RMSE <- rmse(yt, pt)
test_R2   <- r2  (yt, pt)

# ---- 6) Compact result tables to drop in the report
valid_table <- data.frame(
  Model = c("Baseline (new price)", "Full numeric MLR"),
  MAE   = c(base_valid_MAE,  full_valid_MAE),
  MSE   = c(base_valid_MSE,  full_valid_MSE),
  RMSE  = c(base_valid_RMSE, full_valid_RMSE),
  R2    = c(base_valid_R2,   full_valid_R2)
)
print(valid_table)

test_table <- data.frame(
  Chosen_Model = chosen_lab,
  MAE  = test_MAE,
  MSE  = test_MSE,
  RMSE = test_RMSE,
  R2   = test_R2
)
print(test_table)

# ---- 7) (Optional) Inspect coefficients of the chosen model
summary(final_lm)




# ============================
# k-NN REGRESSION 
# ============================

library(dplyr)
library(caret)   
options(scipen = 999)

#Assumptions from earlier steps
stopifnot(exists("train.df"), exists("valid.df"), exists("test.df"))
outcome <- "normalized_used_price"

# Use the NUMERIC predictors only for distance
num_cols_train <- names(train.df)[sapply(train.df, is.numeric)]
stopifnot(outcome %in% num_cols_train)
pred_cols <- setdiff(num_cols_train, outcome)

# Drop the zero-variance predictors IN TRAIN (prevents sd=0 during scaling)
nzv_info <- nearZeroVar(train.df[, pred_cols, drop = FALSE], saveMetrics = TRUE)
drop_nzv <- rownames(nzv_info)[nzv_info$zeroVar]
if (length(drop_nzv)) {
  pred_cols <- setdiff(pred_cols, drop_nzv)
}

# Keeping complete cases for variables in use (zeros stay; only true NAs removed)
train.df <- train.df[complete.cases(train.df[, c(outcome, pred_cols)]), , drop = FALSE]
valid.df <- valid.df[complete.cases(valid.df[, c(outcome, pred_cols)]), , drop = FALSE]
test.df  <- test.df [complete.cases(test.df [, c(outcome, pred_cols)]), , drop = FALSE]

# NORMALIZE predictors to center & scale using TRAIN partition only
pre <- preProcess(train.df[, pred_cols, drop = FALSE], method = c("center", "scale"))
train.x <- predict(pre, train.df[, pred_cols, drop = FALSE])
valid.x <- predict(pre, valid.df[, pred_cols, drop = FALSE])
test.x  <- predict(pre, test.df [, pred_cols, drop = FALSE])

train.y <- train.df[[outcome]]
valid.y <- valid.df[[outcome]]
test.y  <- test.df [[outcome]]

# Metrics 
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))
mse  <- function(actual, pred) mean((actual - pred)^2)
r2   <- function(actual, pred) 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)

# Tune k on VALIDATION
k_grid <- c(3, 5, 7, 9, 11, 15, 21, 31)

valid_rows <- lapply(k_grid, function(k) {
  fit_k <- knnreg(x = train.x, y = train.y, k = k)
  pv    <- predict(fit_k, newdata = valid.x)
  data.frame(
    k    = k,
    MAE  = mae (valid.y, pv),
    MSE  = mse (valid.y, pv),
    RMSE = rmse(valid.y, pv),
    R2   = r2  (valid.y, pv)
  )
})
valid_tbl <- do.call(rbind, valid_rows)
valid_tbl <- valid_tbl[order(valid_tbl$RMSE), ]
print(valid_tbl)

best_k <- valid_tbl$k[1]
cat("Chosen k by lowest validation RMSE:", best_k, "\n")

# Plot of RMSE vs k
plot(valid_tbl$k, valid_tbl$RMSE, type = "b",
     xlab = "k (neighbors)", ylab = "Validation RMSE",
     main = "k-NN: Validation RMSE vs k")

# Final TEST evaluation with chosen k
fit_final <- knnreg(x = train.x, y = train.y, k = best_k)
pred_test <- predict(fit_final, newdata = test.x)

test_MAE  <- mae (test.y, pred_test)
test_MSE  <- mse (test.y, pred_test)
test_RMSE <- rmse(test.y, pred_test)
test_R2   <- r2  (test.y, pred_test)

test_table_knn <- data.frame(
  Model = paste0("k-NN (k = ", best_k, ")"),
  MAE   = test_MAE,
  MSE   = test_MSE,
  RMSE  = test_RMSE,
  R2    = test_R2
)
print(test_table_knn)

# ============================
# RANDOM FOREST REGRESSION
# ============================

library(dplyr)
library(randomForest)     
options(scipen = 999)

# Assumptions and outcome
stopifnot(exists("train.df"), exists("valid.df"), exists("test.df"))
outcome   <- "normalized_used_price"
pred_cols <- setdiff(names(train.df), outcome)  # using all predictors except the outcome variable

# Aligning factor levels across splits in trees need consistent levels.
fac_cols <- pred_cols[sapply(train.df[, pred_cols, drop = FALSE], is.factor)]
if (length(fac_cols)) {
  train.df[fac_cols] <- lapply(train.df[fac_cols], droplevels)   # drop unused in TRAIN
  for (cl in fac_cols) {
    lev <- levels(train.df[[cl]])
    if (cl %in% names(valid.df)) valid.df[[cl]] <- factor(valid.df[[cl]], levels = lev)
    if (cl %in% names(test.df))  test.df[[cl]]  <- factor(test.df[[cl]],  levels = lev)
  }
}

# Metric functions 
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))
mse  <- function(actual, pred) mean((actual - pred)^2)
r2   <- function(actual, pred) 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)

# Tuning mtry on the VALIDATION split
#    Textbook defaults for regression: mtry ≈ p/3; try a small grid around it
p <- length(pred_cols)
default_mtry <- max(floor(p/3), 1)
grid_mtry <- sort(unique(c(default_mtry, max(1, floor(sqrt(p))), max(1, floor(p/2)))))

# Fit a forest for each mtry, predict VALIDATION, collect metrics
set.seed(1)
valid_rows <- lapply(grid_mtry, function(mtry_val) {
  rf_fit <- randomForest(
    x = train.df[, pred_cols, drop = FALSE],
    y = train.df[[outcome]],
    ntree = 500,                 # stable number of trees
    mtry  = mtry_val,            # candidate value
    importance = TRUE,
    na.action = na.omit
  )
  pv <- predict(rf_fit, newdata = valid.df[, pred_cols, drop = FALSE])
  data.frame(
    mtry = mtry_val,
    MAE  = mae (valid.df[[outcome]], pv),
    MSE  = mse (valid.df[[outcome]], pv),
    RMSE = rmse(valid.df[[outcome]], pv),
    R2   = r2  (valid.df[[outcome]], pv)
  )
})
valid_rf_tbl <- do.call(rbind, valid_rows)
valid_rf_tbl <- valid_rf_tbl[order(valid_rf_tbl$RMSE), ]
print(valid_rf_tbl)

# Choosing the best mtry by lowest VALIDATION RMSE, refit on TRAIN, then TEST once
best_mtry <- valid_rf_tbl$mtry[1]

set.seed(1)
final_rf <- randomForest(
  x = train.df[, pred_cols, drop = FALSE],
  y = train.df[[outcome]],
  ntree = 500,
  mtry  = best_mtry,
  importance = TRUE,
  na.action = na.omit
)

pred_test <- predict(final_rf, newdata = test.df[, pred_cols, drop = FALSE])

test_table_rf <- data.frame(
  Model = paste0("Random Forest (mtry = ", best_mtry, ")"),
  MAE   = mae (test.df[[outcome]], pred_test),
  MSE   = mse (test.df[[outcome]], pred_test),
  RMSE  = rmse(test.df[[outcome]], pred_test),
  R2    = r2  (test.df[[outcome]], pred_test)
)
print(test_table_rf)




#########CLASSICATION#########
# ==========================================
# C5.0 DECISION TREE — CLASSIFICATION 
# ==========================================

library(dplyr)
library(C50)
library(caret)      
options(scipen = 999)

# Assumptions of train.df, valid.df, test.df already created from earlier split
stopifnot(exists("train.df"), exists("valid.df"), exists("test.df"))
outcome <- "normalized_used_price"

# Defining predictors of use all columns except the numeric target
pred_cols <- setdiff(names(train.df), outcome)

# (Optional safety) Align factor levels across splits for any factor predictors
fac_cols <- pred_cols[sapply(train.df[, pred_cols, drop = FALSE], is.factor)]
if (length(fac_cols)) {
  train.df[fac_cols] <- lapply(train.df[fac_cols], droplevels)
  for (cl in fac_cols) {
    lev <- levels(train.df[[cl]])
    if (cl %in% names(valid.df)) valid.df[[cl]] <- factor(valid.df[[cl]], levels = lev)
    if (cl %in% names(test.df))  test.df[[cl]]  <- factor(test.df[[cl]],  levels = lev)
  }
}

# Creating a classification label from THE SAME target (training median threshold)
thr <- median(train.df[[outcome]], na.rm = TRUE)

make_label <- function(df, thr) {
  factor(ifelse(df[[outcome]] >= thr, "High", "Low"), levels = c("Low","High"))
}

y_train <- make_label(train.df, thr)
y_valid <- make_label(valid.df, thr)
y_test  <- make_label(test.df,  thr)

# Buildling training data frame for C5.0 (predictors + label)
train_cl_df <- cbind(train.df[, pred_cols, drop = FALSE], PriceBand = y_train)

# Fit C5.0 decision tree (textbook style; no scaling needed)
set.seed(1)
c50_fit <- C5.0(PriceBand ~ ., data = train_cl_df)
summary(c50_fit)     # model size, rules, etc.

# VALIDATION performance (Accuracy, Sensitivity, Specificity)
valid_pred <- predict(c50_fit, newdata = valid.df[, pred_cols, drop = FALSE])
cm_valid   <- confusionMatrix(valid_pred, y_valid, positive = "High")
print(cm_valid)

# TEST performance once (final generalization estimate)
test_pred <- predict(c50_fit, newdata = test.df[, pred_cols, drop = FALSE])
cm_test   <- confusionMatrix(test_pred, y_test, positive = "High")
print(cm_test)


# ==========================================
# LOGISTIC REGRESSION — CLASSIFICATION 
# ==========================================

library(dplyr)
library(caret)    
options(scipen = 999)

# Assumptions and target
stopifnot(exists("train.df"), exists("valid.df"), exists("test.df"))
outcome <- "normalized_used_price"

# Predictors of all columns except the numeric outcome
pred_cols <- setdiff(names(train.df), outcome)

# Align factor levels across splits (prevents new-level errors)
fac_cols <- pred_cols[sapply(train.df[, pred_cols, drop = FALSE], is.factor)]
if (length(fac_cols)) {
  train.df[fac_cols] <- lapply(train.df[fac_cols], droplevels)
  for (cl in fac_cols) {
    lev <- levels(train.df[[cl]])
    if (cl %in% names(valid.df)) valid.df[[cl]] <- factor(valid.df[[cl]], levels = lev)
    if (cl %in% names(test.df))  test.df[[cl]]  <- factor(test.df[[cl]],  levels = lev)
  }
}

# Create a balanced classification label from TRAIN ONLY (median threshold)
thr <- median(train.df[[outcome]], na.rm = TRUE)

make_label <- function(df, thr) {
  factor(ifelse(df[[outcome]] >= thr, "High", "Low"), levels = c("Low","High"))
}
y_train <- make_label(train.df, thr)
y_valid <- make_label(valid.df, thr)
y_test  <- make_label(test.df,  thr)

# Build modeling frames (predictors + label)
train_cl <- cbind(train.df[, pred_cols, drop = FALSE], PriceBand = y_train)
valid_x  <- valid.df[, pred_cols, drop = FALSE]
test_x   <- test.df[,  pred_cols, drop = FALSE]

# Baseline logistic: PriceBand ~ normalized_new_price
stopifnot("normalized_new_price" %in% names(train_cl))
glm_base <- glm(PriceBand ~ normalized_new_price, data = train_cl, family = binomial)

# Predict probabilities on VALIDATION, then classify at 0.5 cutoff
p_valid_base <- predict(glm_base, newdata = valid_x, type = "response")
pred_valid_base <- factor(ifelse(p_valid_base >= 0.5, "High", "Low"), levels = c("Low","High"))

cm_valid_base <- confusionMatrix(pred_valid_base, y_valid, positive = "High")
print(cm_valid_base)

#  Full logistic: PriceBand ~ .  (all remaining predictors)
glm_full <- glm(PriceBand ~ ., data = train_cl, family = binomial)

p_valid_full <- predict(glm_full, newdata = valid_x, type = "response")
pred_valid_full <- factor(ifelse(p_valid_full >= 0.5, "High", "Low"), levels = c("Low","High"))

cm_valid_full <- confusionMatrix(pred_valid_full, y_valid, positive = "High")
print(cm_valid_full)

# Choosing the model with higher VALIDATION Accuracy; then TEST once
acc_base <- cm_valid_base$overall["Accuracy"]
acc_full <- cm_valid_full$overall["Accuracy"]

use_full <- is.finite(acc_full) && (acc_full >= acc_base)
final_glm   <- if (use_full) glm_full else glm_base
chosen_name <- if (use_full) "Logistic (Full)" else "Logistic (Baseline new price)"

# TEST evaluation
p_test <- predict(final_glm, newdata = test_x, type = "response")
pred_test <- factor(ifelse(p_test >= 0.5, "High", "Low"), levels = c("Low","High"))
cm_test <- confusionMatrix(pred_test, y_test, positive = "High")

cat("\nChosen model:", chosen_name, "\n\n")
print(cm_test)

# Viewing significant coefficients for interpretability
summary(final_glm)


