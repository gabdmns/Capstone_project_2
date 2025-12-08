#Loading packages
required_packages <- c(
  "haven",        # read .sav (SPSS) files
  "dplyr",        # data manipulation
  "tidyr",        # tidy data tools
  "purrr",        # functional programming
  "janitor",      # clean tables, crosstabs
  "sjlabelled",   # convert SPSS labelled → factor
  "ggplot2",      # visualization
  "GGally",       # ggpairs, exploratory plots
  "naniar",       # missing data visualization
  "VIM",          # alternative missing data plots
  "missForest",   # <- NEW: mixed-type imputation
  "FactoMineR",   # FAMD (mixed data PCA)
  "factoextra",   # visualizations for FAMD/clustering
  "cluster",      # PAM, silhouette
  "clusterCrit",  # CH, Dunn indices
  "clustertend",  # Hopkins, VAT (clusterability)
  "fpc",          # cluster stability (clusterboot)
  "ggrepel",      # nicer text labels
  "scales",       # formatting (percentages, numbers)
  "stringr",      # string operations
  "forcats",      # factor manipulation
  "knitr",
  "kableExtra"
)

# Install missing packages and load them
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}     

# Loading Data

url_sav  <- "https://github.com/gabdmns/Capstone_project_2/raw/main/PTE2024PROMPERU.sav"
tmp_file <- tempfile(fileext = ".sav")

download.file(
  url    = url_sav,
  destfile = tmp_file,
  mode   = "wb",
  method = "libcurl"  # <- clave en Windows moderno
)

data_sav <- read_sav(tmp_file)

nrow(data_sav)  # Number of observations
ncol(data_sav)  # Number of variables

# Data Wrangling


# NA percentage in each variable
na_pct <- sapply(data_sav, function(x) mean(is.na(x)) * 100)
# Filter variables >90% NA's
vars_mas_90_na <- na_pct[na_pct > 90]
# Count of variables with more than 90% NA'S
length(vars_mas_90_na)

# Variable selection

vars_final <- c("P01","P53_RANGO2","P54","P56","P57_1","P58","P59","P33","P34",
                "P33_34_RNG","P60","P61","P53_GENERACION","P43_1")
data_filtrada <- data_sav[, vars_final, drop = FALSE]

info_compacto <- tibble(
  variable  = vars_final,
  pregunta  = map_chr(
    vars_final,
    ~ {
      lab <- attr(data_sav[[.x]], "label")
      if (is.null(lab)) NA_character_ else lab
    }
  ),
  categorias = map_chr(
    vars_final,
    ~ {
      labs <- attr(data_sav[[.x]], "labels")
      if (is.null(labs)) return(NA_character_)
      paste(names(labs), collapse = " | ")
    }
  ),
  na_pct = na_pct[vars_final]
)

print(info_compacto)

# Correct Data type

data_fixed <- data_filtrada %>%
  # 1) labelled → factor
  mutate(across(
    where(~ sjlabelled::is_labelled(.x)),
    ~ sjlabelled::as_factor(.x, levels = "labels")
  )) %>%
  # 2) character → factor
  mutate(across(
    where(is.character),
    as.factor
  )) %>%
  # 3) Date → numeric
  mutate(across(
    where(~ inherits(.x, "Date")),
    ~ as.numeric(.x)
  )) %>%
  # 4) POSIXct → numeric
  mutate(across(
    where(~ inherits(.x, "POSIXt")),
    ~ as.numeric(.x)
  ))

# P59: add NS/NR category
if ("P59" %in% names(data_fixed) && is.factor(data_fixed[["P59"]])) {
  tmp <- as.character(data_fixed[["P59"]])
  tmp[is.na(tmp)] <- "NS/NR"
  data_fixed[["P59"]] <- factor(tmp)
}

# P43_1: log-transform and set invalids to NA
if ("P43_1" %in% names(data_fixed) && is.numeric(data_fixed[["P43_1"]])) {
  data_fixed <- data_fixed %>%
    mutate(
      P43_1 = ifelse(P43_1 <= 0, NA_real_, P43_1),
      P43_1 = log(P43_1)
    )
}

data_fixed_df <- as.data.frame(data_fixed)

colSums(is.na(data_fixed_df))

# Validation
which(sapply(data_fixed, is.list))  

# Handling Missing Values


# Add a helper numeric variable from a factor (e.g., age range)
data_fixed_df$helper_num <- as.numeric(data_fixed_df$P53_RANGO2)

set.seed(123)
mf_res <- missForest(data_fixed_df)

# Check OOB error
mf_res$OOBerror

# Update data

data_fixed_df_imputed <- mf_res$ximp

# Drop helper variable
data_fixed_df_imputed$helper_num <- NULL

# This is your final imputed dataset
data_fixed <- data_fixed_df_imputed


# Exploratory Data Analysis

# Distributions of key variables

# data_fixed (already cleaned, 14 vars)
df <- data_fixed

#  Categorical variables: barplots in a loop --------------------------
cat_vars <- c("P01", "P53_RANGO2", "P54", "P56", "P57_1",
              "P58", "P59", "P33", "P34", "P33_34_RNG",
              "P60", "P61", "P53_GENERACION")

for (v in cat_vars) {
  p <- df %>%
    count(.data[[v]]) %>%
    mutate(prop = n / sum(n)) %>%
    ggplot(aes(x = .data[[v]], y = prop)) +
    geom_col() +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
              vjust = -0.2, size = 3) +
    labs(
      title = paste("Distribution of", v),
      x = v, y = "Proportion"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# Numeric variable: total spending (P43_1) ---------------------------

# Histogram
ggplot(df, aes(x = P43_1)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Total Trip Spending (P43_1)",
    x = "Total spending (USD)", y = "Count"
  ) +
  theme_minimal()

# Histogram on log scale (to show skewness)
ggplot(df, aes(x = P43_1)) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  labs(
    title = "Distribution of Total Trip Spending (log scale)",
    x = "log10(Spending)", y = "Count"
  ) +
  theme_minimal()

# Boxplot for outliers
ggplot(df, aes(y = P43_1)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Total Trip Spending",
    y = "Total spending (USD)"
  ) +
  theme_minimal()



# Prepare data for FAMD (ensure unique rownames)

data_famd_use <- as.data.frame(data_fixed)
rownames(data_famd_use) <- paste0("ind_", seq_len(nrow(data_famd_use)))

# FAMD on data without NA
famd_res <- FAMD(data_famd_use, graph = FALSE)

# Individual coordinates (for clustering)
head(famd_res$ind$coord)

# Contribution of variables to each dimension
var_contrib <- famd_res$var$contrib
var_contrib

#  Distance + PAM + Silhouette (k = 2 to 10)
dist_famd <- dist(famd_res$ind$coord)

max_k <- 10
sil_famd <- numeric(max_k)

for (k in 2:max_k) {
  pam_fit     <- pam(dist_famd, k = k, diss = TRUE)
  sil_vals    <- silhouette(pam_fit$clustering, dist_famd)
  sil_famd[k] <- mean(sil_vals[, 3])
  cat("k =", k, "| Silhouette (FAMD+PAM) =", round(sil_famd[k], 4), "\n")
}

plot(2:max_k, sil_famd[2:max_k], type = "b",
     xlab = "Número de clusters (K)",
     ylab = "Silhouette promedio",
     main = "Selección de K usando Silhouette (FAMD + PAM)")

# Select optimal k (maximum silhouette between 2 and max_k)
k_opt <- which.max(sil_famd[2:max_k]) + 1  # +1 porque el vector empieza en k=2
cat("k óptimo elegido:", k_opt, "\n")

# CALINSKI-HARABASZ INDEX

# Convert FAMD coordinates to numeric matrix
X <- as.matrix(famd_res$ind$coord)

# Evaluate CH for k = 2 to 10
ch_values <- numeric(max_k)

for (k in 2:max_k) {
  pam_fit <- pam(dist_famd, k = k, diss = TRUE)
  
  ch_values[k] <- intCriteria(
    X,
    as.integer(pam_fit$clustering),
    c("calinski_harabasz")
  )$calinski_harabasz
}

plot(2:max_k, ch_values[2:max_k], type = "b",
     main = "Índice de Calinski–Harabasz",
     xlab = "Número de clusters (k)",
     ylab = "CH index")

# DUNN INDEX

dunn_values <- numeric(max_k)

for (k in 2:max_k) {
  pam_fit <- pam(dist_famd, k = k, diss = TRUE)
  
  dunn_values[k] <- intCriteria(
    X,
    as.integer(pam_fit$clustering),
    c("dunn")
  )$dunn
}

plot(2:max_k, dunn_values[2:max_k], type = "b",
     main = "Índice de Dunn",
     xlab = "Número de clusters (k)",
     ylab = "Dunn index")


#  Final clustering with PAM

pam_famd <- pam(dist_famd, k = 5, diss = TRUE)
clusters_famd <- factor(pam_famd$clustering)

table(clusters_famd)

# Final average silhouette
sil_famd_kopt <- silhouette(pam_famd$clustering, dist_famd)
mean_sil_kopt <- mean(sil_famd_kopt[, 3])
cat("Silhouette promedio (k_opt) =", round(mean_sil_kopt, 4), "\n")

# Add clusters to dataset
data_clust <- data_famd_use %>%
  mutate(cluster = clusters_famd)

table(data_clust$cluster)

# FAMD plot: individuals by cluster (with ggplot2)

# Using coordinates of the first two dimensions:
ind_coords <- as.data.frame(famd_res$ind$coord[, 1:2])
colnames(ind_coords) <- c("Dim1", "Dim2")
ind_coords$cluster <- clusters_famd

ggplot(ind_coords, aes(x = Dim1, y = Dim2, color = cluster)) +
  geom_point(alpha = 0.6, size = 1) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.95) +
  labs(
    title = "FAMD - Individuos por cluster (PAM sobre coordenadas FAMD)",
    x = "Dimensión 1",
    y = "Dimensión 2",
    color = "Cluster"
  ) +
  theme_minimal()

# Cluster Stability Analysis

# Wrapper function for PAM (required by clusterboot)
pam_5 <- function(x, k) {
  pam(x, k = k, diss = FALSE)$clustering
}

# Cluster stability analysis with bootstrap
set.seed(123)
cb <- clusterboot(
  X,
  B = 300,                 # number of bootstrap samples 
  bootmethod = "boot",     # resample rows with replacement
  clustermethod = pamkCBI, # PAM method compatible with clusterboot
  k = 5,                   # your selected number of clusters
  seed = 123,
  showplots = FALSE
)

cb


X <- as.matrix(famd_res$ind$coord)


# Cluster profiles (categorical variables)

perfil_var_cluster <- function(var_name) {
  tab <- table(data_clust[[var_name]], data_clust$cluster)
  prop_clust <- prop.table(tab, margin = 2)  # % dentro de cada cluster
  prop_total <- prop.table(tab)              # % global
  
  list(
    tabla            = tab,
    prop_por_cluster = round(prop_clust * 100, 1),
    prop_global      = round(prop_total * 100, 1)
  )
}

# Cluster profiles (categorical variables)

perfil_var_cluster <- function(var_name) {
  tab <- table(data_clust[[var_name]], data_clust$cluster)
  prop_clust <- prop.table(tab, margin = 2)  # % within each cluster
  prop_total <- prop.table(tab)              # % overall
  
  list(
    tabla            = tab,
    prop_por_cluster = round(prop_clust * 100, 1),
    prop_global      = round(prop_total * 100, 1)
  )
}

# Variables used in FAMD (same vars_final),
# excluding numeric P43_1 from categorical profiling:
vars_perfil <- intersect(
  c("P01", "P53_RANGO2", "P54", "P56", "P57_1", "P58",
    "P59", "P33", "P34", "P33_34_RNG", "P60", "P61", "P53_GENERACION"),
  names(data_clust)
)

# Create a list of profiles per variable
lista_perfiles <- lapply(vars_perfil, perfil_var_cluster)
names(lista_perfiles) <- vars_perfil

# --- FUNCTION TO EXTRACT DETAILED VARIABLE INFORMATION ---
info_variable <- function(v) {
  etiqueta   <- attr(data_sav[[v]], "label")
  categorias <- attr(data_sav[[v]], "labels")
  
  cat("\n============================\n")
  cat("Variable:", v, "\n")
  cat("Question:", ifelse(is.null(etiqueta), "NO LABEL AVAILABLE", etiqueta), "\n")
  
  if (!is.null(categorias)) {
    cat("Categories (original SPSS labels):\n")
    print(categorias)
  } else {
    cat("Categories: NOT APPLICABLE (numeric or no labels)\n")
  }
  
  cat("----------------------------\n")
}

# --- DISPLAY CLUSTER PROFILES FOR CATEGORICAL VARIABLES ---
for (v in vars_perfil) {
  info_variable(v)   # show variable name + question + categories
  
  cat("Percentage distribution by cluster (%):\n")
  print(lista_perfiles[[v]]$prop_por_cluster)
  
  cat("\n")
}

# Numerical summary per cluster

# FUNCTION TO SHOW NUMERIC VARIABLE INFO (NAME + QUESTION)
show_numeric_info <- function(var) {
  etiqueta <- attr(data_sav[[var]], "label")
  
  cat("\n============================\n")
  cat("Numeric variable:", var, "\n")
  cat("Question:", ifelse(is.null(etiqueta), "NO LABEL AVAILABLE", etiqueta), "\n")
  cat("----------------------------\n")
}

# Add original-scale spending (USD) for reporting
data_clust <- data_clust %>%
  mutate(
    P43_1_usd = exp(P43_1)  # back-transform from log
  )

# Optional: add a descriptive label for the back-transformed variable
attr(data_clust$P43_1_usd, "label") <- 
  "Total trip spending in USD (back-transformed from log)"

# Identify numeric variables in data_clust
num_vars <- names(data_clust)[sapply(data_clust, is.numeric)]

# Remove the log version of P43_1 from the summary (we only want original-scale USD)
num_vars <- setdiff(num_vars, "P43_1")

# Show info for numeric variables (only those that exist in data_sav or are P43_1_usd)
for (var in num_vars) {
  if (var %in% names(data_sav)) {
    show_numeric_info(var)
  } else if (var == "P43_1_usd") {
    cat("\n============================\n")
    cat("Numeric variable: P43_1_usd\n")
    cat("Question: Total trip spending in USD (back-transformed from log)\n")
    cat("----------------------------\n")
  }
}

# COMPUTE NUMERIC SUMMARY PER CLUSTER (using original-scale spending)
resumen_numerico <- data_clust %>%
  select(cluster, all_of(num_vars)) %>%
  group_by(cluster) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns  = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

cat("\nNUMERIC SUMMARY BY CLUSTER (original scale for spending):\n")
print(resumen_numerico)