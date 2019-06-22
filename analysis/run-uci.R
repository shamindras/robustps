# General Description ----------------------------------------------------------
# Here we will download several open-source binary classification datasets
# from the UCI machine learning repository. The goal is to construct
# semi-synthetic datasets i.e. take one of the binary classes as a "true" class
# and sample from the other binary class as a "contamination" class so that the
# empirical proportion of contamination/ true class number observations is held
# at a specified epsilon level

# Setup ------------------------------------------------------------------------
# TODO: Delete this and move to package dependencies
# The xfun package needs to be separately installed (if not already on your
# system). So simply run the following line of code:
# install.packages(c("xfun", "here", "MASS", "tidyverse", "conflicted"))
pckgs <- c("here", "tidyverse", "conflicted")
xfun::pkg_attach2(pckgs)

# Need to install the patchwork plot library through devtools
devtools::install_github("thomasp85/patchwork")

# Source GLOBAL files ----------------------------------------------------------
# We can source all R files from our core functions
fs::dir_ls(here::here("R"), glob = "*.R") %>%
    purrr::walk(.x = ., .f = ~base::source(file = .x))

# Global Parameters ------------------------------------------------------------
MAIN_UCI_DATA_URL <- stringr::str_c("https://archive.ics.uci.edu",
                                    "ml/machine-learning-databases/",
                                    sep = "/")

# UCI Banknote Data ------------------------------------------------------------
# Define the UCI Banknote data params
# url params
banknote_uci_extn <- "00267/data_banknote_authentication.txt"
banknote_uci_url <- stringr::str_c(MAIN_UCI_DATA_URL, banknote_uci_extn,
                                   collapse = "")

# Column Names and Column Types
banknote_col_names_raw <- c("VARIANCE_WAVELET","SKEWNESS_WAVELET",
                            "CURTOSIS_WAVELET", "ENTROPY_IMAGE", "CLASS")
banknote_col_types_raw = c("ddddf")

# Get the UCI banknote Data
bn_uci_dat <- get_uci_data(ucidurl = banknote_uci_url,
                           col_names_raw = banknote_col_names_raw,
                           col_types_raw = banknote_col_types_raw)

# bn_uci_dat %>%
#     ggplot2::ggplot(data = ., aes(x = variance_wavelet,
#                                   y = skewness_wavelet, col = class)) +
#     ggplot2::geom_point()
# dplyr::glimpse(bn_uci_dat)

# UCI Banknote Cleaned plots ---------------------------------------------------
# Note we have applied an extra filter here to split the classes further
# This is with the benefit of hindsight so that we get separated classes
bn_uci_dat2 <- bn_uci_dat %>%
                   dplyr::filter(!(class == "0" & variance_wavelet < 1.5))

ds_name <- "UCI Banknote Data"
plt_vars <- c("variance_wavelet", "skewness_wavelet", "class")
clss_vals <- c("1", "0")
clss_rnm_vals <- c(True = "1", Cont = "0")
# epsilon <- 0.1
epsilons <- base::seq(from = 0.1, to = 0.5, by = 0.1)
tot_nrows_req <- 200

# Create all plots in a list for all epsilon values
bn_uci_plts <- epsilons %>%
    purrr::map(.x = .,
               .f = ~get_plot_clss(ds = bn_uci_dat2,
                                   ds_name = "UCI Banknote Data",
                                   plt_vars =
                                       c("variance_wavelet",
                                         "skewness_wavelet",
                                         "class"),
                                   clss_vals = c("1", "0"),
                                   clss_rnm_vals = c(True = "1",
                                                     Cont = "0"),
                                   epsilon = .x,
                                   tot_nrows_req = 200))

# Display all plots in a grid
patchwork::wrap_plots(bn_uci_plts)

# bn_uci_dat %>%
#     ggplot2::ggplot(data = ., aes_string(x = plt_vars[1], y = plt_vars[2],
#                                          col = "class")) +
#     ggplot2::geom_point(size = 2.5)

# What do we want to do here
# Human readable x-y labels
# Header in ggplot2
# - Dataset name
# - Contamination Proportion
# - Total number of points
# >> UCI Banknote: epsilon = 0.2

# Create plots for varying epsilon proportions
# - NOTE: We will not necessarily sample a superset of the existing points as
#         epsilon increases

# UCI Hepatitis Data -----------------------------------------------------------
# Define the UCI Hepatitis data params

# # url params
# hepatitis_uci_extn <- "hepatitis/hepatitis.data"
# hepatitis_uci_url <- stringr::str_c(MAIN_UCI_DATA_URL, hepatitis_uci_extn,
#                                     collapse = "")
# # Column Names and Column Types
# hepatitis_col_names_raw <- c("CLASS", "AGE", "SEX", "STEROID", "ANTIVIRALS",
#                              "FATIGUE", "MALAISE", "ANOREXIA", "LIVER_FIRM",
#                              "LIVER_BIG", "SPLEEN_PALPABLE", "SPIDERS",
#                              "ASCITES", "VARICES", "BILIRUBIN", "ALK_PHOSPHATE",
#                              "SGOT", "ALBUMIN", "PROTIME", "HISTOLOGY")
# hepatitis_col_types_raw = c("fdffffffffffffdddddf")
#
# # Get the UCI Hepatitis Data
# hep_uci_dat <- get_uci_data(ucidurl = hepatitis_uci_url,
#                             col_names_raw = hepatitis_col_names_raw,
#                             col_types_raw = hepatitis_col_types_raw)
# dplyr::glimpse(hep_uci_dat)

# Hepatitis - Contamination sampling and combining -----------------------------
# plt_vars <- c("age", "albumin", "class")

# plt_vars <- c("age", "alk_phosphate", "class")
# hep_dat_resmpl <- get_split_clss(uci_dat = hep_uci_dat,
#                                  plt_vars = plt_vars,
#                                  clss_vals = c("1", "2"),
#                                  epsilon = 0.4,
#                                  tot_nrows_req = 100)
#
# hep_uci_dat %>%
#     ggplot2::ggplot(data = ., aes_string(x = plt_vars[1], y = plt_vars[2],
#                                          col = "class")) +
#     ggplot2::geom_point(size = 2.5)
#
# hep_dat_resmpl  %>%
#     ggplot2::ggplot(data = ., aes_string(x = plt_vars[1], y = plt_vars[2],
#                                          col = "class")) +
#     ggplot2::geom_point(size = 2.5)

# UCI Breast Cancer Data -------------------------------------------------------
# Define the UCI Breast Cancer data params
# url params

# brcancer_uci_extn <- "breast-cancer/breast-cancer.data"
# brcancer_uci_url <- stringr::str_c(MAIN_UCI_DATA_URL, brcancer_uci_extn,
#                                     collapse = "")
#
# # Column Names and Column Types
# brcancer_col_names_raw <- c("CLASS", "AGE", "MENOPAUSE", "TUMOR_SIZE",
#                             "INV_NODES", "NODE_CAPS", "DEG_MALIG", "BREAST",
#                             "BREAST_QUAD", "IRRADIAT")
# brcancer_col_types_raw = c("ffffffifff")
#
# # Get the UCI brcancer Data
# bc_uci_dat <- get_uci_data(ucidurl = brcancer_uci_url,
#                            col_names_raw = brcancer_col_names_raw,
#                            col_types_raw = brcancer_col_types_raw)
#
# bc_uci_dat %>%
#     ggplot2::ggplot(data = ., aes(x = age, y = inv_nodes, col = class)) +
#     ggplot2::geom_point()
