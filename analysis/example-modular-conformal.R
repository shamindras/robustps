# Background -------------------------------------------------------------------
# We develop here a modular framework for the conformal prediction scoring
# methodology. The final goal is to estimate robust prediction sets with
# coverage and lebesgue measure guarantees

# Clean up ---------------------------------------------------------------------
rm(list = ls())
cat("\014")

# Setup ------------------------------------------------------------------------
# TODO: Delete this and move to package dependencies
# The xfun package needs to be separately installed (if not already on your
# system). So simply run the following line of code:
# install.packages(c("xfun", "here", "MASS", "tidyverse", "conflicted"))
pckgs <- c("here", "MASS", "tidyverse", "conflicted", "devtools")
xfun::pkg_attach2(pckgs)

# Source GLOBAL files ----------------------------------------------------------
# We can source all R files from our core functions
fs::dir_ls(here::here("R"), glob = "*.R") %>%
    purrr::walk(.x = ., .f = ~base::source(file = .x))

# devtools::install_local(path = here::here())

# Define GLOBAL functions ------------------------------------------------------

#' Get summary i.e. mean/median/sd of all columns in a dataframe in the form
#' of a tibble
#'
#' @param inp_df (dataframe) : Input dataframe with only numeric columns
#'
#' @return (tibble) : A 1 row tibble containing only the mean of each column of
#'                    the input dataframe
#' @export
smry_df_features <- function(inp_df, smry_type){
    inp_df %>%
        dplyr::ungroup() %>%
        dplyr::summarise_all(.tbl = ., .funs = {{ smry_type }}) %>%
        base::return(.)
}

#' Get differences between input vector and another vector of the same size
#'
#' @param inp_vec (double) : A in input numeric vector
#' @param diff_val (double) : A numeric vector of the same length as the input
#'                            vector
#'
#' @return (double) : A vector of the same length as the input vector which is
#'                    the input vector less the \code{diff_val} vector
#' @export
diff_vec <- function(inp_vec, diff_val){
    base::return(inp_vec - diff_val)
}

#' Takes row-by-row differences from a common vector of same length
#' as the number of columns in the original dataframe. Typical use case
#' is to take differences from a mean from each observation (i.e. row of
#' numeric values in the datframe). This will only work if the dataframe is
#' contains numeric values
#'
#' @param inp_df (data.frame) : A dataframe of numeric values
#' @param anchor_vec (vector) : A vector of numeric values of the same length
#' as the number of columns as \code{inp_df}
#'
#' @export
get_mean_diff_vec <- function(inp_df, anchor_vec){
    out_vec <- inp_df %>%
        purrr::transpose(.l = .) %>%
        purrr::map(.x = .,
                   .f = function(x){dvec <-  x - anchor_vec
                   dvec <- base::unname(obj = dvec)
                   base::return(x - anchor_vec)})
    base::return(out_vec)
}


#' Given a list of numeric vectors return a single numeric vector which contains
#' the specified norm of each of the numeric vector. This ouput numeric vector
#' will have the same length as the original input list of numeric vectors.
#'
#' @param mean_diff_vec (list) : A list of numeric vectors. They should be
#' of the same length
#' @param norm_type (character) : The specified norm type to take of each
#' numeric vector i.e. "2" calculates the 2-norm, "1" calculates the 1-norm and
#' "I" calculates the max-norm. Refer to \code{base::norm} for more details
#' on norm types.
#'
#' @export
get_mean_diff_vec_norms <- function(mean_diff_vec, norm_type = "2"){
    out_vec <- mean_diff_vec %>%
                purrr::map_dbl(.x = ., .f = ~base::norm(x = .x,
                                                        type = norm_type))

    base::return(out_vec)
}

#' Generates a 2 column data frame containing a cartesian product of a
#' sequence of x-values and y-values to form a rectangular grid of 2D pairs
#'
#' @param grid_x_by (double) : The length of the grid in the "x" variable
#' @param grid_y_by (double) : The length of the grid in the "y" variable
#' @param grid_x_min (double) : The minimum x-value for the grid
#' @param grid_x_max (double) : The maximum x-value for the grid
#' @param grid_y_min (double) : The minimum y-value for the grid
#' @param grid_y_max (double) : The maximum y-value for the grid
#' @param var1_nm (character) : The name of the first column, default is "x1"
#' @param var2_nm (character) : The name of the second column, default is "x2"
#'
#' @export
generate_2d_grid <- function(grid_x_by, grid_y_by,
                             grid_x_min, grid_x_max,
                             grid_y_min, grid_y_max,
                             var1_nm = "x1",
                             var2_nm = "x2"){
    # Grid sequence by axis
    grid_x_seq <- base::seq(from = grid_x_min, to = grid_x_max,
                            by = grid_x_by) %>%
        tibble::enframe(x = ., name = NULL, value = var1_nm)
    grid_y_seq <- base::seq(from = grid_y_min, to = grid_y_max,
                            by = grid_y_by) %>%
        tibble::enframe(x = ., name = NULL, value = var2_nm)

    # Generate the grid
    plt_grid <- grid_x_seq %>% tidyr::crossing(grid_y_seq)

    base::return(plt_grid)
}


# hist(gen_huber_samples_nd(density_p0 = list(type = "normal", mu_vec = 0, sigma = 1),
#                      density_q = list(type = "normal", mu_vec = 5, sigma = 1),
#                      eps = 0.2,
#                      n_samples = 1000))

# Define GLOBAL variables ------------------------------------------------------

# Set seed for reproducibility
SEED_VAL <- 78655256
base::set.seed(seed = SEED_VAL)

# Define epsilon and alpha values
EPSILON <- 0.1
ALPHA <- 0.05

# DEFINE Global variables ------------------------------------------------------
# Core BVN parameters
NUM_OBS <- 500
BIN_SPLIT_PROP <- 0.5

# General useful variables
I <- base::diag(x = c(1, 1)) # identity matrix

# TRUE values
# mean vector
TRUE_MU_VEC <- c(0, 0)
# Covariance matrix
TRUE_SIGMA_MAT <- 2*I

# Contmination
# mean vector
CONT_MU_VEC <- c(2, 2)
# CONT_SIGMA_MAT <- base::matrix(c(2,0,0,2), 2, 2)
CONT_SIGMA_MAT <- 0.5*I

# Default column names
BVN_NVARS <- 2
BVN_COLNAMES <- base::seq.int(from = 1, to = BVN_NVARS, by = 1) %>%
                    purrr::map_chr(.x = .,
                                   .f = ~stringr::str_c("x", .x, sep = ""))

# TYPE I, TYPE_II error vars
NUM_OBS_TYPE_I_ERR <- 1000
NUM_OBS_TYPE_II_ERR <- NUM_OBS_TYPE_I_ERR

# Generate UNCONTAMINATED bivariate normal (BVN) data --------------------------

# Generate the required multivariate normal data
bvn_sim_mat <- MASS::mvrnorm(n = NUM_OBS, mu = TRUE_MU_VEC,
                             Sigma = TRUE_SIGMA_MAT)

# Keep it in data frame format for convenience
bvn_sim_df <- bvn_sim_mat %>%
                tibble::as_tibble(x = .) %>%
                magrittr::set_colnames(x = ., value = BVN_COLNAMES)

# Plot density plots on the sample data points
bvn_sim_df %>%
    ggplot2::ggplot(data = ., aes(x = x1, y = x2)) +
    ggplot2::geom_point() +
    ggplot2::geom_density_2d()

# Generate CONTAMINATED bivariate normal (BVN) data ----------------------------

# Plot the contours empirically for the eps-contaminated bivariate normal
bvn_sim_cont_df <- gen_huber_samples_nd(density_p0 =
                                             list(type = "normal",
                                                  mu_vec = TRUE_MU_VEC,
                                                  sigma = TRUE_SIGMA_MAT),
                                         density_q =
                                             list(type = "normal",
                                                  mu_vec = CONT_MU_VEC,
                                                  sigma = CONT_SIGMA_MAT),
                                         eps = EPSILON,
                                         n_samples = NUM_OBS) %>%
                        magrittr::set_colnames(x = .,
                                               value = c(BVN_COLNAMES, "compts")) %>%
                        dplyr::mutate(.data = ., compts =
                                          forcats::as_factor(compts))

# Plot density plots on the sample data points
bvn_sim_cont_df %>%
    ggplot2::ggplot(data = ., aes(x = x1, y = x2, col = compts)) +
    ggplot2::geom_point() +
    ggplot2::geom_density_2d()


# TODO: Insert sample-splitting code here

# SCORE Function - distance to mean
# Get the sample mean vector
get_means <- bvn_sim_df %>% smry_df_features(inp_df = ., smry_type = mean)

# Compute the distance measure on the second sample ----------------------------
mean_diff_vec <- get_mean_diff_vec(inp_df = bvn_sim_df, anchor_vec = get_means)

# Get the 2-norm difference for the mean vector i.e. the phi function
mean_diff_vec_norms <- get_mean_diff_vec_norms(mean_diff_vec = mean_diff_vec,
                                               norm_type = "2")

# Compute the appropriate quantile threshold -----------------------------------
des_covg_prob <- (1 - ALPHA)
new_thresh_qtil <- mean_diff_vec_norms %>%
                    stats::quantile(x = .,
                                    probs = c(des_covg_prob),
                                    na.rm = FALSE,
                                    names = FALSE,
                                    type = 3) # This is to get the order
                                              # statistic, no interpolation

# Apply the threshold and retrieve the confidence set  -------------------------
# Generate the grid
plt_grid <- generate_2d_grid(grid_x_by = 0.2, grid_y_by = 0.2,
                             grid_x_min = -8, grid_x_max = 8,
                             grid_y_min = -8, grid_y_max = 8,
                             var1_nm = "x1",
                             var2_nm = "x2")

mean_diff_vec_grid <- get_mean_diff_vec(inp_df = plt_grid,
                                        anchor_vec = get_means)

mean_diff_vec_norms_grid <- get_mean_diff_vec_norms(
                                mean_diff_vec = mean_diff_vec_grid,
                                norm_type = "2")

conf_set_grid <- mean_diff_vec_norms_grid[mean_diff_vec_norms_grid <=
                                              new_thresh_qtil]
length(conf_set_grid)

plt_grid_final <- plt_grid %>%
                    dplyr::mutate(dist_to_mean = mean_diff_vec_norms_grid,
                                  ind_conf_set =
                                      as.factor(dist_to_mean <= new_thresh_qtil))
bvn_sim_df %>%
    ggplot2::ggplot(data = ., aes(x = x1, y = x2)) +
    ggplot2::geom_point() +
    geom_point(data = plt_grid_final, aes(colour = ind_conf_set))

# TYPE I error
# Simulate from the true distribution
# i.e. set epsilon to 0
bvn_sim_typeI <- MASS::mvrnorm(n = NUM_OBS_TYPE_I_ERR, mu = TRUE_MU_VEC,
                               Sigma = TRUE_SIGMA_MAT)

# SAMPLE SPLITTING of the input data -------------------------------------------
# We perform a binary sample split of the input dataframe
bin_split_prop <- BIN_SPLIT_PROP
bin_split_props <- c(bin_split_prop, 1 - bin_split_prop)
n_obs <- base::nrow(x = bvn_sim_df)

# Get the split allocation for each component
compts <- base::sample(x = c(1:2), prob = bin_split_props,
                       size = n_obs, replace = TRUE)

# Randomly split the samples in 2 separate dataframes
split_smpls <- bvn_sim_df %>%
                dplyr::mutate(.data = ., compts = forcats::as_factor(compts)) %>%
                dplyr::group_by(.data = ., compts) %>%
                dplyr::group_split(.tbl = ., keep = FALSE)

# Compute some central measure on the first of the split datasets --------------
bvn_sim_samp1_df <- split_smpls[[1]]
bvn_sim_samp2_df <- split_smpls[[2]]

