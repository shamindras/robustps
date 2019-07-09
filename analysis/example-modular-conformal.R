# Background -------------------------------------------------------------------
# We develop here a modular framework for the conformal prediction scoring
# methodology. The final goal is to estimate robust prediction sets with
# coverage and lebesgue measure guarantees

# Setup ------------------------------------------------------------------------
# TODO: Delete this and move to package dependencies
# The xfun package needs to be separately installed (if not already on your
# system). So simply run the following line of code:
# install.packages(c("xfun", "here", "MASS", "tidyverse", "conflicted"))
pckgs <- c("here", "MASS", "tidyverse", "conflicted")
xfun::pkg_attach2(pckgs)

# Source GLOBAL files ----------------------------------------------------------
# We can source all R files from our core functions
fs::dir_ls(here::here("R"), glob = "*.R") %>%
    purrr::walk(.x = ., .f = ~base::source(file = .x))

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

# Generate Contaminated Multivariate normal Data
#' Generate specified number of samples from the required density function
#'
#' @param density_list
#' @param n_samples
#'
#' @return
#' @export
gen_density_samples_nd <- function(density_list, n_samples){

    density_type <- density_list$type

    if(density_type == "normal"){
        out <- MASS::mvrnorm(n = n_samples,
                             mu = density_list$mu_vec,
                             Sigma = density_list$sigma)
    } else {
        stop("We only accept normal as our density type currently")
    }

    base::return(out)
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

# Generate bivariate normal (BVN) data -----------------------------------------

# Core BVN parameters
NUM_OBS <- 1000
BIN_SPLIT_PROP <- 0.5

# Covariance matrix
I <- base::diag(x = c(1, 1)) # identity matrix
TRUE_SIGMA_MAT <- 2*I

# Contmination
# CONT_SIGMA_MAT <- base::matrix(c(2,0,0,2), 2, 2)
CONT_SIGMA_MAT <- 0.5*I

# mean vector
mu_vec <- c(0, 0)

# Generate the required multivariate normal data
bvn_sim_mat <- MASS::mvrnorm(n = NUM_OBS, mu = mu_vec, Sigma = I)

# Keep it in data frame format for convenience
bvn_sim_df <- bvn_sim_mat %>% tibble::as_tibble(x = .)

# Plot density plots on the sample data points
bvn_sim_df %>%
    ggplot2::ggplot(data = ., aes(x = V1, y = V2)) +
    ggplot2::geom_point() +
    ggplot2::geom_density_2d()

# Plot the contours empirically for the eps-contaminated bivariate normal
bvn_sim_cont_df <- gen_huber_samples_nd(density_p0 =
                                             list(type = "normal",
                                                  mu_vec = c(0, 0),
                                                  sigma = TRUE_SIGMA_MAT),
                                         density_q =
                                             list(type = "normal",
                                                  mu_vec = c(3, 3),
                                                  sigma = CONT_SIGMA_MAT),
                                         eps = EPSILON,
                                         n_samples = NUM_OBS) %>%
                        dplyr::mutate(.data = ., compts =
                                          forcats::as_factor(compts))

bvn_sim_cont_df %>%
    ggplot2::ggplot(data = ., aes(x = V1, y = V2, col = compts)) +
    ggplot2::geom_point() +
    ggplot2::geom_density_2d()

# Perform sample splitting of the input data -----------------------------------
# We perform a binary sample split of the input dataframe
bin_split_prop <- BIN_SPLIT_PROP
bin_split_props <- c(bin_split_prop, 1 - bin_split_prop)
n_obs <- base::nrow(x = bvn_sim_df)

# Get the split allocation for each component
compts <- base::sample(x = c(1:2), prob = bin_split_props, size = n_obs,
                       replace = TRUE)

# Randomly split the samples in 2 separate dataframes
split_smpls <- bvn_sim_df %>%
                dplyr::mutate(.data = ., compts = forcats::as_factor(compts)) %>%
                dplyr::group_by(.data = ., compts) %>%
                dplyr::group_split(.tbl = ., keep = FALSE)

# Compute some central measure on the first of the split datasets --------------
bvn_sim_samp1_df <- split_smpls[[1]]
bvn_sim_samp2_df <- split_smpls[[2]]

# Get the sample mean vector
get_means <- bvn_sim_df %>% smry_df_features(inp_df = ., smry_type = mean)

# Compute the distance measure on the second sample ----------------------------
mean_diff_vec <- bvn_sim_df %>%
                    purrr::map2_dfc(.x = ., .y = get_means, ~ .x - .y) %>%
                    purrr::pmap(., c, use.names = FALSE)
mean_diff_vec

# Get the 2-norm difference for the mean vector i.e. the phi function
mean_diff_vec_norms <- mean_diff_vec %>%
                        purrr::map_dbl(.x = .,
                                       .f = ~base::norm(x = .x, type = "2"))

# Compute the appropriate quantile threshold -----------------------------------
new_thresh_qtil <- (1 - ALPHA)
new_thresh_qtil <- mean_diff_vec_norms %>%
                    stats::quantile(x = .,
                                    probs = c(1 - new_thresh_qtil),
                                    na.rm = FALSE,
                                    names = FALSE)

# Apply the threshold and retrieve the confidence set  -------------------------
conf_set <- mean_diff_vec_norms[mean_diff_vec_norms >= new_thresh_qtil]

# DO this on the grid
# Grid x sequence
grid_x_by <- 0.2
grid_y_by <- 0.2

# Grid end-points (x, y)
grid_x_min <- -5
grid_x_max <- 5
grid_y_min <- -5
grid_y_max <- 5

# Grid sequence by axis
grid_x_seq <- base::seq(from = grid_x_min, to = grid_x_max, by = grid_x_by) %>%
                tibble::enframe(x = ., name = NULL, value = "x")
grid_y_seq <- base::seq(from = grid_y_min, to = grid_y_max, by = grid_y_by) %>%
                tibble::enframe(x = ., name = NULL, value = "y")

# Generate the grid
plt_grid <- grid_x_seq %>% tidyr::crossing(grid_y_seq)

# Apply the threshold to the validation set
# Not quite sure how we want to do this here specifically. We can discuss
# and finalize this
