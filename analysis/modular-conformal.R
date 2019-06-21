# Background -------------------------------------------------------------------
# We develop here a modular framework for the conformal prediction scoring
# methodology. The final goal is to estimate robust prediction sets with
# coverage and lebesgue measure guarantees

# Setup ------------------------------------------------------------------------
# TODO: Delete this and move to package dependencies
# The xfun package needs to be separately installed (if not already on your
# system). So simply run the following line of code:
# install.packages("xfun")
pckgs <- c("here", "MASS", "tidyverse")
xfun::pkg_attach2(pckgs)

# Define GLOBAL variables ------------------------------------------------------

# Set seed for reproducibility
SEED_VAL <- 78655256
base::set.seed(seed = SEED_VAL)

# Define epsilon and alpha values
EPSILON <- 0.2
ALPHA <- 0.05

# Generate bivariate normal (BVN) data -----------------------------------------

# Core BVN parameters
num_obs <- 200

# Covariance matrix
I <- base::diag(x = c(1, 1)) # identity matrix
sigma_mat <- I
# sigma_mat <- base::matrix(c(10,3,3,2), 2, 2)

# mean vector
mu_vec <- c(3, 3)

# Generate the required multivariate normal data
bvn_sim_mat <- MASS::mvrnorm(n = num_obs, mu = mu_vec, Sigma = I)

# Keep it in data frame format for convenience
bvn_sim_df <- bvn_sim_mat %>% tibble::as_tibble(x = .)

# Plot density plots on the sample data points
bvn_sim_df %>%
    ggplot2::ggplot(data = ., aes(x = V1, y = V2)) +
    ggplot2::geom_point() +
    ggplot2::geom_density_2d()

# Generate Contaminated Multivariate normal Data -------------------------------
# TODO: Not too tricky, keep track of the contaminated versions in a separate
#       column



# TODO: Plot the contours empirically for the eps-contaminated bivariate normal

# Perform sample splitting of the input data -----------------------------------
bin_split_prop <- 0.5
bin_split_props <- c(bin_split_prop, 1 - bin_split_prop)
n_obs <- base::nrow(x = bvn_sim_df)

# Get the split allocation for each component
compts <- base::sample(x = c(1:2),
                      prob = bin_split_props,
                      size = n_obs,
                      replace = TRUE)

split_smpls <- bvn_sim_df %>%
                dplyr::mutate(.data = ., compts = forcats::as_factor(compts)) %>%
                dplyr::group_by(.data = ., compts) %>%
                dplyr::group_split(.tbl = ., keep = TRUE) %>%
                purrr::map_int(.x, .f = ~base::nrow(x = .x))

# Compute some central measure on the first of the split datasets --------------
mean_df_features <- function(inp_df){
    inp_df %>%
        dplyr::ungroup() %>%
        dplyr::summarise_all(.tbl = ., .funs = mean) %>%
        base::return(.)
}

# Compute the distance measure on the second sample ----------------------------
diff_vec <- function(inp_vec, diff_val){
    base::return(inp_vec - diff_val)
}

mean_diff_vec <- bvn_sim_df %>%
                    purrr::map2_dfc(.x = ., .y = get_means, ~ .x - .y) %>%
                    purrr::pmap(., c, use.names = FALSE)

mean_diff_vec

mean_diff_vec_norms <- mean_diff_vec %>%
                        purrr::map_dbl(.x = .,
                                       .f = ~base::norm(x = .x, type = "2"))

# Compute the appropriate quantile threshold -----------------------------------
new_thresh_qtil <- EPSILON*(1 - ALPHA)
new_thresh_qtil <- mean_diff_vec_norms %>%
                    stats::quantile(x = .,
                                    probs = c(1 - new_thresh_qtil),
                                    na.rm = FALSE,
                                    names = FALSE)

# Apply the threshold and retrieve the confidence set  -------------------------
conf_set <- mean_diff_vec_norms[mean_diff_vec_norms >= new_thresh_qtil]
