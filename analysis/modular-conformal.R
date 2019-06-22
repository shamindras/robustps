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

# Define GLOBAL functions ------------------------------------------------------

#' Get mean of all columns in a dataframe in the form of a tibble
#'
#' @param inp_df (dataframe) : Input dataframe with only numeric columns
#'
#' @return (tibble) : A 1 row tibble containing only the mean of each column of
#'                    the input dataframe
#' @export
mean_df_features <- function(inp_df){
    inp_df %>%
        dplyr::ungroup() %>%
        dplyr::summarise_all(.tbl = ., .funs = mean) %>%
        base::return(.)
}

#' Get median of all columns in a dataframe in the form of a tibble
#'
#' @param inp_df (dataframe) : Input dataframe with only numeric columns
#'
#' @return (tibble) : A 1 row tibble containing only the median of each column of
#'                    the input dataframe
#' @export
median_df_features <- function(inp_df){
    inp_df %>%
        dplyr::ungroup() %>%
        dplyr::summarise_all(.tbl = ., .funs = median) %>%
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

#' Generate specified number of samples from the required Huber density function
#' NOTE: we are generating samples for, f, g separately and then combining into
#' an h sample.
#' TODO: Consider creating h sample directly given f, g, epsilon
#'
#' @param density_p0
#' @param density_q
#' @param eps
#' @param n_samples
#'
#' @return
#' @export
gen_huber_samples_nd <- function(density_p0 = list(type = "normal",
                                                   mu_vec = 0, sigma = 1),
                                 density_q = list(type = "normal",
                                                  mu_vec = 5, sigma = 1),
                                 eps,
                                 n_samples){

    components <- sample(1:2, prob = c(1-eps, eps),
                         size = n_samples, replace = TRUE)

    n_samples_p0 <- sum(components == 1)
    n_samples_q <- n_samples - n_samples_p0

    samples_p0 <- gen_density_samples_nd(density_list = density_p0,
                                         n_samples = n_samples_p0)
    samples_q <- gen_density_samples_nd(density_list = density_q,
                                        n_samples = n_samples_q)

    # Convert to a single tibble
    samples_p0_df <- samples_p0 %>%
                        tibble::as_tibble(x = .) %>%
                        dplyr::mutate(.data = ., compts = 1)

    samples_q_df <- samples_q %>%
                        tibble::as_tibble(x = .) %>%
                        dplyr::mutate(.data = ., compts = 2)

    samples_huber <- dplyr::bind_rows(samples_p0_df, samples_q_df)

    base::return(samples_huber)
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
EPSILON <- 0.2
ALPHA <- 0.05

# Generate bivariate normal (BVN) data -----------------------------------------

# Core BVN parameters
NUM_OBS <- 200

# Covariance matrix
I <- base::diag(x = c(1, 1)) # identity matrix
sigma_mat <- I
# sigma_mat <- base::matrix(c(10,3,3,2), 2, 2)

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
                                                  sigma = I),
                                         density_q =
                                             list(type = "normal",
                                                  mu_vec = c(3, 3),
                                                  sigma = base::matrix(
                                                      c(10,3,3,2), 2, 2)),
                                         eps = EPSILON,
                                         n_samples = NUM_OBS)


bvn_sim_cont_df %>%
    ggplot2::ggplot(data = ., aes(x = V1, y = V2)) +
    ggplot2::geom_point() +
    ggplot2::geom_density_2d()

# Perform sample splitting of the input data -----------------------------------
# We perform a binary sample split of the input dataframe
bin_split_prop <- 0.5
bin_split_props <- c(bin_split_prop, 1 - bin_split_prop)
n_obs <- base::nrow(x = bvn_sim_df)

# Get the split allocation for each component
compts <- base::sample(x = c(1:2), prob = bin_split_props, size = n_obs,
                       replace = TRUE)

split_smpls <- bvn_sim_df %>%
                dplyr::mutate(.data = ., compts = forcats::as_factor(compts)) %>%
                dplyr::group_by(.data = ., compts) %>%
                dplyr::group_split(.tbl = ., keep = FALSE)
                # purrr::map_int(.x, .f = ~base::nrow(x = .x))

# Compute some central measure on the first of the split datasets --------------
bvn_sim_samp1_df <- split_smpls[[1]]
bvn_sim_samp2_df <- split_smpls[[2]]

get_means <- bvn_sim_samp1_df %>% mean_df_features(inp_df = .)

# Compute the distance measure on the second sample ----------------------------
mean_diff_vec <- bvn_sim_samp1_df %>%
                    purrr::map2_dfc(.x = ., .y = get_means, ~ .x - .y) %>%
                    purrr::pmap(., c, use.names = FALSE)

mean_diff_vec

# Get the 2-norm difference for the mean vector i.e. the phi function
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

# Apply the threshold to the validation set
# Not quite sure how we want to do this here specifically. We can discuss
# and finalize this
