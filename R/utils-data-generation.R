#' Generate specified number of samples from the required density function
#'
#' @param density_list
#' @param n_samples
#'
#' @return
#' @export
gen_density_samples <- function(density_list, n_samples = 100){

    density_type <- density_list$type

    if(density_type == "normal"){
        out <- stats::rnorm(n = n_samples,
                            mean = density_list$mean,
                            sd = density_list$sd)
    } else if(density_type == "exponential"){
        out <- stats::rexp(n = n_samples, rate = density_list$rate)
    } else if(density_type == "uniform"){
        out <- stats::runif(n = n_samples,
                            min = density_list$min,
                            max = density_list$max)
    } else if(density_type == "pareto"){
        out <- VGAM::rpareto(n = n_samples,
                             scale = density_list$scale,
                             shape = density_list$shape)
    } else if(density_type == "cauchy"){
        out <- stats::rcauchy(n = n_samples,
                              location = density_list$location,
                              scale = density_list$scale)
    }
    # Return the specified distribution function
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
gen_huber_samples <- function(density_p0 = list(type = "normal"
                                                , mean = 0, sd = 1),
                              density_q = list(type = "normal",
                                               mean = 0, sd = 1),
                              eps,
                              n_samples){

    components <- sample(1:2, prob = c(1-eps, eps),
                         size = n_samples, replace = TRUE)

    n_samples_p0 <- sum(components == 1)
    n_samples_q <- n_samples - n_samples_p0

    samples_p0 <- gen_density_samples(density_list = density_p0,
                                      n_samples = n_samples_p0)
    samples_q <- gen_density_samples(density_list = density_q,
                                     n_samples = n_samples_q)

    samples_huber <- numeric(length = n_samples)
    samples_huber[which(components == 1)] <- samples_p0
    samples_huber[which(components == 2)] <- samples_q

    base::return(samples_huber)
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
