
#' Get the level set for the given density type
#'
#' @param density_list
#' @param alpha
#'
#' @return
#' @export
get_known_level_set <- function(density_list, alpha){

    density_type <- density_list$type

    if(density_type == "normal"){
        end_point_lower <- density_list$mean -
                           density_list$sd*qnorm(p = 1 - alpha/2, mean = 0,
                                                 sd = 1)
        end_point_upper <- density_list$mean +
                           density_list$sd*qnorm(p = 1 - alpha/2, mean = 0,
                                                 sd = 1)
        optimal_level   <- dnorm(x = end_point_upper,
                                 mean = density_list$mean,
                                 sd = density_list$sd)
        out <- list(end_point_lower = end_point_lower,
                    end_point_upper = end_point_upper,
                    optimal_level = optimal_level,
                    lebesgue_measure = end_point_upper - end_point_lower)
    } else if(density_type == "cauchy"){
        end_point_lower <- qcauchy(p = -alpha/2,
                                   location = density_list$location,
                                   scale = density_list$scale)
        end_point_upper <- qcauchy(p = 1 - alpha/2,
                                   location = density_list$location,
                                   scale = density_list$scale)
        optimal_level   <- dcauchy(x = end_point_upper,
                                   location = density_list$location,
                                   scale = density_list$scale)
        out <- list(end_point_lower = end_point_lower,
                    end_point_upper = end_point_upper,
                    optimal_level = optimal_level,
                    lebesgue_measure = end_point_upper - end_point_lower)
    } else if(density_type == "exponential"){
        end_point_lower <- 0
        end_point_upper <- qexp(p = alpha, rate = density_list$rate)
        optimal_level   <- dexp(x = end_point_upper, rate = density_list$rate)
        out <- list(end_point_lower = end_point_lower,
                    end_point_upper = end_point_upper
                    , optimal_level = optimal_level
                    , lebesgue_measure = end_point_upper - end_point_lower)
    } else if(density_type == "pareto"){
        end_point_lower <- 0
        end_point_upper <- VGAM::qpareto(p = alpha,
                                   scale = density_list$scale,
                                   shape = density_list$shape)
        optimal_level   <- VGAM::dpareto(x = end_point_upper,
                                   scale = density_list$scale,
                                   shape = density_list$shape)
        out <- list(end_point_lower = end_point_lower,
                    end_point_upper = end_point_upper
                    , optimal_level = optimal_level
                    , lebesgue_measure = end_point_upper - end_point_lower)
    } else if(density_type == "uniform"){
        mid <- (density_list$min + density_list$max)/2
        tot_diff <- density_list$max - density_list$min
        half_rect_length <- tot_diff*((1 - alpha)/2)
        end_point_lower <- mid - half_rect_length
        end_point_upper <- mid + half_rect_length
        optimal_level   <- 1/tot_diff
        out <- list(end_point_lower = end_point_lower
                    , end_point_upper = end_point_upper
                    , optimal_level = optimal_level
                    , lebesgue_measure = end_point_upper - end_point_lower)
    }

    base::return(out)
}


#-------------------------------------------------------------------------------
# PART 3: Generate Level Set
#-------------------------------------------------------------------------------

#' Generate the level set for the Huber density
#'
#' @param huber_density
#' @param level
#' @param tol
#' @param grid_params
#'
#' @return
#' @export
get_level_set <- function(huber_density, level, tol, grid_params){

    grid_partition <- base::seq(from = grid_params$lower
                                , to = grid_params$upper
                                , by = grid_params$interval_length)

    huber_vals <- huber_density(grid_partition)

    level_set_ind <- (huber_vals - level > -1*tol)
    level_set_xvals <- grid_partition[level_set_ind]

    # Get the Connected Components in the Level sets
    connected_level_sets <- level_set_ind %>%
        base::as.integer() %>%
        stringr::str_c(... = ., collapse = "") %>%
        stringr::str_locate_all(string = .
                                , pattern = "1+") %>%
        purrr::pluck(.x = ., 1)

    end_point_lower <- grid_partition[connected_level_sets[, 1]]
    end_point_upper <- grid_partition[connected_level_sets[, 2]]

    out_list <- list(level_set_ind = level_set_ind
                     , level_set_xvals = level_set_xvals
                     , grid_partition = grid_partition
                     , end_point_lower = end_point_lower
                     , end_point_upper = end_point_upper)
}

#-------------------------------------------------------------------------------
# PART 4: Get Probability Mass
#-------------------------------------------------------------------------------

#' Get probability mass for the level set
#'
#' @param level_set
#' @param huber_distribution
#'
#' @return
#' @export
get_level_set_probability_mass <- function(level_set, huber_distribution){

    end_point_lower <- level_set$end_point_lower
    end_point_upper <- level_set$end_point_upper

    out_measure <-list(prob_measure = sum(huber_distribution(end_point_upper) -
                                              huber_distribution(end_point_lower))
                       , lebesgue_measure = sum(end_point_upper - end_point_lower))

    base::return(out_measure)
}

#-------------------------------------------------------------------------------
# PART 5: Get Prediction Set
#-------------------------------------------------------------------------------

#' Get the robust prediction set
#'
#' @param density_p0
#' @param density_q
#' @param eps
#' @param alpha
#' @param grid_params
#' @param tol
#'
#' @return
#' @export
get_robust_prediction_set <- function(density_p0, density_q, eps, alpha
                                      , grid_params, tol = 1E-3){

    huber_density <- generate_huber_density(density_p0 = density_p0
                                            , density_q = density_q
                                            , eps = eps)

    huber_distribution <- generate_huber_distribution(density_p0 = density_p0
                                                      , density_q = density_q
                                                      , eps = eps)

    grid_partition <- base::seq(from = grid_params$lower
                                , to = grid_params$upper
                                , by = grid_params$interval_length)


    # Get the mode for the Huber Density
    level_max <- max(huber_density(grid_partition))
    level_min <- 0
    level_nbins <- 10000
    level_grid <- base::seq(from = level_min, to = level_max
                            , length = level_nbins)

    level_set_grid <- purrr::map(.x = level_grid
                                 , ~ get_level_set(huber_density = huber_density
                                                   , level = .x
                                                   , tol = tol
                                                   , grid_params = grid_params))

    prob_mass_grid <- purrr::map(.x = level_set_grid
                                 , ~ get_level_set_probability_mass(.x, huber_distribution))

    prob_mass_values <- purrr::modify_depth(prob_mass_grid, 1, "prob_measure") %>%
        base::unlist()

    # leb_meas_values <- purrr::modify_depth(prob_mass_grid, 1, "lebesgue_measure") %>%
    #     base::unlist()

    alpha_robust <- (1 - eps)*alpha

    optimal_mass_indices <- which.min(abs(prob_mass_values - (1 - alpha_robust))) %>%
        base::sort(x = ., decreasing = TRUE)

    optimal_level <- level_grid[optimal_mass_indices[1]]
    optimal_level_set <- level_set_grid[[optimal_mass_indices[1]]]
    out <- list(end_point_lower = optimal_level_set$end_point_lower
                , end_point_upper = optimal_level_set$end_point_upper
                , optimal_level = optimal_level
                , lebesgue_measure = sum(optimal_level_set$end_point_upper -
                                             optimal_level_set$end_point_lower))

    base::return(out)
}

#-------------------------------------------------------------------------------
# PART 6: Plotting support functions
#-------------------------------------------------------------------------------

#' Title
#'
#' @param density_list
#' @param scale_factor
#'
#' @return
#' @export
get_density_plot_support <- function(density_list, scale_factor = 5){

    density_type <- density_list$type

    if(density_type == "normal"){
        mean <- density_list$mean
        sd <- density_list$sd
        out <- list(min = mean - scale_factor*sd,
                    max = mean + scale_factor*sd)
    } else if(density_type == "cauchy"){
        location <- density_list$location
        scale <- density_list$scale
        out <- list(min = location - scale_factor*scale,
                    max = location + scale_factor*scale)
    } else if(density_type == "exponential"){
        rate <- density_list$rate
        out <- list(min = 0,
                    # max = density_list$mean + scale_factor*density_list$sd
                    max = 1/rate +
                        scale_factor*(1/rate))
    } else if(density_type == "pareto"){
        scale <- density_list$scale
        shape <- density_list$shape
        out <- list(min = 0,
                    max = (shape*scale)/(shape - 1) +
                        scale_factor*((scale^2 * shape)/((shape - 1)^2*(shape - 2))))
    } else if(density_type == "uniform"){
        min <- density_list$min
        max <- density_list$max
        tot_diff <- max - min
        out <- list(min = min - scale_factor * tot_diff,
                    max = max + scale_factor * tot_diff)
    }

    base::return(out)
}

#' Title
#'
#' @param density_p0
#' @param density_q
#' @param scale_factor
#'
#' @return
#' @export
get_huber_plot_support <- function(density_p0 = list(type = "normal"
                                                     , mean = 0, sd = 1),
                                   density_q  = list(type = "normal"
                                                     , mean = 5, sd = 1),
                                   scale_factor = 5){

    density_p0_supp <- get_density_plot_support(density_list = density_p0
                                                , scale_factor = scale_factor)
    density_q_supp <- get_density_plot_support(density_list = density_q
                                               , scale_factor = scale_factor)

    min_huber_supp <- min(density_p0_supp$min, density_q_supp$min)
    max_huber_supp <- max(density_p0_supp$max, density_q_supp$max)

    density_huber_supp <- list(min = min_huber_supp,
                               max = max_huber_supp)

    base::return(density_huber_supp)
}

#' Title
#'
#' @param density_p0
#' @param density_q
#' @param eps
#' @param alpha
#' @param tol
#' @param scale_factor
#'
#' @return
#' @export
compare_pred_sets <- function(density_p0 = list(type = "normal", mean = 0, sd = 1),
                              density_q  = list(type = "normal", mean = 5, sd = 1),
                              eps = 0.2, alpha = 0.05,
                              tol = 1E-3,
                              scale_factor = 5){

    # TODO: Create a density plotting function
    # Sample x values for plotting
    huber_range <- get_huber_plot_support(density_p0 = density_p0,
                                          density_q  = density_q,
                                          scale_factor = scale_factor)

    lower_range <- huber_range$min
    upper_range <- huber_range$max

    grid_params = list(lower = lower_range
                       , upper = upper_range
                       , interval_length = 0.01)

    robust_prediction <- get_robust_prediction_set(density_p0 = density_p0
                                                   , density_q = density_q
                                                   , eps = eps
                                                   , alpha = alpha
                                                   , grid_params = grid_params
                                                   , tol = 1E-3)

    true_prediction <- get_known_level_set(density_list = density_p0
                                           , alpha = alpha)

    # Generate the huber density function and evaluate at x_vals
    huber_density <- generate_huber_density(density_p0 = density_p0
                                            , density_q = density_q
                                            , eps = eps)

    true_density <- generate_huber_density(density_p0 = density_p0
                                           , density_q = density_p0
                                           , eps = eps)

    pred_set_plot <- ggplot(data.frame(x = c(lower_range, upper_range)), aes(x = x)) +
        stat_function(fun = huber_density,
                      aes(colour = "Huber")) +
        stat_function(fun = true_density,
                      aes(colour = "True")) +
        scale_x_continuous(name = "x") +
        scale_y_continuous(name = "Density") +
        ggtitle("Comparing Prediction Sets") +
        scale_colour_manual("Groups", values = c("red", "blue")) +
        coord_cartesian(ylim = c(0, 0.5))

    rect_robust <- data.frame(xmin=robust_prediction$end_point_lower
                              , xmax=robust_prediction$end_point_upper
                              , ymin=0, ymax=0.005)
    rect_true <- data.frame(xmin=true_prediction$end_point_lower
                            , xmax=true_prediction$end_point_upper
                            , ymin=0.01, ymax=0.015)

    leb_meas_x_loc_huber <- ((robust_prediction$end_point_lower +
                                  robust_prediction$end_point_upper)/2) %>%
        base::min()
    leb_meas_y_loc_huber <- -0.015

    leb_meas_x_loc_true <- ((robust_prediction$end_point_lower +
                                 robust_prediction$end_point_upper)/2) %>%
        base::min()
    leb_meas_y_loc_true <- 0.03

    pred_set_plot_final <- pred_set_plot + geom_rect(data=rect_robust
                                                     , aes(xmin=xmin
                                                           , xmax=xmax
                                                           , ymin=ymin
                                                           , ymax=ymax),
                                                     fill="red",
                                                     alpha=0.5,
                                                     inherit.aes = FALSE) + geom_rect(data=rect_true
                                                                                      , aes(xmin=xmin
                                                                                            , xmax=xmax
                                                                                            , ymin=ymin
                                                                                            , ymax=ymax),
                                                                                      fill="blue",
                                                                                      alpha=0.5,
                                                                                      inherit.aes = FALSE) +
        annotate("text", x = leb_meas_x_loc_huber, y = leb_meas_y_loc_huber,
                 label = stringr::str_c("Leb_Huber == "
                                        , round(robust_prediction$lebesgue_measure, 2)),
                 parse = TRUE, color = "red") +
        annotate("text", x = leb_meas_x_loc_true, y = leb_meas_y_loc_true,
                 label = stringr::str_c("Leb_True == ", round(true_prediction$lebesgue_measure, 2)),
                 parse = TRUE, color = "blue") +
        geom_hline(aes(yintercept = robust_prediction$optimal_level)
                   , colour="red", linetype="dashed") +
        geom_text(aes(x = lower_range, y = robust_prediction$optimal_level, label = round(robust_prediction$optimal_level, 2), vjust = -1), color = "red") +
        geom_hline(aes(yintercept = true_prediction$optimal_level)
                   , colour="blue", linetype="dashed") +
        geom_text(aes(x = lower_range, y = true_prediction$optimal_level, label = round(true_prediction$optimal_level, 2), vjust = -1), color = "blue") +
        ggplot2::theme_bw()

    base::return(pred_set_plot_final)
}
