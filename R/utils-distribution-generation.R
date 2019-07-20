#-------------------------------------------------------------------------------
# PART 1: Generate Huber Density
#-------------------------------------------------------------------------------

#' Generate the required density function
#'
#' @param density_list
#'
#' @return
#' @export
gen_density <- function(density_list){

    density_type <- density_list$type

    if(density_type == "normal"){
        out <- function(x){stats::dnorm(x = x,
                                        mean = density_list$mean,
                                        sd = density_list$sd)}
    } else if(density_type == "exponential"){
        out <- function(x){stats::dexp(x = x, rate = density_list$rate)}
    } else if(density_type == "uniform"){
        out <- function(x){stats::dunif(x = x,
                                        min = density_list$min,
                                        max = density_list$max)}
    } else if(density_type == "pareto"){
        out <- function(x){VGAM::dpareto(x = x,
                                         scale = density_list$scale,
                                         shape = density_list$shape)}
    } else if(density_type == "cauchy"){
        out <- function(x){stats::dcauchy(x = x,
                                          location = density_list$location,
                                          scale = density_list$scale)}
    }
    # Return the specified distribution function
    base::return(out)
}

#' Generate the required huber density function
#'
#' @param density_p0
#' @param density_q
#' @param eps
#'
#' @return
#' @export
generate_huber_density <- function(density_p0 = list(type = "normal"
                                                     , mean = 0, sd = 1)
                                   , density_q = list(type = "normal"
                                                      , mean = 0, sd = 1)
                                   , eps){

    eval_p0 <- gen_density(density_list = density_p0)
    eval_q  <- gen_density(density_list = density_q)

    eval_p  <- function(x){(1-eps)*eval_p0(x) + eps*eval_q(x)}
    base::return(eval_p)
}

#-------------------------------------------------------------------------------
# PART 2: Generate Huber Distribution
#-------------------------------------------------------------------------------

#' Generate the specified distribution
#'
#' @param density_list
#'
#' @return
#' @export
gen_distribution <- function(density_list){

    density_type <- density_list$type

    if(density_type == "normal"){
        out <- function(x){pnorm(q = x
                                 , mean = density_list$mean
                                 , sd = density_list$sd)}
    } else if(density_type == "cauchy"){
        out <- function(x){pcauchy(q = x,
                                   location = density_list$location,
                                   scale = density_list$scale)}
    } else if(density_type == "exponential"){
        out <- function(x){pexp(q = x, rate = density_list$rate)}
    } else if(density_type == "pareto"){
        out <- function(x){VGAM::ppareto(q = x,
                                         scale = density_list$scale,
                                         shape = density_list$shape)}
    } else if(density_type == "uniform"){
        out <- function(x){punif(q = x
                                 , min = density_list$min
                                 , max = density_list$max)}
    }

    base::return(out)
}

#' Generate the required Huber Distribution function
#'
#' @param density_p0
#' @param density_q
#' @param eps
#'
#' @return
#' @export
generate_huber_distribution <- function(density_p0 = list(type = "normal"
                                                          , mean = 0, sd = 1)
                                        , density_q = list(type = "normal"
                                                           , mean = 0, sd = 1)
                                        , eps){

    eval_p0 <- gen_distribution(density_list = density_p0)
    eval_q  <- gen_distribution(density_list = density_q)

    eval_p  <- function(x){(1-eps)*eval_p0(x) + eps*eval_q(x)}
    base::return(eval_p)
}
