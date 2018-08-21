#' Performs Approximate Conformal Prediction
#'
#' @param input_data d by n data matrix
#' @param h bandwidth for Kernel Density Estimation
#' @param grid d by n_grid coordinate grids
#' @param alpha prediction significance level
#'
#' @return
#' @export
get_conf_approx <- function(input_data, h, alpha
                        , grid_xmin
                        , grid_xmax
                        , grid_bgridsize){

    n <- base::nrow(input_data)

    # Dimensions
    d <- base::ncol(input_data)

    # TODO: Change this to be multidimensional
    K_0  <- mvtnorm::dmvnorm(x = base::rep(x = 0, d)
                             , mean = base::rep(x = 0, d)
                             , sigma = (h^2)*diag(d))

    # TODO: think about conditioning on small h i.e. d = 1
    # TODO: work out the gridsize
    kde_fit <- ks::kde(x = input_data
                       , H = (h^2)*diag(d)
                       , h = h
                       , xmin = grid_xmin
                       , xmax = grid_xmax
                       , binned = TRUE
                       , bgridsize = grid_bgridsize)

    # Do the predictions
    p_Y <- stats::predict(object = kde_fit, x = input_data, zero.flag=TRUE)
    p_grid <- kde_fit$estimate

    # Sort the predicted values
    p_sort = base::sort(p_Y)

    cut_inner <- p_sort[floor(n * alpha)]
    cut_outer <- cut_inner - 1/(n*h^d)*K_0

    conf_inner <- (p_grid >= cut_inner)
    conf_outer <- (p_grid >= cut_outer)

    out_list <- list("conf_inner" = conf_inner
                     , "conf_outer" = conf_outer)

    base::return(out_list)
}
