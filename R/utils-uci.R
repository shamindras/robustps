#' Function to download and clean UCI data into a tibble
#'
#' @param ucidurl (character): the specific url subdirectory of the required
#' UCI dataset e.g. for the UCI hepatitis dataset, is typically located in the
#' \url{https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/hepatitis.data}
#' so user only needs to enter "hepatitis/hepatitis.data" here in this case
#' @param col_names_raw (character): A vector of column names for the input
#' dataset. These can be in any case. Spaces should be replaced by uncerscores
#' for all values e.g. c("Body Temp") should be changed to c("Body_Temp")
#' @param col_types_raw (character): A single string where each character
#' corresponds to the \code{readr} shorthand for the column type
#' (e.g. "f" for factor)#' for the corresponding column to be read in by
#' \code{readr}
#'
#' @return tibble: required UCI dataset
#' @export
get_uci_data <- function(ucidurl, col_names_raw, col_types_raw){
    data_raw <- readr::read_csv(file = ucidurl,
                                col_names = col_names_raw,
                                col_types = col_types_raw)

    data_mod <- data_raw %>%
        magrittr::set_colnames(x = ., value = col_names_raw)
    data_mod <- data_mod %>% janitor::clean_names(dat = ., case = "snake")
    base::return(data_mod)
}

#' Returns an original binary class dataframe filtered to the required
#' \code{class} variable column
#'
#' @param df (dataframe) : The required binary class dataframe to be filtered.
#' This is required to have a \code{class} variable
#' @param clss_val (character) : The required class value to be filtered
#'
#' @return (dataframe) : The filtered version of the original dataframe to the
#' required class value
#' @export
split_by_class <- function(df, clss_val){
    df %>%
        dplyr::filter(class == clss_val) %>%
        base::return()
}

#' Get a list of sampled dataframes of the original binary class dataframe
#' split to get the specified true and contamination proportion between the
#' 2 binary classes.
#'
#' @param uci_dat (dataframe) : Dataframe for the binary classification data
#' @param plt_vars (character) : These are the 2 variables we will scatterplot
#' User needs to ensure that the class variable is last in the vector i.e. the
#' third value
#' @param clss_vals (character) : These are the binary class variables values.
#' The first element value is assumed to be the true class value and
#' the second value is assumed to be the contamination class value
#' @param epsilon (double) : The proportion of the contamination class value that
#' we require in our final dataframe
#' @param tot_nrows_req (integer) : The total number of observations we require
#' in our final dataframe of true and contaminated proportions
#'
#' @return (dataframe) : Combined resampled dataframe of true and contamination
#' classes
#' @export
get_split_clss <- function(uci_dat, plt_vars, clss_vals,
                           epsilon, tot_nrows_req){

    # Select the required variables and remove any NAs for plotting
    uci_dat_cln <- uci_dat %>%
        dplyr::select(plt_vars) %>%
        stats::na.omit()

    # Get tibbles split by true and contamination classes
    split_clss <- clss_vals %>%
        purrr::map(.x = ., .f =
                       ~split_by_class(df = uci_dat_cln,
                                       clss_val = .x))
    base::length(split_clss) # TODO: delete later

    # Get original clean dataframe
    # Perform the sampling verifications
    cont_nrows_orig <- purrr::map_int(.x = split_clss, .f = ~base::nrow(x = .x))
    tot_nrows_orig <- base::sum(cont_nrows_orig)

    # cont_nrows_req <- base::ceiling(epsilon * cont_nrows_orig[1])
    cont_nrows_req <- base::ceiling(epsilon * tot_nrows_req)
    nrows_req <- base::vector(mode = "integer", length = 0L)
    nrows_req <- c(tot_nrows_req - cont_nrows_req, cont_nrows_req)

    # We want to print out to the user:
    print(glue::glue("The contamination proportion chosen: {epsilon}"))
    print(glue::glue("The total number of contamination observations available {cont_nrows_orig[1]}"))
    print(glue::glue("The total number of (rounded up) contamination observations requested: {nrows_req[1]}"))
    print(glue::glue("The total number of true observations available: {cont_nrows_orig[2]}"))
    print(glue::glue("The total number of (rounded) true observations requested: {nrows_req[2]}"))

    split_clss_smpl <- split_clss %>%
        purrr::map2(.x = ., .y = nrows_req,
                    .f = ~dplyr::sample_n(tbl = .x,
                                          size = .y,
                                          replace = FALSE))

    # Rowbind the sampled true and combined dataframe
    split_clss_smpl_comb <- split_clss_smpl %>% purrr::map_df(.x = ., .f = ~.x)

    base::return(split_clss_smpl_comb)
}

#' Create the scatter plot of the True and \code{epsilon}-contaminated
#' binary class dataset split by the specified 2 variables
#'
#' @param ds (data.frame) : The input cleaned data frame, assumes that this has
#' a binary valued variable called \code{class}
#' @param ds_name (character) : The name of the data set we are importing to be
#' displayed in the plot title
#' @param plt_vars (character) : The 2 variables we intend to plot across the
#' class labels in our final dataset, specified in the x-y ordering
#' @param clss_vals (character) : These are the binary class variables values.
#' The first element value is assumed to be the true class value and
#' the second value is assumed to be the contamination class value
#' @param clss_rnm_vals (character) : These are the renamed binary class
#' variables values to be displayed in the plot legend. The first element value
#' is assumed to be the true class value and the second value is assumed to be
#' the contamination class value.
#' @param epsilon (double) : The contamination proportion, must be a value in
#' (0, 1), but should really be < 0.5
#' @param tot_nrows_req (integer) : The total number of observations i.e. True
#' and contamination values to display in the final dataset and plot
#'
#' @export
get_plot_clss <- function(ds, ds_name, plt_vars,
                          clss_vals, clss_rnm_vals,
                          epsilon,tot_nrows_req){

    # Create the sampled true and epsilon-contaminated dataset
    bn_uci_resmpl <- get_split_clss(uci_dat = ds,
                                    plt_vars = plt_vars,
                                    clss_vals = clss_vals,
                                    epsilon = epsilon,
                                    tot_nrows_req = tot_nrows_req)

    # Create automated header explaining true/ contamination proportions
    # Convert plot variables to Title Case for plot axes labeling
    plt_vars_ttl <- plt_vars %>% purrr::map_chr(.x = .,
                                                .f = ~stringr::str_replace_all(
                                                    string = .x,
                                                    pattern = "_",
                                                    replace = " ")) %>%
        stringr::str_to_title(string = .)
    # Main Plot Title
    ttl <- glue::glue('{ds_name} : $\\epsilon = $ {epsilon} ',
                      'total number of obs. = {tot_nrows_req}', sep = "")

    # Generate the ordered plot - ensure that we rename the
    # class variable values to the "True/ Cont" names
    out_plt <- bn_uci_resmpl  %>%
        dplyr::mutate(class = forcats::fct_recode(class, !!!clss_rnm_vals)) %>%
        ggplot2::ggplot(data = .,
                        aes_string(x = plt_vars[1],
                                   y = plt_vars[2], col = "class")) +
        ggplot2::scale_color_manual(values=c("#E9254B", "#56B4E9")) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::labs(x = plt_vars_ttl[1],
                      y = plt_vars_ttl[2],
                      title = latex2exp::TeX(ttl))

    base::return(out_plt)
}
