#' Calculate derived data
#' 
#' This function allows easy calculation of any quantities derived from other variables. The new quantities can be assigned to a specific data_type and values, errors as well as the resulting variable names are calculated/constructed based on custom functions that can be provided via the function parameters. \link{calculate_sums}, \link{calculate_ratios} and \link{calculate abundances} are all based on this and provide an easy way for common standard calculations.
#' 
#' @param data a data frame with raw ion counts retrieved from \code{\link{load_analysis_data()}}, can be grouped to do calculations within individual groups
#' @param ... the columns to send to the value, error and naming function for each derived value, e.g. c("12C", "13C", "12C14N"), the number of parameters needs to match those expected by the value, error and name functions. Error values of different columns (say for classical error propagation) can be addressed using the suffix "sigma", e.g. c("12C", "12C sigma") would pass both the value and error of this variable to the value and error function.
#' @param value_fun a custom function used to calculate the derived value - needs to match the sets of paramters provided through ...
#' @param error_fun a custom function used to calcluate the error (sigma) for the derived value
#' @param name_fun a custom function used to construct the variable name for the derived quantity
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @return the original data frame with the sums information appended (data_type == "ion_sum")
#' @export
calculate <- function(data, data_type, ..., value_fun, 
                      error_fun = function(...) return(NA), 
                      name_fun = function(...) return(paste(list(...), collapse = " ")),
                      quiet = F) {
  # checks
  if(is.null(data$variable)) stop("'variable' column does not exist")
  if(is.null(data$value)) stop("'value' column does not exist")
  if(is.null(data$data_type)) stop("'data_type' column does not exist")
  
  # checks
  params <- list(...)
  missing <- setdiff(params %>% unlist(), c(names(data), data$variable %>% unique(), data$variable %>% unique() %>% paste("sigma")))
  if (length(missing) > 0) {
    stop("some variables do not exist in this data set: ", missing %>% paste(collapse = ", ")) 
  }
  
  # generate function calls to make calculations
  var_new <- sapply(params, function(i) do.call(name_fun, i %>% as.list()))
  val_fields <-
    lapply(params, function(i) {
      func_call <- sprintf("f(`%s`)", i %>% paste(collapse = "`,`"))
      lazyeval::as.lazy(func_call, parent.frame()) %>% lazyeval::interp(f = value_fun)
    }) %>% setNames(var_new)
  
  err_fields <-
    lapply(params, function(i) {
      func_call <- sprintf("f(`%s`)", i %>% paste(collapse = "`,`"))
      lazyeval::as.lazy(func_call, parent.frame()) %>% lazyeval::interp(f = error_fun)
    }) %>% setNames(var_new)
  
  # figure out what are the actual new variables (includes overriding old ones)
  new_data_type <- data_type
  var_old <- data$variable %>% unique() %>% setdiff(var_new)
  var_new_select <- lapply(var_old, function(i) lazyeval::interp(~-var, var = as.name(i)))
  
  # spread data into wide format (relies on groups getting carried through the spread)
  df <- 
    suppressMessages(
      left_join(
        data %>% 
          select(-sigma, -data_type) %>% 
          tidyr::spread(variable, value),
        data %>% 
          mutate(variable = paste(variable, "sigma")) %>% 
          select(-value, -data_type) %>% 
          tidyr::spread(variable, sigma)
      ))

  # just in case of grouping, make calculations with do
  new_data <- 
    df %>% 
    do({
      
      df_group <- .
      
      # calculate values and error within in each group
      values <- 
        df_group %>% 
        mutate_(.dots = val_fields) %>% 
        select(-ends_with("sigma")) %>% 
        select_(.dots = var_new_select) %>% 
        tidyr::gather_("variable", "value", var_new) 
      
      error <- 
        df_group %>% 
        mutate_(.dots = err_fields) %>% 
        select(-ends_with("sigma")) %>% 
        select_(.dots = var_new_select) %>% 
        tidyr::gather_("variable", "sigma", var_new) 
      
      suppressMessages(left_join(values, error))  %>% 
        mutate(variable = as.character(variable)) %>% # don't like the factor it introduces
        return()
  }) %>% 
  filter(!is.na(value)) %>% # remove calcluations that don't exist
  mutate(data_type = new_data_type)
  
  # info
  if (!quiet) {
    sprintf(
      paste0(
        "INFO: %d '%s' values + errors calculated and added to the data frame",
        "\n      values added (stored in 'variable' column): %s"),
      new_data %>%  nrow(), new_data_type,
      new_data %>% 
        group_by(variable) %>% tally() %>%
        mutate(label = paste0("'", variable, "' (", n, "x)")) %>% 
        magrittr::extract2("label") %>% paste(collapse = ", ")
    ) %>% message()
  }
  
  # combine old data with new data
  bind_rows(
    data %>% filter(!variable %in% var_new), # make sure no duplicates
    new_data
  )
}



#' Calculate ion sums
#' 
#' This function calculates the ion sums and resulting counting
#' statistics error from multiple raw ion counts. It can be applied to data from both
#' LANS_summary and LANS_maps loading but can be slow if LANS_maps is combined
#' from many analyses.
#' 
#' @param data a data frame with raw ion counts retrieved from \code{\link{load_analysis_data()}}
#' @param ... the ion sums to calculate, each entry is for one sum of as many ions as desired,
#' e.g. c("13C", "12C"), c("15N12C", "14C12C"), ...
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @return the original data frame with the sums information appended (data_type == "ion_sum")
#' @export
calculate_sums <- function(data, ..., quiet = F) {
  
  # function to sum up arbitrary number of vectors by entry
  sum_vectors <- 
    function(...) { 
      r <- ..1
      for(i in list(...)[-1]) r <- r+i
      return(r)
    }
  
  # calculate sums
  calculate(
    data,
    data_type = "ion_sum",
    ...,
    value_fun = sum_vectors,
    error_fun = function(...) {
      lans2r:::iso.errN(sum_vectors(...))
    },
    name_fun = function(...) paste(list(...), collapse = "+"),
    quiet = quiet
  )
}

#' Calculate isotope ratios
#' 
#' This function calculates the ratios and resulting counting
#' statistics error from the raw ion counts. It can be applied to data from both
#' LANS_summary and LANS_maps loading but can be slow if LANS_maps is combined
#' from many analyses. It can also be applied to ion_sums generate by calculate_sums
#' to calculate elemental ratios (careful, ionization efficiencies skew their scaling!)
#' 
#' @param data a data frame with raw ion counts retrieved from \code{\link{load_analysis_data()}}
#' @param ... the ratios to calculate, each entry is one ratio with major isotope first, then
#' minor isotope, e.g. c("13C", "12C"), c("15N12C", "14C12C"), ...
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @note TODO: see if can improve performance by avoiding the call to spread and use joins instead.
#' @return the original data frame with the ratio information appended (all ratios have data_type == "ratio")
#' @export
calculate_ratios <- function(data, ..., quiet = F) {
  
  # calculate ratios
  calculate(
    data,
    data_type = "ratio",
    ...,
    value_fun = function(m, M) lans2r:::iso.R(M, m),
    error_fun = function(m, M) lans2r:::iso.errR(M, m),
    name_fun = function(m, M) paste0(m,"/",M),
    quiet = quiet
  )
}


#' Calculate istope fractional abundances
#' 
#' This function calculates the isotope abundances (in %!) and resulting counting
#' statistics error from the raw ion counts. It can be applied to data from both
#' LANS_summary and LANS_maps loading but can be slow if LANS_maps is combined
#' from many analyses.
#' 
#' @param data a data frame with raw ion counts retrieved from \code{\link{load_analysis_data()}}
#' @param ... the fractional abundances to calculate, each entry is for one fractional abundance with major isotope first, then
#' minor isotope, e.g. c("13C", "12C"), c("15N12C", "14C12C"), ...
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @note TODO: see if can improve performance by avoiding the call to spread and use joins instead.
#' @return the original data frame with the fractional abundance information appended (all fractoinal abundances are in % and have data_type == "abundance")
#' @export
calculate_abundances <- function(data, ..., quiet = F) {
  
  # calculate ratios
  calculate(
    data,
    data_type = "abundance",
    ...,
    value_fun = function(m, M) 100*lans2r:::iso.F(M, m),
    error_fun = function(m, M) 100*lans2r:::iso.errF(M, m),
    name_fun = function(m, M) paste(m, "F"),
    quiet = quiet
  )
  
}
