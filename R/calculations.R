#' Calculate ion sums
#' 
#' This function calculates the ion sums and resulting counting
#' statistics error from multiple raw ion counts. It can be applied to data from both
#' LANS_summary and LANS_maps loading but can be slow if LANS_maps is combined
#' from many analyses.
#' 
#' @param data a data frame with raw ion counts retrieved from \code{\link{load_analysis_data()}}
#' @param ... the ion sums to calculate, each entry is for one sum of two ions,
#' e.g. c("13C", "12C"), c("15N12C", "14C12C"), ...
#' @param quiet whether the function should output information messages or be quiet (default is to output)
#' @note TODO: right now only supports sum of two ions, make this more flexible by introducing
#'  a flexible function call that can take multiple paramters
#' @return the original data frame with the fractional abundance information appended (all fractoinal abundances are in % and have data_type == "abundance")
#' @export
calculate_sums <- function(data, ..., quiet = F) {
  # checks
  if(is.null(data$variable)) stop("'variable' column does not exist")
  if(is.null(data$value)) stop("'value' column does not exist")
  if(is.null(data$data_type)) stop("'data_type' column does not exist")
  
  # checks
  params <- list(...)
  missing <- setdiff(params %>% unlist(), filter(data, data_type == "ion_count")$variable %>% unique)
  if (length(missing) > 0) {
    stop("some variables do not exist in this data set: ", missing %>% paste(collapse = ", ")) 
  }
  
  ion_sum <- function(a,b) a+b
  ion_sum_error <- function(a,b) lans2r:::iso.errN(a+b) 
  
  # generate function calls to make calculations
  var_new <- sapply(params, function(i) paste0(i[1],"+",i[2]))
  val_fields <- 
    lapply(params, function(i) lazyeval::interp(~ion_sum(NM, Nm), NM = as.name(i[2]), Nm = as.name(i[1]))) %>% 
    setNames(var_new)
  err_fields <- 
    lapply(params, function(i) lazyeval::interp(~ion_sum_error(NM, Nm), NM = as.name(i[2]), Nm = as.name(i[1]))) %>% 
    setNames(var_new)
  
  # existing variables
  var_old <- (data %>% filter(data_type == "ion_count"))$variable %>% unique() %>% setdiff(var_new)
  var_old_deselect <- lapply(var_old, function(i) lazyeval::interp(~-var, var = as.name(i)))
  
  values <- 
    data %>% filter(data_type == "ion_count") %>% 
    select(-sigma) %>% 
    tidyr::spread(variable, value) %>% 
    mutate_(.dots = val_fields) %>% 
    select_(.dots = var_old_deselect) %>% 
    tidyr::gather_("variable", "value", var_new) 
  error <- 
    data %>% filter(data_type == "ion_count") %>% 
    select(-sigma) %>% 
    tidyr::spread(variable, value) %>% 
    mutate_(.dots = err_fields) %>% 
    select_(.dots = var_old_deselect) %>% 
    tidyr::gather_("variable", "sigma", var_new) 
  
  if (!quiet) {
    sprintf(
      paste0(
        "INFO: %d isotope sums + errors calculated and added to the data frame with data_type 'ion_count'",
        "\n      isotopes sums added (stored in 'variable' column): %s"),
      nrow(values), 
      (values %>% count(variable) %>% mutate(label = paste0("'", variable, "' (", n, "x)")))$label %>% paste(collapse = ", ")
    ) %>% message()
  }
  
  # combine old data with new data
  bind_rows(
    data %>% filter(! (data_type == "ion_count" & variable %in% var_new)), # make sure no duplicates
    suppressMessages(left_join(values, error)) %>% 
      filter(!is.na(value)) %>% # remove non existent abundances
      mutate(data_type = "ion_sum") %>% 
      mutate(variable = as.character(variable)) # don't like the factor it introduces
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
  # checks
  if(is.null(data$variable)) stop("'variable' column does not exist")
  if(is.null(data$value)) stop("'value' column does not exist")
  if(is.null(data$data_type)) stop("'data_type' column does not exist")
  
  # checks
  params <- list(...)
  missing <- setdiff(params %>% unlist(), filter(data, data_type %in% c("ion_count", "ion_sum"))$variable %>% unique)
  if (length(missing) > 0) {
    stop("some ion_counts or ion_sums do not exist in this data set: ", missing %>% paste(collapse = ", ")) 
  }
  
  # generate function calls to make calculations
  var_new <- sapply(params, function(i) paste0(i[1],"/",i[2]))
  val_fields <- 
    lapply(params, function(i) lazyeval::interp(~lans2r:::iso.R(NM, Nm), NM = as.name(i[2]), Nm = as.name(i[1]))) %>% 
    setNames(var_new)
  err_fields <- 
    lapply(params, function(i) lazyeval::interp(~lans2r:::iso.errR(NM, Nm), NM = as.name(i[2]), Nm = as.name(i[1]))) %>% 
    setNames(var_new)
  
  # existing variables
  var_old <- (data %>% filter(data_type %in% c("ion_count", "ion_sum")))$variable %>% unique() %>% setdiff(var_new)
  var_old_deselect <- lapply(var_old, function(i) lazyeval::interp(~-var, var = as.name(i)))
  
  values <- 
    data %>% filter(data_type %in% c("ion_count", "ion_sum")) %>% 
    select(-sigma) %>% 
    tidyr::spread(variable, value) %>% 
    mutate_(.dots = val_fields) %>% 
    select_(.dots = var_old_deselect) %>% 
    tidyr::gather_("variable", "value", var_new) 
  error <- 
    data %>% filter(data_type %in% c("ion_count", "ion_sum")) %>% 
    select(-sigma) %>% 
    tidyr::spread(variable, value) %>% 
    mutate_(.dots = err_fields) %>% 
    select_(.dots = var_old_deselect) %>% 
    tidyr::gather_("variable", "sigma", var_new) 
  
  if (!quiet) {
    sprintf(
      paste0(
        "INFO: %d ratios + errors calculated and added to the data frame with data_type 'ratio'",
        "\n      ratios added (stored in 'variable' column): %s"),
      nrow(values), 
      (values %>% count(variable) %>% mutate(label = paste0("'", variable, "' (", n, "x)")))$label %>% paste(collapse = ", ")
    ) %>% message()
  }
  
  # combine old data with new data
  bind_rows(
    data %>% filter(! (data_type == "ratio" & variable %in% var_new)), # make sure no duplicates
    suppressMessages(left_join(values, error)) %>% 
      filter(!is.na(value)) %>% # remove non existent ratios
      mutate(data_type = "ratio") %>% 
      mutate(variable = as.character(variable)) # don't like the factor it introduces
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
  # checks
  if(is.null(data$variable)) stop("'variable' column does not exist")
  if(is.null(data$value)) stop("'value' column does not exist")
  if(is.null(data$data_type)) stop("'data_type' column does not exist")
  
  # checks
  params <- list(...)
  missing <- setdiff(params %>% unlist(), filter(data, data_type == "ion_count")$variable %>% unique)
  if (length(missing) > 0) {
    stop("some ion_counts do not exist in this data set: ", missing %>% paste(collapse = ", ")) 
  }
  
  # generate function calls to make calculations
  var_new <- sapply(params, function(i) paste(i[1], "F"))
  val_fields <- 
    lapply(params, function(i) lazyeval::interp(~100*lans2r:::iso.F(NM, Nm), NM = as.name(i[2]), Nm = as.name(i[1]))) %>% 
    setNames(var_new)
  err_fields <- 
    lapply(params, function(i) lazyeval::interp(~100*lans2r:::iso.errF(NM, Nm), NM = as.name(i[2]), Nm = as.name(i[1]))) %>% 
    setNames(var_new)
  
  # existing variables
  var_old <- (data %>% filter(data_type == "ion_count"))$variable %>% unique() %>% setdiff(var_new)
  var_old_deselect <- lapply(var_old, function(i) lazyeval::interp(~-var, var = as.name(i)))
  
  values <- 
    data %>% filter(data_type == "ion_count") %>% 
    select(-sigma) %>% 
    tidyr::spread(variable, value) %>% 
    mutate_(.dots = val_fields) %>% 
    select_(.dots = var_old_deselect) %>% 
    tidyr::gather_("variable", "value", var_new) 
  error <- 
    data %>% filter(data_type == "ion_count") %>% 
    select(-sigma) %>% 
    tidyr::spread(variable, value) %>% 
    mutate_(.dots = err_fields) %>% 
    select_(.dots = var_old_deselect) %>% 
    tidyr::gather_("variable", "sigma", var_new) 
  
  if (!quiet) {
    sprintf(
      paste0(
        "INFO: %d abundances + errors calculated (in %%!) and added to the data frame with data_type 'abundance'",
        "\n      fractional abundances added (stored in 'variable' column): %s"),
      nrow(values), 
      (values %>% count(variable) %>% mutate(label = paste0("'", variable, "' (", n, "x)")))$label %>% paste(collapse = ", ")
    ) %>% message()
  }
  
  # combine old data with new data
  bind_rows(
    data %>% filter(! (data_type == "abundance" & variable %in% var_new)), # make sure no duplicates
    suppressMessages(left_join(values, error)) %>% 
      filter(!is.na(value)) %>% # remove non existent abundances
      mutate(data_type = "abundance") %>% 
      mutate(variable = as.character(variable)) # don't like the factor it introduces
  )
}
