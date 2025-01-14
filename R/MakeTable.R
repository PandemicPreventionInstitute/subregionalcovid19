#' Calculate Risk
#'
#' @param p_I Probability one individual in a population is infectious.
#' @param g Event size.
#'
#' @return The risk (%) one or more individuals at an event of size g will be infectious.
#' @export
#' @seealso [estRisk()]
#' @examples
#' \dontrun{
#' risk <- calcrisk(.001, 50)
#' }
#'
calc_risk <- function(p_I, g) {
  r <- 1 - (1 - p_I)**g
  return(round(r * 100, 1))
}

#' Create Table of Risk Estimates
#'
#' @param df_in Input data.
#' @param risk_output Name of output file.
#' @param output_prefix Folder location to store table file.
#' @param event_size Event size(s) to calculate risk for.
#' @param asc_bias_list Ascertainment bias(es) to calculate risk for.
#'
#' @return Creates, and writes to file, a table showing estimated risk that one or more people will be infectious for the given input locations, event sizes and ascertainment biases.
#' @export
#'
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' Canada <- LoadCanada()
#' create_c19r_data(Canada)
#' }
create_c19r_data <- function(df_in,
                             risk_output = sprintf("world_risk_regions_%s.csv", stringr::str_replace_all(lubridate::today(), "-", "")),
                             output_prefix = ".",
                             event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000),
                             asc_bias_list = c(3, 4, 5)) {
  if (!all(is.numeric(event_size)) & !all(event_size > 0)) {
    stop("'event_size' must be a vector of positive numbers")
  }

  if (!all(is.numeric(asc_bias_list)) & !all(asc_bias_list > 0)) {
    stop("'asc_bias_list' must be a vector of positive numbers")
  }

  risk_output <- file.path(output_prefix, risk_output)
  if (file.access(dirname(risk_output), mode = 2) != 0) {
    stop("Directory for risk_output file does not appear to be writeable.")
  }

  pInf <- Nr <- geoid <- risk <- NULL
  df_in <- data.frame(df_in)
  df_in$geometry <- NULL

  risk_data <- list()

  for (asc_bias in asc_bias_list) {
    data_Nr <- df_in %>%
      dplyr::mutate(Nr = pInf * asc_bias)

    for (size in event_size) {
      cn <- glue::glue("{asc_bias}_{size}")

      riskdt <- data_Nr %>%
        dplyr::mutate(
          risk = round(calc_risk(
            Nr, size
          ), 0),
          risk = dplyr::case_when(
            risk < 1 ~ 0,
            TRUE ~ risk
          ),
          "asc_bias" = asc_bias,
          "event_size" = size
        )
      risk_data[[cn]] <- riskdt %>%
        dplyr::select(geoid, "{cn}" := risk)
      id <- paste(asc_bias, size, sep = "_")
    }
  }

  risk_data_df <- purrr::reduce(.x = append(list(df_in), risk_data), .f = dplyr::left_join) %>%
    dplyr::mutate(updated = lubridate::ymd(gsub("-", "", Sys.Date())))

  utils::write.csv(risk_data_df,
    risk_output,
    quote = T,
    row.names = F
  )
}
