#' Calculate Invested Capital Decomposition
#'
#' Calculates cumulative change in invested capital decomposed into four components:
#' net income, dividends, debt change, and equity capital activity (residual).
#'
#' @param data Data frame with quarterly data containing: date, net_income,
#'   dividends, debt, equity
#' @param base_date Date to use as baseline (default: min date in data)
#'
#' @return Data frame with cumulative changes for each component
#' @export
calculate_ic_decomposition <- function(data, base_date = NULL) {
  required_cols <- c("date", "net_income", "dividends", "debt", "equity")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")

  data <- data %>%
    dplyr::arrange(date)

  if (is.null(base_date)) {
    base_date <- min(data$date)
  }

  base_idx <- which(data$date == base_date)
  if (length(base_idx) == 0) {
    stop("base_date not found in data: ", base_date)
  }

  base_debt <- data$debt[base_idx]
  base_equity <- data$equity[base_idx]
  base_ic <- base_debt + base_equity

  data %>%
    dplyr::filter(date >= base_date) %>%
    dplyr::mutate(
      cum_net_income = cumsum(dplyr::coalesce(net_income, 0)),
      cum_dividends = -cumsum(dplyr::coalesce(dividends, 0)),
      debt_change = debt - base_debt,
      equity_change = equity - base_equity,
      ic_change = debt_change + equity_change,
      equity_capital_activity = equity_change - cum_net_income - cum_dividends
    ) %>%
    dplyr::select(
      date,
      cum_net_income,
      cum_dividends,
      debt_change,
      equity_capital_activity,
      ic_change
    )
}
