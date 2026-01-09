#' Calculate Invested Capital Decomposition
#'
#' Calculates cumulative change in invested capital decomposed into four components:
#' net income, dividends, debt change, and equity activity (residual).
#'
#' @param data Data frame with quarterly data containing: date, net_income,
#'   dividends, debt, equity
#' @param base_date Date to use as baseline (default: min date in data)
#'
#' @return Data frame with columns: date, cum_net_income, cum_dividends,
#'   cum_debt_change, cum_equity_activity, cum_ic_change
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
      cum_debt_change = debt - base_debt,
      cum_equity_change = equity - base_equity,
      cum_ic_change = cum_debt_change + cum_equity_change,
      cum_equity_activity = cum_equity_change - cum_net_income - cum_dividends
    ) %>%
    dplyr::select(
      date,
      cum_net_income,
      cum_dividends,
      cum_debt_change,
      cum_equity_activity,
      cum_ic_change
    )
}
