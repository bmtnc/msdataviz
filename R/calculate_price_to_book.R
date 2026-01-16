#' Calculate Price-to-Book Ratio
#'
#' @param price Stock price (numeric vector)
#' @param book_value_per_share Book value per share (numeric vector)
#' @return P/B ratio (numeric vector)
#' @keywords internal
#' @export
calculate_price_to_book <- function(price, book_value_per_share) {
  ifelse(book_value_per_share > 0, price / book_value_per_share, NA_real_)
}
