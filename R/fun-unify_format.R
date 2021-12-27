#' Unify the format of data
#'
#' @param raw_data unformated data frame from excel
#' @import magrittr
#' @import dplyr
#' @return formated data frame
#' @export

unify_format <- function(raw_data) {
  stopifnot(!missing(raw_data))
  stopifnot(raw_data %>% is.data.frame())
  data_out <- raw_data %>%
    dplyr::filter(!is.na(cabin)) %>%
    unify_cabin() %>%
    add_deck_col()
  return(data_out)
}



unify_cabin <- function(raw_data) {
  stopifnot("cabin" %in% colnames(raw_data))
  deck_col <- raw_data %>% dplyr::select("cabin") %>% apply(X = ., MARGIN = 1, FUN = cabin_to_deck)
  data_out <- raw_data %>% dplyr::mutate(deck = factor(deck_col))
  return(data_out)
}



cabin_to_deck <- function(cabine_name) {
  deck_name <- cabine_name %>% strsplit(split = "") %>% unlist() %>% .[1]
  return(deck_name)
}

add_deck_col <- function(data_in) {
  data_out <- data_in %>%
    dplyr::mutate(deck_A = (.data[["deck"]] == "A")) %>%
    dplyr::mutate(deck_B = (.data[["deck"]] == "B")) %>%
    dplyr::mutate(deck_C = (.data[["deck"]] == "C")) %>%
    dplyr::mutate(deck_D = (.data[["deck"]] == "D")) %>%
    dplyr::mutate(deck_E = (.data[["deck"]] == "E")) %>%
    dplyr::mutate(deck_F = (.data[["deck"]] == "F")) %>%
    dplyr::mutate(deck_G = (.data[["deck"]] == "G"))
  return(data_out)
}
