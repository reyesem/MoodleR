#' Create Moodle XML code for storing a dataset for a calculated question.
#'
#' Calculated questions in Moodle store the wildcards in a dataset.
#' \code{wildcard_mxml} creates the Moodle XML node for storing the wildcards.
#' The code is returned as a text string that would generally be included in the
#' HTML file rendered by the RMarkdown process; it is then read and converted
#' to XML code using the \code{\link{mxml_read_calculated}} function.
#'
#'
#'
#' @param data a data.frame storing the wildcard information. The variables
#' in the data.frame correspond to wildcards. Each row corresponds to a
#' unique set of linked values.
#' @param shared a vector of boolean values of the same length as the number of
#' columns in \code{data}. If an element is TRUE, the corresponding wildcard
#' values are shared among questions within the same category. If FALSE, they
#' are specific to this question. Per Moodle documentation, if questions within
#' the same category make use of shared datasets and have the same number of
#' datasets, the questions will be synchronized. Values are replicated as
#' necessary (default = TRUE).
#'
#' @return a character string containing XML code.
#'
#' @export
wildcards_mxml <- function(data, shared = TRUE){
  # replicated shared value as needed
  shared <- rep_len(shared, length.out = ncol(data))

  # create dataset items from entries
  data <- data |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), function(u){
      paste0("<dataset_item>",
             "<number>", seq_along(u), "</number>",
             "<value>", u, "</value>",
             "</dataset_item>")}))

  # each wildcard becomes new dataset
  .text <- purrr::pmap_chr(
    list(data, shared, colnames(data)),
    function(wildcard, share, name){
      paste0("<dataset_definition>",
             "<status><text>",
             ifelse(share, "shared", "private"),
             "</text></status>",
             "<name><text>", name, "</text></name>",
             "<type>calculated</type>",
             "<distribution><text>uniform</text></distribution>",
             "<minimum><text>0</text></minimum>",
             "<maximum><text>1</text></maximum>",
             "<decimals><text>2</text></decimals>",
             "<itemcount>", length(wildcard), "</itemcount>",
             "<dataset_items>",
             paste0(wildcard, collapse = ""),
             "</dataset_items>",
             "<number_of_items>", length(wildcard), "</number_of_items>",
             "</dataset_definition>")})

  # complete text
  paste0('<div class="dataset_definitions" style="visibility: hidden">',
         "<dataset_definitions>",
         paste0(.text, collapse = ""),
         "</dataset_definitions>",
         "</div>") |>
    cat()
}

