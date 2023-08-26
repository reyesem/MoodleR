#' Generate wildcard datasets given a template.
#'
#' Wildcards hold the various problems in Moodle. When you want each student
#' to have a unique dataset, there are many wildcards to generate. This
#' provides a way of generating the wildcards given a template of how to
#' construct an individual dataset.
#'
#'
#' @param df.fct Function which generates a single dataset that could be given
#' to a student. This function should not take any parameters.
#' @param soln.fct Function that computes additional wildcards from the dataset,
#' which are often the solutions to calculated questions. This function should
#' take a single argument (the resulting call of \code{df.fct}) and return a
#' named vector of computed values.
#' @param replicates The number of datasets to generate (max is 50).
#' @param cols columns in the result of \code{df.fct} that should not be
#' considered wildcards. Takes any form of \code{dplyr::tidyselect}.
#'
#' @return a dataframe that contains all the wildcards and their values.
#'
#' @importFrom rlang !!
#' @export
generate_wildcards <- function(df.fct, soln.fct, replicates = 50, cols = NULL){
  if (replicates > 50) replicates <- 50

  dfList <- lapply(seq_len(replicates), function(u) df.fct())

  solns <- purrr::map(dfList, soln.fct) |>
    do.call(what = "rbind") |>
    tibble::as_tibble()

  if (!is.null(substitute(cols))){
    dfList <- purrr::map(dfList,
                         function(df){
                           cols <- dplyr::enquo(cols)
                           dplyr::select(df, -(!! cols))
                         })
  }

  out <- purrr::map(
    dfList,
    function(df){
      df <- df |>
        dplyr::mutate(.names = seq_len(nrow(df)))

      df |>
        tidyr::pivot_wider(
          names_from = '.names',
          values_from = -'.names',
          names_prefix = ifelse(ncol(df) == 2,
                                paste0(colnames(df)[1], "_"), "")
        )
    }
  ) |>
    purrr::list_rbind()

  out <- dplyr::bind_cols(out, solns)

  out
}



#' Generate r code text given a template.
#'
#' Wildcards hold the various problems in Moodle. When you want each student
#' to have a unique dataset, there are many wildcards to generate. You must
#' then specify to students how to read in this data, the code for which will
#' contain Moodle's notation for wildcards. This provides a way of writing such
#' code.  It is to be used alongside \code{\link{generate_wildcards}}.
#'
#' This prints code to the screen using \code{cat}.
#'
#'
#' @param df.fct Function which generates a single dataset that could be given
#' to a student. This function should not take any parameters.
#' @param cols columns in the result of \code{df.fct} that should not be
#' considered wildcards. Takes any form of \code{dplyr::tidyselect}.
#' @param name the name of the resulting dataset (default = \code{"df"}).
#' @param type one of \code{"tibble"} or \code{"data.frame"} that specifies the
#' type of output that should be created from the code.
#'
#' @return invisible
#'
#' @importFrom rlang !!
#' @export
generate_rcode <- function(df.fct, cols = NULL, name = "df",
                           type = c("tibble", "data.frame")){
  type <- match.arg(type)

  df <- df.fct()

  .mynames <- function(u){
    paste0("{", substitute(u), "_", seq_along(u), "}")
  }

  .convert <- function(u){
    if (is.numeric(u) || is.logical(u)){
      return(u)
    } else if (is.character(u) || is.factor(u)){
      out <- paste0('"', u, '"')
      return(out)
    }
  }

  if (is.null(substitute(cols))){
    df <- df |>
      dplyr::mutate_all(.mynames)
  } else {
    cols <- dplyr::enquo(cols)
    keep <- dplyr::vars(!! cols)
    wild <- dplyr::vars(-(!! cols))

    df <- df |>
      dplyr::mutate(dplyr::across(wild, .mynames)) |>
      dplyr::mutate(dplyr::across(keep, .convert))
  }

  out <- purrr::map_chr(
    df,
    function(u){
      paste0('c(', paste0(u, collapse = ', '), ')')
    }
  )

  out <- paste0(names(out), ' = ', out, collapse = ',\n  ')

  cat(name, ' = ', type, '(\n  ',
      out, '\n',
      ')', sep = '')
}
