#' Convert a Moodle quiz to a Microsoft Word paper survey.
#'
#' Take a Moodle XML file and create a Microsoft Word version suitable for
#' distributing a paper quiz.
#'
#'
#' @param file the Moodle XML file that should be converted.
#' @param solution if `TRUE`, solutions are included in the output. If `FALSE`
#' (default), solutions are not included.
#' @param title string indicating the title of the quiz (default = 'Quiz').
#'
#' @return invisible
#'
#' @export
moodle_to_word <- function(file,
                           solution = FALSE,
                           title = 'Quiz'){

  # Use XML to create a structure, and then use write_html to write an html file
  # Then, use pandoc to convert the html to word, which should retain all structure
  # Will need to use xml2::write_html and rmarkdown::pandoc_convert along with
  # tempfile in base.

  .out <- rvest::read_html(paste0('<h1>', title, '</h1>'))

  raw <- xml2::read_xml(file)

  .questions <- raw |>
    rvest::html_elements('question')

  .questions <- .questions |>
    magrittr::extract(
      rvest::html_attr(.questions, 'type') != 'category'
    )


  qnumber <- 1
  myseed <- sample(1e09, size = 1)
  for (i in seq_along(.questions)){
    q <- .questions[[i]]
    type <- rvest::html_attr(q, 'type')

    fullq <- switch(type,
                    "calculated" = word_create_calculated(q, qnumber, solution, myseed),
                    "cloze" = word_create_cloze(q, qnumber, solution),
                    "description" = word_create_description(q),
                    "essay" = word_create_essay(q, qnumber, solution),
                    "matching" = word_create_matching(q, qnumber, solution),
                    "ddmatch" = word_create_ddmatch(q, qnumber, solution),
                    "multichoice" = word_create_multichoice(q, qnumber, solution),
                    "numerical" = word_create_numerical(q, qnumber, solution))

    if (type != 'description') {
      qnumber <- qnumber + 1
    }

    xml2::xml_add_child(xml2::xml_child(.out, 'body'), fullq)
  }

  .outfile <- tempfile(fileext = '.html')
  xml2::write_html(.out, .outfile)
  rmarkdown::pandoc_convert(
    .outfile,
    to = 'docx',
    from = 'html+tex_math_single_backslash',
    output = paste0(stringr::str_sub(normalizePath(file), end = -4), 'docx'))

  invisible(.out)
}