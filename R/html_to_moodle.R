#' Convert HTML output into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to several questions is converted to Moodle XML. This function
#' writes an XML file with the same name as the input file.
#'
#'
#' @param file the HTML file that should be converted.
#'
#' @return invisible
#'
#' @export
html_to_moodle <- function(file){
  raw <- xml2::read_html(file)

  .out <- xml2::xml_new_root("quiz")

  .questions <- raw |>
    rvest::html_elements("section.moodle-question")

  for (q in .questions){
    opts <- mxml_obtain_options(q)

    if (is.element("category", names(opts))){
      category <- mxml_create_category(opts["category"])

      xml2::xml_add_child(.out, category)

      opts <- opts[-which(names(opts)=="category")]
    }

    type <- opts["type"]
    opts <- opts[-which(is.element(names(opts), c("id", "type")))]

    if (type != 'description') {
      defaults <- default_options(type)

      # only keep elements which have a valid option
      if (any(!is.element(names(opts), names(defaults)))) {
        warning(
          paste0('Invalid option names provided on one of the ',
                 type, ' questions.'))
      }

      opts <- opts[is.element(names(opts), names(defaults))]
      defaults[names(opts)] <- opts
      opts <- defaults
    }

    fullq <- switch(type,
                    "calculated" = mxml_read_calculated(q, opts),
                    "description" = mxml_read_description(q),
                    "essay" = mxml_read_essay(q, opts),
                    "matching" = mxml_read_matching(q, opts),
                    "ddmatch" = mxml_read_dragdrop(q, opts),
                    "multichoice" = mxml_read_multichoice(q, opts),
                    "numerical" = mxml_read_numerical(q, opts))

    xml2::xml_add_child(.out, fullq)
  }

  xml2::write_xml(.out, paste0(stringr::str_sub(file, end = -5), "xml"))
  invisible(.out)
}