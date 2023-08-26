# ---- xmloption ----
# Description: create the xml code for sub options that are key value pairs.
xmloption <- function(name, value){
  paste0('<', name, '>', value, '</', name, '>')
}


# ---- cdata_it ----
# Description: make sure CDATA is added to HTML elements for XML.
cdata_it <- function(code){
  if (length(code)==0){
    return("")
  } else {
    paste0("<![CDATA[", paste0(as.character(code), collapse = ""), "]]>")
  }
}


# ---- html2xml ----
# Description: convert some html code to xml node.
html2xml <- function(code, plural = FALSE){
  out <- paste0("<fake-root>", code, "</fake-root>") |>
    xml2::read_xml()

  if (plural) return(xml2::xml_children(out))
  if (!plural) return(xml2::xml_child(out))
}


# ---- type_text ----
# Description: XML code for some type of text.
type_text <- function(code, type, format = "html"){
  if (missing(type) || is.null(type)){
    out <- paste0('<text>', code, '</text>')
  } else {
    out <- paste0('<', type,
                  ifelse(!is.null(format),
                         paste0(' format="', format, '"><text>'),
                         '><text>'),
                  code,
                  '</text></', type, '>')
  }

  out
}


# ---- default_options ----
# Description: Set default options for each question type.
default_options <- function(type = c("calculated",
                                     "essay",
                                     "matching",
                                     "ddmatch",
                                     "multichoice",
                                     "numerical")) {
  type <- match.arg(type)

  if (type == 'calculated') {
    out <- c(
      'penalty' = '0',
      'defaultgrade' = '1',
      'synchronize' = '0',
      'unitgradingtype' = '0',
      'unitpenalty' = '0.1',
      'showunits' = '3',
      'unitsleft' = '0'
    )
  } else if (type == 'essay') {
    out <- c(
      'defaultgrade' = '1',
      'responseformat' = 'editor',
      'responserequired' = '1',
      'responsefieldlines' = '15',
      'attachments' = '0',
      'attachmentsrequired' = '0'
    )
  } else if (type == 'matching') {
    out <- c(
      'penalty' = '0',
      'defaultgrade' = '1',
      'shuffleanswers' = '1'
    )
  } else if (type == 'ddmatch') {
    out <- c(
      'penalty' = '0',
      'defaultgrade' = '1',
      'shuffleanswers' = '1'
    )
  } else if (type == 'multichoice') {
    out <- c(
      'penalty' = '0',
      'defaultgrade' = '1',
      'single' = '1',
      'shuffleanswers' = '1',
      'answernumbering' = 'abc'
    )
  } else if (type == 'numerical') {
    out <- c(
      'penalty' = '0',
      'defaultgrade' = '1',
      'unitgradingtype' = '0',
      'unitpenalty' = '0.1',
      'showunits' = '3',
      'unitsleft' = '0'
    )
  }

  return(out)
}