#' Retrieve Moodle options from HTML.
#'
#' Given the text with the options for the question, retrieve them. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node HTML node that contains the options.
#'
#' @return a named character vector containing the options.
mxml_obtain_options <- function(node){
  opts <- node |>
    rvest::html_attrs()

  opts <- opts[names(opts) != 'class']

  names(opts) <- names(opts) |>
    stringr::str_remove_all('data-') |>
    stringr::str_remove_all('-') |>
    stringr::str_replace_all('qtype', 'type')

  opts[] <- opts |>
    dplyr::case_match(
      'true' ~ '1',
      'false' ~ '0',
      'relative' ~ '1',
      'nominal' ~ '2',
      'geometric' ~ '3',
      'decimal' ~ '1',
      'sigfig' ~ '2',
      .default = opts
    )

  return(opts)
}



#' Create Moodle XML code for category.
#'
#' Given the text for the name of a category, create Moodle XML code. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param text The name of the category to create.
#'
#' @return an XML node to be inserted into a document.
mxml_create_category <- function(text){
  paste0('<question type="category">',
         '<category>',
         '<text>$course$/top/', text, '</text>',
         '</category>',
         '<info format="html"><text></text></info>',
         '</question>') |>
    html2xml()
}




#' Convert HTML output to the basic components of any question.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to all question types is converted to Moodle XML. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the section of html code corresponding to the question.
#' @param type character specifying the type of question to construct.
#'
#' @return an XML node to be inserted into a document.
mxml_create_basic <- function(node, type){
  .full <- paste0('<question type="', type, '"></question>') |>
    html2xml()

  title <- node |>
    rvest::html_attr('id') |>
    type_text(type = 'name', format = NULL) |>
    html2xml()

  exclusion.classes <-
    paste0('(', c('anchored',
                  'answer',
                  'dataset',
                  'feedback',
                  'subquestion',
                  'hint',
                  'grader-info',
                  'response-template'), ')',
           collapse = '|')

  question.id <- node |>
    rvest::html_children() |>
    rvest::html_attr('class') |>
    stringr::str_detect(exclusion.classes, negate = TRUE)
  question.id <- (question.id | is.na(question.id)) |>
    which()

  question <- node |>
    rvest::html_children() |>
    magrittr::extract(question.id) |>
    cdata_it() |>
    type_text(type = 'questiontext') |>
    html2xml()

  feedback <- node |>
    rvest::html_element('div.general-feedback')

  if (length(feedback) > 0L) {
    feedback <- feedback |>
      rvest::html_children() |>
      cdata_it() |>
      type_text(type = 'generalfeedback') |>
      html2xml()
  } else {
    feedback <- type_text('', type = 'generalfeedback', format = NULL) |>
      html2xml()
  }

  xml2::xml_add_child(.full, title)
  xml2::xml_add_child(.full, question)
  xml2::xml_add_child(.full, feedback)

  .full
}




#' Convert HTML output for Calculated Questions into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to a Calculated Question is converted to Moodle XML. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the html node corresponding to the calculated
#' question.
#' @param opts named character vector of options associated with this question,
#' generally the result of a call to \code{\link{mxml_obtain_options}}.
#'
#' @return an XML node to be inserted into a document.
mxml_read_calculated <- function(node, opts){
  .full <- mxml_create_basic(node, "calculated")

  opts <- xmloption(names(opts), opts) |>
    lapply(html2xml)
  for (o in opts) xml2::xml_add_child(.full, o)

  defaults <- c(
    'fraction' = '100',
    'tolerance' = '0.01',
    'tolerancetype' = '2',
    'correctanswerformat' = '1',
    'correctanswerlength' = '2')

  answers <- node |>
    rvest::html_elements('div.answer') |>
    lapply(function(u){
      feedback <- u |>
        rvest::html_element('.feedback')

      if (length(feedback) > 0) {
        xml2::xml_remove(feedback)

        feedback <- feedback |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = 'feedback') |>
          html2xml()
      } else {
        feedback <- type_text('', type = 'feedback', format = NULL) |>
          html2xml()
      }

      aopts <- u |>
        mxml_obtain_options()

      if (any(!is.element(names(aopts), names(defaults)))) {
        warning('Invalid answer options given on a calculated question.')
      }

      aopts <- aopts[is.element(names(aopts), names(defaults))]

      aopts[setdiff(names(defaults), names(aopts))] <-
        defaults[setdiff(names(defaults), names(aopts))]

      .answer <- paste0('<answer fraction="', aopts['fraction'],
                        '"></answer>') |>
        html2xml()

      soln <- u |>
        rvest::html_text()

      aopts <- aopts[names(aopts) != 'fraction']
      .adds <- xmloption(c("text", names(aopts)),
                         c(soln, aopts)) |>
        lapply(html2xml)

      for (a in .adds) xml2::xml_add_child(.answer, a)
      xml2::xml_add_child(.answer, feedback)

      .answer
    })

  for (a in answers) xml2::xml_add_child(.full, a)

  hints <- node |>
    rvest::html_elements('.hint')

  if (length(hints) > 0){
    hints <- hints |>
      lapply(function(u){
        u |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = "hint") |>
          html2xml()
      })

    for (h in hints) xml2::xml_add_child(.full, h)
  }


  datasets <- node |>
    rvest::html_element('div.dataset_definitions') |>
    xml2::xml_child()


  xml2::xml_add_child(.full, datasets)

  .full
}



#' Convert HTML output for Cloze Questions into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to a Description Question is converted to Moodle XML. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the section of html code corresponding to the description
#' question.
#'
#' @return an XML node to be inserted into a document.
mxml_read_cloze <- function(node, opts){
  .full <- mxml_create_basic(node, "cloze")

  opts <- xmloption(names(opts), opts) |>
    lapply(html2xml)
  for (o in opts) xml2::xml_add_child(.full, o)

  hints <- node |>
    rvest::html_elements('.hint')

  if (length(hints) > 0){
    hints <- hints |>
      lapply(function(u){
        u |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = "hint") |>
          html2xml()
      })

    for (h in hints) xml2::xml_add_child(.full, h)
  }

  .full
}




#' Convert HTML output for Description Questions into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to a Description Question is converted to Moodle XML. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the section of html code corresponding to the description
#' question.
#'
#' @return an XML node to be inserted into a document.
mxml_read_description <- function(node){
  mxml_create_basic(node, "description")
}




#' Convert HTML output for Essay Questions into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to a Essay Question is converted to Moodle XML. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the section of html code corresponding to the calculated
#' question.
#' @param opts named character vector of options associated with this question,
#' generally the result of a call to \code{\link{mxml_obtain_options}}.
#'
#' @return an XML node to be inserted into a document.
mxml_read_essay <- function(node, opts){
  .full <- mxml_create_basic(node, "essay")

  opts <- xmloption(names(opts), opts) |>
    lapply(html2xml)
  for (o in opts) xml2::xml_add_child(.full, o)

  graderinfo <- node |>
    rvest::html_element('.grader-info')

  if (length(graderinfo) > 0){
    graderinfo <- graderinfo |>
      rvest::html_children() |>
      cdata_it() |>
      type_text(type = "graderinfo") |>
      html2xml()
  } else {
    graderinfo <- type_text("", type = "graderinfo", NULL) |>
      html2xml()
  }

  responsetemplate <- node |>
    rvest::html_element('.response-template')

  if (length(responsetemplate) > 0){
    responsetemplate <- responsetemplate |>
      rvest::html_children() |>
      cdata_it() |>
      type_text(type = "responsetemplate") |>
      html2xml()
  } else {
    responsetemplate <- type_text("", type = "responsetemplate", NULL) |>
      html2xml()
  }


  xml2::xml_add_child(.full, graderinfo)
  xml2::xml_add_child(.full, responsetemplate)

  .full
}


#' Convert HTML output for Matching Questions into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to a Matching Question is converted to Moodle XML. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the section of html code corresponding to the calculated
#' question.
#' @param opts named character vector of options associated with this question,
#' generally the result of a call to \code{\link{mxml_obtain_options}}.
#'
#' @return an XML node to be inserted into a document.
mxml_read_matching <- function(node, opts){
  .full <- mxml_create_basic(node, "matching")

  opts <- xmloption(names(opts), opts) |>
    lapply(html2xml)
  for (o in opts) xml2::xml_add_child(.full, o)

  answers <- node |>
    rvest::html_elements('.answer')

  xml2::xml_remove(answers)

  answers <- answers |>
    lapply(function(u) {
      .answer <- u |>
        rvest::html_text() |>
        type_text(type = 'answer', format = NULL) |>
        html2xml()

      .answer
    })

  subQs <- node |>
    rvest::html_elements('.subquestion') |>
    lapply(function(u){
      .subq <- u |>
        rvest::html_children() |>
        cdata_it() |>
        type_text(type = 'subquestion') |>
        html2xml()

      .subq
    })

  if (length(subQs) < 2){
    stop("For matching questions, must give at least 2 subquestions.")
  }

  if (length(answers) < 3){
    stop("For matching questions, must give at least 3 answers.")
  }

  if (length(answers) > length(subQs)){
    for (i in (length(subQs) + 1):(length(answers))){
      subQs[[i]] <- type_text('', type = 'subquestion') |>
        html2xml()
    }
  }


  for (i in 1:length(subQs)) xml2::xml_add_child(subQs[[i]], answers[[i]])

  for (s in subQs) xml2::xml_add_child(.full, s)

  hints <- node |>
    rvest::html_elements('.hint')

  if (length(hints) > 0){
    hints <- hints |>
      lapply(function(u){
        u |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = "hint") |>
          html2xml()
      })

    for (h in hints) xml2::xml_add_child(.full, h)
  }

  .full
}


#' Convert HTML output for Drag and Drop Matching Questions into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to a Drag and Drop Matching Question is converted to Moodle XML.
#' This is called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the section of html code corresponding to the calculated
#' question.
#' @param opts named character vector of options associated with this question,
#' generally the result of a call to \code{\link{mxml_obtain_options}}.
#'
#' @return an XML node to be inserted into a document.
mxml_read_dragdrop <- function(node, opts){
  .full <- mxml_create_basic(node, "ddmatch")

  opts <- xmloption(names(opts), opts) |>
    lapply(html2xml)
  for (o in opts) xml2::xml_add_child(.full, o)

  answers <- node |>
    rvest::html_elements('.answer')

  xml2::xml_remove(answers)

  answers <- answers |>
    lapply(function(u) {
      .answer <- u |>
        rvest::html_children() |>
        cdata_it() |>
        type_text(type = 'answer') |>
        html2xml()

      .answer
    })

  subQs <- node |>
    rvest::html_elements('.subquestion') |>
    lapply(function(u){
      .subq <- u |>
        rvest::html_children() |>
        cdata_it() |>
        type_text(type = 'subquestion') |>
        html2xml()

      .subq
    })

  if (length(subQs) < 2){
    stop("For matching questions, must give at least 2 subquestions.")
  }

  if (length(answers) < 3){
    stop("For matching questions, must give at least 3 answers.")
  }

  if (length(answers) > length(subQs)){
    for (i in (length(subQs) + 1):(length(answers))){
      subQs[[i]] <- type_text('', type = 'subquestion') |>
        html2xml()
    }
  }


  for (i in 1:length(subQs)) xml2::xml_add_child(subQs[[i]], answers[[i]])

  for (s in subQs) xml2::xml_add_child(.full, s)

  hints <- node |>
    rvest::html_elements('.hint')

  if (length(hints) > 0){
    hints <- hints |>
      lapply(function(u){
        u |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = "hint") |>
          html2xml()
      })

    for (h in hints) xml2::xml_add_child(.full, h)
  }

  .full
}


#' Convert HTML output for Multichoice Questions into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to a Multichoice Question is converted to Moodle XML. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the section of html code corresponding to the calculated
#' question.
#' @param opts named character vector of options associated with this question,
#' generally the result of a call to \code{\link{mxml_obtain_options}}.
#'
#' @return an XML node to be inserted into a document.
mxml_read_multichoice <- function(node, opts){
  .full <- mxml_create_basic(node, "multichoice")

  opts <- xmloption(names(opts), opts) |>
    lapply(html2xml)
  for (o in opts) xml2::xml_add_child(.full, o)

  answers <- node |>
    rvest::html_elements('div.answer') |>
    lapply(function(u){
      feedback <- u |>
        rvest::html_element('.feedback')

      if (length(feedback) > 0) {
        xml2::xml_remove(feedback)

        feedback <- feedback |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = 'feedback') |>
          html2xml()
      } else {
        feedback <- type_text('', type = 'feedback', format = NULL) |>
          html2xml()
      }

      fraction <- u |>
        rvest::html_attr('data-fraction')

      .answer <- paste0('<answer fraction="', fraction, '"></answer>') |>
        html2xml()

      soln <- u |>
        rvest::html_children() |>
        cdata_it() |>
        type_text() |>
        html2xml()

      xml2::xml_add_child(.answer, soln)
      xml2::xml_add_child(.answer, feedback)

      .answer
    })

  for (a in answers) xml2::xml_add_child(.full, a)

  hints <- node |>
    rvest::html_elements('.hint')

  if (length(hints) > 0){
    hints <- hints |>
      lapply(function(u){
        u |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = "hint") |>
          html2xml()
      })

    for (h in hints) xml2::xml_add_child(.full, h)
  }

  .full
}


#' Convert HTML output for Numerical Questions into Moodle XML.
#'
#' HTML code generated from knitting together an RMarkdown document which
#' corresponds to a Numerical Question is converted to Moodle XML. This is
#' called from within \code{\link{html_to_moodle}}.
#'
#'
#' @param node the section of html code corresponding to the calculated
#' question.
#' @param opts named character vector of options associated with this question,
#' generally the result of a call to \code{\link{mxml_obtain_options}}.
#'
#' @return an XML node to be inserted into a document.
mxml_read_numerical <- function(node, opts){
  .full <- mxml_create_basic(node, "numerical")

  opts <- xmloption(names(opts), opts) |>
    lapply(html2xml)
  for (o in opts) xml2::xml_add_child(.full, o)

  defaults <- c(
    'fraction' = '100',
    'tolerance' = '0')

  answers <- node |>
    rvest::html_elements('div.answer') |>
    lapply(function(u){
      feedback <- u |>
        rvest::html_element('.feedback')

      if (length(feedback) > 0) {
        xml2::xml_remove(feedback)

        feedback <- feedback |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = 'feedback') |>
          html2xml()
      } else {
        feedback <- type_text('', type = 'feedback', format = NULL) |>
          html2xml()
      }

      aopts <- u |>
        mxml_obtain_options()

      aopts <- aopts[is.element(names(aopts), names(defaults))]
      aopts[setdiff(names(defaults), names(aopts))] <-
        defaults[setdiff(names(defaults), names(aopts))]

      .answer <- paste0('<answer fraction="', aopts['fraction'],
                        '" format="moodle_auto_format"></answer>') |>
        html2xml()

      soln <- u |>
        rvest::html_text()

      .adds <- xmloption(c("text", "tolerance"),
                         c(soln, aopts['tolerance'])) |>
        lapply(html2xml)

      for (a in .adds) xml2::xml_add_child(.answer, a)
      xml2::xml_add_child(.answer, feedback)

      .answer
    })

  for (a in answers) xml2::xml_add_child(.full, a)

  hints <- node |>
    rvest::html_elements('.hint')

  if (length(hints) > 0){
    hints <- hints |>
      lapply(function(u){
        u |>
          rvest::html_children() |>
          cdata_it() |>
          type_text(type = "hint") |>
          html2xml()
      })

    for (h in hints) xml2::xml_add_child(.full, h)
  }

  .full
}