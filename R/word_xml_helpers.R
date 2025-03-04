#' Convert Moodle XML to basic components of any question output to the basic
#' components of any question.
#'
#' Moodle XML code that creates a question is converted to markdown syntax for
#' writing to Microsoft Word.
#'
#'
#' @param node the section of html code corresponding to the question.
#' @param qnumber the question number to include.
#' @param solution a boolean indicating whether the solution/feedback should be
#' printed.
#'
#' @return a string of markdown text.
word_create_basic <- function(node, qnumber, solution){
  question <- node |>
    rvest::html_element('questiontext text') |>
    rvest::html_text2() |>
    rvest::read_html() |>
    rvest::html_element('body')

  xml2::xml_name(question) <- 'div'


  if (!missing(qnumber)) {
    if (qnumber > 0) {
      title <- paste0('<h4>Question ', qnumber, '</h4>') |>
        html2xml()
    } else {
      title <- paste0('<h4>Descripton</h4>') |>
        html2xml()
    }


    xml2::xml_add_child(question, title, .where = 0)
  }

  if (!missing(solution) && solution) {
    feedback <- node |>
      rvest::html_element('generalfeedback text') |>
      rvest::html_text2()

    feedback <- paste0('<p><strong>Solution:</strong></p>') |>
      paste0(feedback) |>
      rvest::read_html() |>
      rvest::html_element('body')

    xml2::xml_name(feedback) <- 'div'
    xml2::xml_add_child(question, feedback)
  }

  question
}



#' Convert Moodle XML of calculated questions into word format.
#'
#' Moodle XML code that creates a question is converted to markdown syntax for
#' writing to Microsoft Word.
#'
#'
#' @param node the section of html code corresponding to the question.
#' @param qnumber the question number to include.
#' @param solution a boolean indicating whether the solution/feedback should be
#' printed.
#' @param seed a numeric value to set the seed for determining the wildcard
#' values to use.
#'
#' @return a string of markdown text.
word_create_calculated <- function(node, qnumber, solution, seed){
  set.seed(seed)

  #need to randomly select the wildcard replacements
  wildcards <- node |>
    rvest::html_element('dataset_definitions') |>
    rvest::html_elements('dataset_definition')

  nvalues <- wildcards[[1]] |>
    rvest::html_element('itemcount') |>
    rvest::html_text() |>
    as.numeric()

  selected.value <- sample(nvalues, size = 1)


  wildcard.values <- wildcards |>
    sapply(function(u) {
      u |>
        rvest::html_elements('dataset_items dataset_item') |>
        magrittr::extract2(selected.value) |>
        rvest::html_element('value') |>
        rvest::html_text()
    })

  names(wildcard.values) <- wildcards |>
    rvest::html_element('name text') |>
    rvest::html_text()

  names(wildcard.values) <- paste0(
    '\\{', names(wildcard.values), '\\}'
  )


  question <- node |>
    rvest::html_element('questiontext text') |>
    rvest::html_text2() |>
    stringr::str_replace_all(wildcard.values) |>
    rvest::read_html() |>
    rvest::html_element('body')


  xml2::xml_name(question) <- 'div'


  if (!missing(qnumber)) {
    title <- paste0('<h4>Question ', qnumber, '</h4>') |>
      html2xml()

    xml2::xml_add_child(question, title, .where = 0)
  }

  if (!missing(solution) && solution) {
    feedback <- node |>
      rvest::html_element('generalfeedback text') |>
      rvest::html_text2()

    answer <- node |>
      rvest::html_element('answer text') |>
      rvest::html_text2() |>
      stringr::str_replace_all(wildcard.values)

    feedback <- paste0('<p><strong>Solution: ', answer, '</strong></p>') |>
      paste0(feedback) |>
      rvest::read_html() |>
      rvest::html_element('body')

    xml2::xml_name(feedback) <- 'div'
    xml2::xml_add_child(question, feedback)
  }

  question
}



#' Convert Moodle XML of description questions into word format.
#'
#' Moodle XML code that creates a question is converted to markdown syntax for
#' writing to Microsoft Word.
#'
#'
#' @param node the section of html code corresponding to the question.
#'
#' @return a string of markdown text.
word_create_description <- function(node){
  word_create_basic(node, -1)
}



#' Convert Moodle XML of essay questions into word format.
#'
#' Moodle XML code that creates a question is converted to markdown syntax for
#' writing to Microsoft Word.
#'
#'
#' @param node the section of html code corresponding to the question.
#' @param qnumber the question number to include.
#' @param solution a boolean indicating whether the solution/feedback should be
#' printed.
#'
#' @return a string of markdown text.
word_create_essay <- function(node, qnumber, solution){
  word_create_basic(node, qnumber, solution)
}


#' Convert Moodle XML of matching questions into word format.
#'
#' Moodle XML code that creates a question is converted to markdown syntax for
#' writing to Microsoft Word.
#'
#'
#' @param node the section of html code corresponding to the question.
#' @param qnumber the question number to include.
#' @param solution a boolean indicating whether the solution/feedback should be
#' printed.
#'
#' @return a string of markdown text.
word_create_matching <- function(node, qnumber, solution){
  question <- node |>
    rvest::html_element('questiontext text') |>
    rvest::html_text2() |>
    rvest::read_html() |>
    rvest::html_element('body')

  xml2::xml_name(question) <- 'div'


  if (!missing(qnumber)) {
    title <- paste0('<h4>Question ', qnumber, '</h4>') |>
      html2xml()

    xml2::xml_add_child(question, title, .where = 0)
  }

  nonempty <- node |>
    rvest::html_elements('subquestion') |>
    rvest::html_element('text') |>
    rvest::html_text2() |>
    magrittr::equals('') |>
    magrittr::not() |>
    which()

  subquestions <- node |>
    rvest::html_elements('subquestion') |>
    rvest::html_element('text') |>
    rvest::html_text2() |>
    magrittr::extract(nonempty)

  answers <- node |>
    rvest::html_elements('subquestion') |>
    rvest::html_element('answer text') |>
    rvest::html_text2()

  answerorder <- sample(seq_along(answers))
  answers <- answers[answerorder]

  questiontext <- paste0(
    '<ol type="i">',
    paste0('<li>', subquestions, '</li>', collapse = ''),
    '</ol>') |>
    rvest::read_html() |>
    rvest::html_element('body')

  xml2::xml_name(questiontext) <- 'div'
  xml2::xml_add_child(question, questiontext)

  answertext <- paste0(
    '<ol type="A">',
    paste0('<li>', answers, '</li>', collapse = ''),
    '</ol>') |>
    rvest::read_html() |>
    rvest::html_element('body')

  xml2::xml_name(answertext) <- 'div'
  xml2::xml_add_child(question, answertext)


  if (!missing(solution) && solution) {
    solutions <- (LETTERS[order(answerorder)])[nonempty] |>
      paste0(collapse = ', ')

    feedback <- node |>
      rvest::html_element('generalfeedback text') |>
      rvest::html_text2()

    feedback <- paste0('<p><strong>Solution: ', solutions, '</strong></p>') |>
      paste0(feedback) |>
      rvest::read_html() |>
      rvest::html_element('body')

    xml2::xml_name(feedback) <- 'div'
    xml2::xml_add_child(question, feedback)
  }

  question
}


#' Convert Moodle XML of drag and drop matching questions into word format.
#'
#' Moodle XML code that creates a question is converted to markdown syntax for
#' writing to Microsoft Word.
#'
#'
#' @param node the section of html code corresponding to the question.
#' @param qnumber the question number to include.
#' @param solution a boolean indicating whether the solution/feedback should be
#' printed.
#'
#' @return a string of markdown text.
word_create_ddmatch <- function(node, qnumber, solution){
  word_create_matching(node, qnumber, solution)
}


#' Convert Moodle XML of multiple choice questions into word format.
#'
#' Moodle XML code that creates a question is converted to markdown syntax for
#' writing to Microsoft Word.
#'
#'
#' @param node the section of html code corresponding to the question.
#' @param qnumber the question number to include.
#' @param solution a boolean indicating whether the solution/feedback should be
#' printed.
#'
#' @return a string of markdown text.
word_create_multichoice <- function(node, qnumber, solution){
  question <- node |>
    rvest::html_element('questiontext text') |>
    rvest::html_text2() |>
    rvest::read_html() |>
    rvest::html_element('body')

  xml2::xml_name(question) <- 'div'


  if (!missing(qnumber)) {
    title <- paste0('<h4>Question ', qnumber, '</h4>') |>
      html2xml()

    xml2::xml_add_child(question, title, .where = 0)
  }

  shuffle <- node |>
    rvest::html_element('shuffleanswers') |>
    rvest::html_text() |>
    magrittr::equals('1')

  single <- node |>
    rvest::html_element('single') |>
    rvest::html_text() |>
    magrittr::equals('1')


  answers <- node |>
    rvest::html_elements('answer')

  if (shuffle) {
    answers <- sample(answers)
  }

  answertext <- paste0(
    '<ol type="A">',
    paste0('<li>',
           answers |> rvest::html_element('text') |> rvest::html_text2(),
           '</li>', collapse = ''),
    '</ol>') |>
    rvest::read_html() |>
    rvest::html_element('body')

  xml2::xml_name(answertext) <- 'div'
  xml2::xml_add_child(question, answertext)


  if (!missing(solution) && solution) {
    if (single) {
      answers <- answers |>
        rvest::html_attr('fraction') |>
        magrittr::equals('100') |>
        which()
    } else {
      answers <- answers |>
        rvest::html_attr('fraction') |>
        as.numeric() |>
        magrittr::is_greater_than(0) |>
        which()
    }

    answers <- LETTERS[answers] |>
      paste0(collapse = ', ')

    feedback <- node |>
      rvest::html_element('generalfeedback text') |>
      rvest::html_text2()

    feedback <- paste0('<p><strong>Solution: ', answers, '</strong></p>') |>
      paste0(feedback) |>
      rvest::read_html() |>
      rvest::html_element('body')

    xml2::xml_name(feedback) <- 'div'
    xml2::xml_add_child(question, feedback)
  }

  question
}


#' Convert Moodle XML of numerical questions into word format.
#'
#' Moodle XML code that creates a question is converted to markdown syntax for
#' writing to Microsoft Word.
#'
#'
#' @param node the section of html code corresponding to the question.
#' @param qnumber the question number to include.
#' @param solution a boolean indicating whether the solution/feedback should be
#' printed.
#'
#' @return a string of markdown text.
word_create_numerical <- function(node, qnumber, solution){
  question <- node |>
    rvest::html_element('questiontext text') |>
    rvest::html_text2() |>
    rvest::read_html() |>
    rvest::html_element('body')

  xml2::xml_name(question) <- 'div'


  if (!missing(qnumber)) {
   title <- paste0('<h4>Question ', qnumber, '</h4>') |>
      html2xml()

    xml2::xml_add_child(question, title, .where = 0)
  }

  if (!missing(solution) && solution) {
    feedback <- node |>
      rvest::html_element('generalfeedback text') |>
      rvest::html_text2()

    answer <- node |>
      rvest::html_elements('answer')

    if (length(answer) > 1) {
      fractions <- answers |>
        rvest::html_attr('fraction') |>
        magrittr::equals('100') |>
        which() |>
        magrittr::extract(1)

      answer <- answer[fractions]
    }

    answer <- answer |>
      rvest::html_element('text') |>
      rvest::html_text2()

    feedback <- paste0('<p><strong>Solution: ', answer, '</strong></p>') |>
      paste0(feedback) |>
      rvest::read_html() |>
      rvest::html_element('body')

    xml2::xml_name(feedback) <- 'div'
    xml2::xml_add_child(question, feedback)
  }

  question
}

