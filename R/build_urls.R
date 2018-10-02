#' Functions to build the url required to call the API
#'
#' The url is composed of a base that never changes, a collar id, a collar key
#' and data type at a minimum.  These pieces and the optional parameters
#' after_data_id and start_date must be put together in a rather particular
#' order with appropriate punctuation.  This is the role of these functions.
#'
#' @param base_url NULL or the url to build from, the default NULL should
#' suffice in almost all cases
#' @param collar_id The ID(s) of the collars to query for data
#' @param collar_key The key(s) of the collars to query for data
#' @param type The data type, poorly defined at the moment
#' @param after_data_id Some data types have ID numbers, if you wish to only
#' download data after some data id then supply it here and the API should only
#' return data with an ID greater than that supplied.  Must be equal in length
#' to collar_id and collar_key.  Only one of start_date and
#' after_data_id may be supplied.
#' @param start_date A date class vector.  Supplying this parameter will cause
#' the API to only return data collected after the supplied date.  Must be equal
#' in length to collar_id and collar_key.  Only one of start_date and
#' after_data_id may be supplied.
#'
#' @details The functions cdr_build_vec_url and cdr_build_vec_urls differ only
#' in that cdr_build_vec_url builds a single url from a collection of length 1
#' arguments while cdr_build_vec_url will function on vectors of inputs to
#' create numerous urls.  Functionally the user is encouraged to only use the
#' plural version so that code will function regardles of the length of the
#' input.
#'
#' @return A url as a character string
#' @export
#'
#' @examples
#' cdr_build_vec_urls(
#'   base_url = NULL,
#'   collar_id = cdr_get_id_from_key(
#'     system.file("extdata", package = "CollarDownloadeR")
#'   ),
#'   collar_key = cdr_get_keys(
#'     system.file(
#'       "extdata",
#'       package = "CollarDownloadeR"
#'     )
#'   ),
#'   type = "act"
#' )
cdr_build_vec_url <- function(base_url = NULL,
                              collar_id = NULL,
                              collar_key = NULL,
                              type = c("act", "csq", "mit"),
                              after_data_id = NULL,
                              start_date = NULL) {
  one_null <- function(x, y) {
    is.null(x) | is.null(y)
  }

  assertthat::on_failure(one_null) <- function(call, env) {
    "The Vectronics API cannot accept both the after_data_id and start_date arguments at the same time.  Please change one or both to NULL."
  }

  assertthat::assert_that(one_null(after_data_id, start_date))

  if (is.null(base_url)) {
    base_url <- "https://wombat.vectronic-wildlife.com:9443"
  }

  url <- httr::modify_url(
    base_url,
    path = list(
      "collar",
      collar_id,
      type
    ),
    query = list(
      "collarkey" = collar_key
    )
  )

  if (!is.null(after_data_id)) {
    url <- paste0(url, "&gt-id=", after_data_id)
  }

  if (!is.null(start_date)) {
    assertthat::assert_that(assertthat::is.date(start_date))

    frmt_start_date <- format(start_date, "%d-%m-%Y")

    url <- paste0(url, "&after=", frmt_start_date)
  }

  url
}

cdr_build_vec_urls <- function(base_url = NULL,
                               collar_id = NULL,
                               collar_key = NULL,
                               type = c("act", "csq", "mit"),
                               after_data_id = NULL,
                               start_date = NULL) {
  equal_length <- function(x, y) {
    length(x) == length(y)
  }

  assertthat::on_failure(equal_length) <- function(call, env) {

  }

  purrr::map2_chr(
    collar_id, collar_key,
    ~cdr_build_vec_url(
      base_url,
      .x,
      .y,
      type,
      after_data_id,
      start_date
    )
  )
}