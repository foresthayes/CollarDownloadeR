#' Cell Track web scraping utilities
#'
#' @param usr The username associated with your account
#' @param pwd User's password
#' @param page_count The number of pages displayed in the Unit List or NULL, when NULL an attempt is made to detect the number of pages
#'
#' @return A tibble
#'
#' @examples
#' scrape_celltrack("my_name", "my_secret", NULL)
#'
#' @export
scrape_celltrack <- function(usr, pwd, page_count = 1){

    #  Base url for the login page
    base_url <- "https://account.celltracktech.com/accounts/login/"

    #  Connect to webpage
    pgsession <- rvest::html_session(base_url)
    httr::stop_for_status(pgsession)

    #  Extract html form
    pgform <- rvest::html_form(pgsession)[[1]]

    #  Fill in username and password
    filled_form <- rvest::set_values(
      pgform,
      'username' = usr,
      'password' = pwd
    )

    # "click" login button
    dwnld_form <- rvest::submit_form(
        session = pgsession,
        form = filled_form,
        submit = NULL,
        httr::config(referer = pgsession$url)
      )
    httr::stop_for_status(dwnld_form)

    #  Attempt to get the number of pages, if user did not supply
    pages <- purrr::when(
      page_count,
      is.null(.) ~ scrape_ctt_pages(dwnld_form),
      page_count
    )

    #  Get the links to the data and extract csv url suffix
    csvs <- purrr::map(1:pages, function(i){
      dwnld_form %>%
        rvest::jump_to(paste0(.$url, "?page=", i)) %>%
        xml2::read_html(.) %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        .[grepl("/data/connection/.*csv", .)]
    }) %>%
    unlist

    #  Read the csvs and include the unit number from the webpage
    out <- tibble::tibble(
        id = scrape_unit_names(dwnld_form, pages = pages),
        csv_path = csvs
      ) %>%
      dplyr::group_by(id) %>%
      dplyr::do(
        dat = scrape_ctt_csv(dwnld_form, .$csv_path)
      ) %>%
      tidyr::unnest()

return(out)
}

scrape_unit_names <- function(sess, pages = 1){

  out <- purrr::map(1:pages, function(i){
    sess %>%
      rvest::jump_to(paste0(.$url, "?page=", i)) %>%
      xml2::read_html(.) %>%
      rvest::html_nodes(".underline") %>%
      rvest::html_nodes("a") %>%
      rvest::html_text(.)
  }) %>%
  unlist

return(out)
}

scrape_ctt_pages <- function(sess){
  out <- sess %>%
    rvest::html_nodes(".columns") %>%
    rvest::html_text() %>%
    .[grepl("of [0-9]", .)] %>%
    gsub(".* of ", "", .) %>%
    as.integer

return(out)
}

scrape_ctt_csv <- function(sess, csv){
  out <- rvest::jump_to(sess, csv)$response$content %>%
    readr::read_csv(
      trim_ws = T,
      col_types = c("dDtTdddiidd")
    )
return(out)
}