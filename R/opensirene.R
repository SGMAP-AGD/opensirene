#' Get company
#'
#' @param siret a valid siret number
#' @param endpoint API endpoint
#'
#' @return a list
#' @export
#'
#' @examples
#'
#' get_company(siret = "79101208100018")
#'
get_company <- function(
  siret,
  endpoint = "https://sirene.entreprise.api.gouv.fr") {
  magrittr::extract2(
    httr::content(
      httr::GET(
        paste0(endpoint, "/siret/", siret)
      )
    ),
    "etablissement"
  )
}

#' Get url of Sirene API User interface
#'
#' @param siret a valid siret number as a string
#' @param endpoint API endpoint
#'
#' @return a string
#' @export
#'
#' @examples
#'
#' get_url(siret = "79101208100018")
#'
#' \dontrun{
#' browseURL(get_url(siret = "79101208100018"))
#' }
#'
get_url <- function(siret, endpoint = "https://sirene.entreprise.api.gouv.fr") {
  paste0(endpoint, "/#/entreprise/", siret)
}

#' Search company
#'
#' search company by name
#'
#' @param string a company name as a string
#' @param endpoint API endpoint
#'
#' @return a list
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' search_company(string = "dataiku")
#'
search_company <- function(
  string,
  endpoint = "https://sirene.entreprise.api.gouv.fr") {

  paste0(endpoint, "/full_text/", string) %>%
    httr::GET() %>%
    httr::content() %>%
    purrr::modify_at(
      .at = "etablissement",
      .f = function(x) {
        purrr::map_df(
          .x = x,
          .f = function(x) {
            tibble::as_tibble(
              purrr::modify_if(
                .x = x,
                .p = purrr::is_null,
                .f = function(x) {
                  NA
                  }
              )
            )
          }
        )
      }
    )
}
