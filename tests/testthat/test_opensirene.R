library("opensirene")
context(desc = "Testing open sirene")

test_that(
  desc = "get_company returns a list",
  code = expect_is(
    object = get_company(siret = "79101208100018"),
    class = "list"
    )
  )

test_that(
  desc = "get_url returns a string",
  code = expect_is(
    object = get_url(siret = "79101208100018"),
    class = "character"
  )
)

test_that(
  desc = "search_company returns a list",
  code = expect_is(
    object = search_company(string = "dataiku"),
    class = "list"
  )
)
