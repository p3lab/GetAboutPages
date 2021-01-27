library(testthat)
library(GetAboutPages)

context("Get about page content")

test_that("Get about page content returns a character vector", {
  expect_equal(class(get_about_page_content("https://snfagora.jhu.edu/")), "character")
})

