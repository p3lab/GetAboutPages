library(testthat)
library(GetAboutPages)

context("Get about page content")

test_that("Extract about links should not return NA (with tree search)", {
    expect_equal(class(get_about_page_content("http://www.saltpondscoalition.org/")), "character")
})

test_that("Get about page content returns a character vector (without tree search", {
  expect_equal(class(get_about_page_content("https://snfagora.jhu.edu/")), "character")
})
