library(testthat)
library(GetAboutPages)

context("Extract about links")

test_that("Extract about links should not return NA", {
          expect_equal(unique(extract_about_links("http://www.saltpondscoalition.org/")$href), "AboutUs.html")
})
