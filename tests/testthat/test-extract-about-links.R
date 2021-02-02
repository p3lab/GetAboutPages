library(testthat)
library(GetAboutPages)

context("Extract about links")

test_that("Extract about links should not return NA (with tree search)", {
    
    expect_equal(unique(extract_about_links("http://www.saltpondscoalition.org/")$href), "AboutUs.html")
    
})

test_that("Extract about links should not return NA (without tree search)", {
    
    expect_equal(unique(extract_about_links("https://snfagora.jhu.edu/")$link), "https://snfagora.jhu.edu/about")
    
})