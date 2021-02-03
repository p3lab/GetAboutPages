library(testthat)
library(GetAboutPages)

context("Get about page content")

test_that("Extract about links should not return NA (with tree search)", {
    
    # Test 1 
    expect_equal(class(get_about_page_content("http://www.saltpondscoalition.org/")), "character")
    
    # Test 2 
    expect_equal(class(get_about_page_content("https://www.sierraclub.org/")), "character")
    
    # Test 3 
    expect_equal(class(get_about_page_content("https://front.moveon.org")), "character")
    
})

test_that("Get about page content returns a character vector (without tree search)", {
  expect_equal(class(get_about_page_content("https://snfagora.jhu.edu/")), "character")
})

test_that("Get about page from a website built by PHP", {
  expect_equal(class(get_about_page_content("http://www.egacademyfoundation.org")), "character")
})