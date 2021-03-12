#' Check whether the href attribute contains "about" or not
#'
#' @param href An href attribute
#'
#' @return Either an href attribute (YES) or NA (NO)
#' @importFrom stringr str_detect
#' @export

if_not_about <- function(href) {
  if ((TRUE %in% (href %>% tolower() %>% str_detect("about"))) &
    (any(href %>% tolower() %>% str_detect("about") == TRUE))) {
    return(href)
  } else {
    return(NA)
  }
}

#' Extract links and other information related to about page
#'
#' @param base_url A base URL (the base part of the web address)
#' @param timeout_thres A timeout threshold. The default value is 10 seconds. 
#'
#' @return If successful, the function returns a dataframe of two columns ("href", "link"). If not successful, the function returns a dataframe of three columns ('href', 'link_text', link'). In this dataframe, href should be NA and link_test should inform one of the following five error cases: "Found without tree search.", "This website is broken.", "The website is flat (no tree structure).", "PHP error", or "The website does not have about page."
#' @importFrom stringr str_detect
#' @importFrom stringr str_match
#' @importFrom stringr str_remove
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom glue glue
#' @importFrom RCurl curlOptions
#' @importFrom RCurl url.exists
#' @importFrom tibble tibble
#' @importFrom purrr possibly
#' @importFrom furrr future_map_int
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom xml2 read_html
#' @importFrom jsonlite fromJSON
#' @export
#' 

extract_about_links <- function(base_url, timeout_thres = 10) {

  # Timeout to prevent hanging on unreachable/very slow websites
  if (url.exists(base_url, timeout = timeout_thres) == FALSE) {
    stop(glue("This URL is not responding ({timeout_thres} seconds timeout)."))} 
  
  # First, try looking for it directly
  if (!grepl("/$", base_url)) {
    base_url <- glue("{base_url}/")
  }

  possible_about_urls <- c(
    glue("{base_url}about-us"), # About case 1
    glue("{base_url}about"), # About case 2
    glue("{base_url}who-we-are")
  ) # Who we are case

  # Follow redirections
  opts <- curlOptions(followlocation = TRUE)

  # Check whether a request for the specific URL works without error
  if (sum(future_map_int(possible_about_urls, ~ url.exists(., .opts = opts))) >= 1) {

    # Dataframe with three columns
    about_links <- tibble(
      href = "Base",
      link_text = "Found without tree search.",
      link = possible_about_urls[which(future_map_int(possible_about_urls, ~ url.exists(., .opts = opts)) == 1)]
    )

  } else {

    ## else try looking for a suitable link

    possible_read_html <- possibly(read_html, otherwise = "This URL is broken.")

    pg <- possible_read_html(base_url)

    if ("xml_document" %in% class(pg) == FALSE) {

      # Dataframe with three columns
      about_links <- tibble(
        href = NA,
        link_text = "This website is broken.",
        link = base_url
      )
    }

    # else if checking whether the website is built by Wix
    else if (TRUE %in% grepl("Wix.com", 
                             html_nodes(pg, "meta") %>% 
                             html_attr("content"))) {
      
      emailjs <- pg %>%
        html_nodes("script") %>%
        html_text()
      
      script_idx <- which(grepl("rendererModel", emailjs))
      
      res <- str_match(emailjs[script_idx], "var rendererModel =\\s*(.*?)\\s*;")
      
      res <- fromJSON(res[2])
      
      page_list <- res$pageList$pages

      about_pages <- page_list %>%
        filter(grepl("about", tolower(title)) | grepl("about", tolower(pageUriSEO))) %>%
        select(pageUriSEO)

      # Dataframe with three columns
      about_links <- tibble(
        href = "Base",
        link_text = "The website is built by Wix.",
        link = about_pages <- glue("{base_url}/{about_pages}")
      )
    }

    else {

      # URL of pages
      href <- pg %>% html_nodes("a") %>% html_attr("href")

      # Alternative
      link_text <- pg %>% html_nodes("a") %>% html_text()

      if (length(href) == 0) {

        # Data frame with three columns
        about_links <- tibble(
          href = NA,
          link_text = "The website is flat (no tree structure).",
          link = base_url
        )
      }

      # Dataframe with three columns
      else if (sum(c(is.na(if_not_about(tolower(href))), is.na(if_not_about(tolower(link_text))))) > 0) {

        # Data frame with three columns
        about_links <- tibble(
          "href" = NA,
          "link_text" = "The website does not have about page.",
          "link" = base_url
        )
      } else {
        
        
        df <- tibble(
          "href" = unique(if_not_about(href)), # Don't make it lower case (URLs are case sensitive)
          "link_text" = if_not_about(tolower(link_text))[1],
          "link" = base_url
        ) 
        
        about_links <- df %>%
          filter(str_detect(tolower(link_text), "about") |
                   str_detect(tolower(href), "about")) %>%
          filter(!is.na(href)) %>%
          distinct() 
        
        # If link formatting is off because the page uses an absolute reference        
        if (str_detect(about_links$href, "http")) {

        # TRUE (absolute path)
        about_links <- about_links %>%
          mutate(link = href) %>%
          mutate(href = "Absolute link") %>%
          select(href, link)
        
        } else {
        
          # FALSE (relative path)
          
          # Two or more depths  
          if (str_count(about_links$link_text, "\\\n") >= 2) {
            
            about_links <- about_links %>%
              # Remove only the first / 
              mutate(href = str_replace(href, "/", "")) %>%
              mutate(link = glue("{base_url}{href}")) %>%
              select(href, link)
          }
          
          else {
            
         # One depth 
        about_links <- about_links %>%
          # Remove every / 
          mutate(href = str_replace_all(href, "/", "")) %>%
          mutate(link = glue("{base_url}{href}")) %>%
          select(href, link)
        
          }
        
        }
        
        return(about_links)
        
      }
    }

    return(about_links)
  }

  return(about_links)
}

#' Find links and other information related to about page
#'
#' @param base_url A base URL (the base part of the web address)
#'
#' @return If successful, the function returns a working base URL. If not, the function reports one of the following four error cases: "This link does not have about page.", "This link has a PHP error.", "This link is flat (not tree structure).", or "This link is broken."
#' @importFrom glue glue
#' @importFrom dplyr if_else
#' @importFrom dplyr pull
#' @importFrom dplyr first
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom xml2 url_absolute 
#' @export

find_about_link <- function(base_url) {
  
  about_links <- extract_about_links(base_url)

  # Find cases where links are broken or links don't have about pages
  if (NA %in% about_links$href == TRUE) {

    # if_else is slightly faster than ifelse
    about_url <- if_else(str_detect(about_links$link_text, "have about page"), # About page problem
      glue("This {about_links$link} does not have about page."),
      if_else(str_detect(about_links$link_text, "PHP"), # PHP problem
        glue("This {about_links$link} has a PHP error."),
        if_else(str_detect(about_links$link_text, "(no tree structure)"), # Flat tree problem
          glue("This {about_links$link} is flat (not tree structure)."),
          glue("This {about_links$link} is broken.") # Broken URL problem
        )
      )
    )
  } else {
    
    about_url <- about_links %>% pull("href") %>% unique() %>% first()

    # Base and target cases
    if (about_url == "Base") {
      
      about_url <- about_links$link %>% first()
    
      } else if (str_detect(about_url, "Absolute")) {
        
      about_url <- about_links$link
      
      } else {
        
        about_url <- glue("{base_url}{about_url}")
        
      }
    
    about_url <- str_replace(about_url, "(.*?//.*?)//(.*?)", "\\1/\\2")
    
  }

  # about_url
  return(about_url)
}

#' Extract about page content
#'
#' @param about_url An URL for an about page
#'
#' @return About page text (character vector)
#' @importFrom htm2txt gettxt
#' @importFrom textclean strip
#' @export

extract_about_page_content <- function(about_url) {

  # Broken, flat, PHP error, about page issues
  if (str_detect(about_url, "tree search|is broken|is flat|PHP error|about page") == TRUE) {

    # Output: dataframe
    about_url_text <- about_url
  }

  # Other cases
  else {
    about_page <- gettxt(about_url) %>%
      strip(digit.remove = FALSE) # more comprehensive and efficient text cleaning

    # Output: dataframe
    about_url_text <- about_page
  }

  return(about_url_text)
}

#' Get about page content
#'
#' @param base_url A base URL (the base part of the web address)
#'
#' @return about_page A dataframe that contains about page text 
#' @export

get_about_page_content <- function(base_url) {

  # Search prospective about URLs
  about_url <- find_about_link(base_url)

  # Extract information
  about_url_text <- extract_about_page_content(about_url)

  # Close all URL connections
  on.exit(closeAllConnections())

  about_page <- data.frame("about_page" = about_url_text)
  
  return(about_page)
}
