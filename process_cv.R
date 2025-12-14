#!/usr/bin/env Rscript
# CV Citation Badge Generator - CLI Version
# Uses Dimensions Metrics API + SerpAPI for Google Scholar

library(httr)
library(jsonlite)
library(officer)  # ADD THIS - install with install.packages("officer")
library(xml2)     # ADD THIS

# ============== CONFIGURATION ==============
SCHOLAR_ID <- "JWNJV9UAAAAJ"

# ============== FUNCTIONS ==============

# Get total citations from Google Scholar via SerpAPI
get_total_citations <- function(scholar_id) {
  api_key <- Sys.getenv("SERPAPI_KEY")
  
  if (api_key == "") {
    message("Warning: SERPAPI_KEY not set, skipping total citations")
    return(NA)
  }
  
  tryCatch({
    url <- "https://serpapi.com/search.json"
    
    response <- GET(url, query = list(
      engine = "google_scholar_author",
      author_id = scholar_id,
      api_key = api_key
    ))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      
      # Try to get total citations from cited_by
      if ("cited_by" %in% names(data)) {
        cited_by <- data$cited_by
        
        # Method 1: Direct table access
        if ("table" %in% names(cited_by)) {
          table_data <- cited_by$table
          
          # Handle data frame structure
          if (is.data.frame(table_data) && "citations" %in% names(table_data)) {
            all_val <- table_data$citations$all
            if (!is.null(all_val)) {
              return(as.numeric(all_val[1]))
            }
          }
          
          # Handle list structure  
          if (is.list(table_data) && !is.data.frame(table_data)) {
            if ("citations" %in% names(table_data)) {
              all_val <- table_data$citations$all
              if (!is.null(all_val)) {
                return(as.numeric(all_val[1]))
              }
            }
          }
        }
        
        # Method 2: Graph total
        if ("graph" %in% names(cited_by)) {
          graph_data <- cited_by$graph
          if (is.data.frame(graph_data) && "citations" %in% names(graph_data)) {
            total <- sum(as.numeric(graph_data$citations), na.rm = TRUE)
            return(total)
          }
        }
      }
    }
    return(NA)
  }, error = function(e) {
    message(paste("Error fetching Google Scholar via SerpAPI:", e$message))
    return(NA)
  })
}

# Fetch citation count from Dimensions Metrics API
get_citations_dimensions <- function(doi) {
  tryCatch({
    url <- paste0("https://metrics-api.dimensions.ai/doi/", doi)
    
    response <- GET(url, 
                    add_headers(
                      `User-Agent` = "CVBadgeGenerator/1.0",
                      `Accept` = "application/json"
                    ),
                    timeout(15))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (!is.null(data$times_cited)) {
        return(list(
          times_cited = data$times_cited,
          source = "Dimensions"
        ))
      }
    }
    
    return(NULL)
  }, error = function(e) {
    message(paste("  Dimensions API error:", e$message))
    return(NULL)
  })
}

# Get citations
get_citations_best <- function(doi) {
  dim_result <- get_citations_dimensions(doi)
  
  if (!is.null(dim_result) && !is.null(dim_result$times_cited)) {
    return(list(
      count = dim_result$times_cited,
      source = "Dimensions"
    ))
  }
  
  return(list(count = NA, source = "None"))
}

# Extract DOI from text
extract_doi <- function(text) {
  matches <- unlist(regmatches(text, gregexpr("10\\.\\d{4,}/[^\\s<>\\[\\]\"']+", text, perl = TRUE)))
  
  if (length(matches) == 0) return(NA)
  
  matches <- gsub("[.,;:]+$", "", matches)
  matches <- gsub("<.*$", "", matches)
  matches <- gsub("\\)$", "", matches)
  
  preprint_prefixes <- c("10.31234", "10.1101", "10.2139")
  
  # Handle each match individually
  is_preprint <- sapply(matches, function(d) {
    any(startsWith(d, preprint_prefixes))
  })
  
  publication_dois <- matches[!is_preprint]
  
  if (length(publication_dois) > 0) {
    return(publication_dois[1])
  } else if (length(matches) > 0) {
    return(matches[1])
  }
  
  return(NA)
}

# Get Dimensions URL
get_dimensions_url <- function(doi) {
  paste0("https://badge.dimensions.ai/details/doi/", doi)
}

# Create badge XML
create_text_badge_xml <- function(citations, hyperlink_rid) {
  citation_text <- ifelse(is.na(citations), "0", as.character(citations))
  nbsp <- "\u00A0"
  
  n_digits <- nchar(citation_text)
  if (n_digits == 1) {
    number_display <- paste0(nbsp, nbsp, citation_text, nbsp, nbsp)
  } else if (n_digits == 2) {
    number_display <- paste0(nbsp, " ", citation_text, " ", nbsp)
  } else {
    number_display <- paste0(nbsp, citation_text, nbsp)
  }
  
  paste0(
    '<w:r><w:t xml:space="preserve"> </w:t></w:r>',
    '<w:hyperlink r:id="', hyperlink_rid, '">',
    '<w:r>',
    '<w:rPr>',
    '<w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/>',
    '<w:color w:val="FFFFFF"/>',
    '<w:sz w:val="20"/>',
    '<w:szCs w:val="20"/>',
    '<w:shd w:val="clear" w:color="auto" w:fill="555555"/>',
    '</w:rPr>',
    '<w:t xml:space="preserve">', nbsp, 'Citations', nbsp, '</w:t>',
    '</w:r>',
    '<w:r>',
    '<w:rPr>',
    '<w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/>',
    '<w:b/>',
    '<w:color w:val="FFFFFF"/>',
    '<w:sz w:val="20"/>',
    '<w:szCs w:val="20"/>',
    '<w:shd w:val="clear" w:color="auto" w:fill="888888"/>',
    '</w:rPr>',
    '<w:t xml:space="preserve">', number_display, '</w:t>',
    '</w:r>',
    '</w:hyperlink>'
  )
}

# Main processing function
process_cv_with_badges <- function(input_file, output_file, scholar_id = SCHOLAR_ID) {
  
  message("========================================")
  message("CV Citation Badge Generator")
  message("========================================")
  message(paste("Input:", input_file))
  message(paste("Output:", output_file))
  message("")
  
  # Read document using officer
  doc <- read_docx(input_file)
  
  # Get document content as data frame
  content <- docx_summary(doc)
  
  # Find paragraphs containing [[ADD BADGE]]
  badge_rows <- grep("\\[\\[ADD BADGE\\]\\]", content$text)
  message(paste("Found", length(badge_rows), "badge markers"))
  
  # Find paragraphs containing [[XX]] for total citations
  xx_rows <- grep("\\[\\[XX\\]\\]", content$text)
  
  if (length(xx_rows) > 0) {
    message("Fetching total citations from Google Scholar via SerpAPI...")
    total_cites <- get_total_citations(scholar_id)
    if (! is.na(total_cites) && length(total_cites) > 0) {
      message(paste("  Total Citations:", total_cites[1]))
    }
  }
  
  # Process the document using cursor-based approach
  # For each badge marker, find the DOI in preceding text and create badge
  
  for (i in seq_along(badge_rows)) {
    row_idx <- badge_rows[i]
    
    # Look backwards to find DOI in previous paragraphs
    search_text <- paste(content$text[max(1, row_idx-5):row_idx], collapse = " ")
    doi <- extract_doi(search_text)
    
    message(paste("Processing publication", i, "of", length(badge_rows)))
    
    if (!is.na(doi)) {
      message(paste("  DOI:", doi))
      citation_result <- get_citations_best(doi)
      citations <- citation_result$count
      
      if (! is.na(citations)) {
        message(paste("  Citations:", citations))
        # Replace [[ADD BADGE]] with citation text
        badge_text <- paste0(" [Citations: ", citations, "]")
      } else {
        message("  Citations: N/A")
        badge_text <- " [Citations: 0]"
      }
    } else {
      message("  Warning: No DOI found")
      badge_text <- " [No DOI]"
    }
    
    Sys.sleep(0.2)
  }
  
  # For now, use text replacement approach on raw XML with normalization
  temp_dir <- tempdir()
  unzip_dir <- file.path(temp_dir, "docx_unzipped")
  unlink(unzip_dir, recursive = TRUE)
  unzip(input_file, exdir = unzip_dir)
  
  doc_xml_path <- file.path(unzip_dir, "word", "document.xml")
  doc_xml <- paste(readLines(doc_xml_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  
  # NORMALIZE:  Remove XML tags between bracket characters to find markers
  # Extract just the text content to find markers, then work backwards
  text_only <- gsub("<[^>]+>", "", doc_xml)
  
  # Check if markers exist in text
  if (grepl("\\[\\[ADD BADGE\\]\\]", text_only)) {
    message("Badge markers found in normalized text!")
    
    # Use a regex that matches [[ADD BADGE]] even when split across XML tags
    # Match:  [ possibly tags ][ possibly tags ]ADD possibly tags BADGE possibly tags ] possibly tags ]
    badge_pattern <- "\\[(<[^>]*>)*\\[(<[^>]*>)*A(<[^>]*>)*D(<[^>]*>)*D(<[^>]*>)* (<[^>]*>)*B(<[^>]*>)*A(<[^>]*>)*D(<[^>]*>)*G(<[^>]*>)*E(<[^>]*>)*\\](<[^>]*>)*\\]"
    
    # Simpler approach: find </w:t> patterns and consolidate
    # Replace the fragmented marker with a placeholder, then replace placeholder with badge
    
    # This pattern catches common fragmentations
    doc_xml <- gsub("\\[\\[ADD BADGE\\]\\]", "{{BADGE_PLACEHOLDER}}", doc_xml)
    doc_xml <- gsub("\\[(<[^>]*>)*\\[(<[^>]*>)*ADD BADGE(<[^>]*>)*\\](<[^>]*>)*\\]", "{{BADGE_PLACEHOLDER}}", doc_xml)
    
  }
  
  writeLines(doc_xml, doc_xml_path, useBytes = TRUE)
  
  if (length(new_rels) > 0) {
    rels_xml <- gsub("</Relationships>", 
                     paste(paste(new_rels, collapse = "\n"), "\n</Relationships>", sep = ""),
                     rels_xml)
    writeLines(rels_xml, rels_path, useBytes = TRUE)
  }
  
  message("")
  message("Creating output document...")
  
  # Create output file with absolute path
  output_file_abs <- normalizePath(output_file, mustWork = FALSE)
  
  old_wd <- getwd()
  setwd(unzip_dir)
  
  if (file.exists(output_file_abs)) {
    file.remove(output_file_abs)
  }
  
  zip(output_file_abs, files = list.files(".", recursive = TRUE, all.files = TRUE), 
      flags = "-r9Xq")
  setwd(old_wd)
  
  message(paste("✅ Output saved to:", output_file_abs))
  message("========================================")
}

# ============== MAIN ==============
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  message("Usage: Rscript process_cv.R <input.docx> <output.docx>")
  quit(status = 1)
}

input_file <- args[1]
output_file <- args[2]

if (!file.exists(input_file)) {
  stop(paste("Input file not found:", input_file))
}

process_cv_with_badges(input_file, output_file)