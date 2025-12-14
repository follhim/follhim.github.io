#!/usr/bin/env Rscript
# CV Citation Badge Generator - CLI Version
# Uses Dimensions Metrics API + SerpAPI for Google Scholar

library(httr)
library(jsonlite)

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
      
      if ("cited_by" %in% names(data) && "table" %in% names(data$cited_by)) {
        # Get "Citations" -> "All" value
        citations_all <- data$cited_by$table$citations$all
        return(as.numeric(citations_all))
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
# Extract DOI from text
extract_doi <- function(text) {
  matches <- unlist(regmatches(text, gregexpr("10\\.\\d{4,}/[^\\s<>\\[\\]\"']+", text, perl = TRUE)))
  
  if (length(matches) == 0) return(NA)
  
  matches <- gsub("[.,;:]+$", "", matches)
  matches <- gsub("<.*$", "", matches)
  matches <- gsub("\\)$", "", matches)
  
  preprint_prefixes <- c("10.31234", "10.1101", "10.2139")
  
  # Fixed: handle each match individually
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
  
  message("Reading document...")
  
  temp_dir <- tempdir()
  unzip_dir <- file.path(temp_dir, "docx_unzipped")
  unlink(unzip_dir, recursive = TRUE)
  unzip(input_file, exdir = unzip_dir)
  
  doc_xml_path <- file.path(unzip_dir, "word", "document.xml")
  doc_xml <- paste(readLines(doc_xml_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  
  rels_path <- file.path(unzip_dir, "word", "_rels", "document.xml.rels")
  rels_xml <- paste(readLines(rels_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  
  # Handle total citations [[XX]]
  if (grepl("\\[\\[XX\\]\\]", doc_xml)) {
    message("Fetching total citations from Google Scholar via SerpAPI...")
    total_cites <- get_total_citations(scholar_id)
    if (!is.na(total_cites)) {
      message(paste("  Total Citations:", total_cites))
      doc_xml <- gsub("\\[\\[XX\\]\\]", as.character(total_cites), doc_xml)
    } else {
      message("  Warning: Could not fetch total citations")
      doc_xml <- gsub("\\[\\[XX\\]\\]", "--", doc_xml)
    }
  }
  
  # Handle badges [[ADD BADGE]]
  badge_matches <- gregexpr("\\[\\[ADD BADGE\\]\\]", doc_xml)[[1]]
  badge_count <- ifelse(badge_matches[1] == -1, 0, length(badge_matches))
  
  message(paste("Found", badge_count, "badge markers"))
  message("Using Dimensions Metrics API")
  message("")
  
  new_rels <- c()
  
  if (badge_count > 0) {
    parts <- strsplit(doc_xml, "\\[\\[ADD BADGE\\]\\]")[[1]]
    new_parts <- list()
    
    existing_rids <- as.numeric(gsub("rId", "", unlist(regmatches(rels_xml, gregexpr("rId\\d+", rels_xml)))))
    next_rid <- max(existing_rids, na.rm = TRUE) + 1
    
    for (i in seq_along(parts[-length(parts)])) {
      section <- parts[[i]]
      
      message(paste("Processing publication", i, "of", badge_count))
      
      doi <- extract_doi(section)
      
      if (!is.na(doi)) {
        message(paste("  DOI:", doi))
        
        citation_result <- get_citations_best(doi)
        citations <- citation_result$count
        source <- citation_result$source
        
        if (is.na(citations)) {
          message("  Citations: N/A")
        } else {
          message(paste("  Citations:", citations, paste0("(", source, ")")))
        }
        
        hyperlink_rid <- paste0("rId", next_rid)
        next_rid <- next_rid + 1
        
        dimensions_url <- get_dimensions_url(doi)
        new_rels <- c(new_rels, 
                      paste0('<Relationship Id="', hyperlink_rid, 
                             '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink" Target="', 
                             dimensions_url, '" TargetMode="External"/>'))
        
        badge_xml <- create_text_badge_xml(citations, hyperlink_rid)
        
        new_parts[[length(new_parts) + 1]] <- section
        new_parts[[length(new_parts) + 1]] <- badge_xml
        
      } else {
        message(paste("  Warning: No DOI found for entry", i))
        new_parts[[length(new_parts) + 1]] <- section
        new_parts[[length(new_parts) + 1]] <- '<w:r><w:rPr><w:color w:val="888888"/><w:sz w:val="20"/></w:rPr><w:t> [No DOI]</w:t></w:r>'
      }
      
      Sys.sleep(0.2)
    }
    
    new_parts[[length(new_parts) + 1]] <- parts[[length(parts)]]
    doc_xml <- paste(unlist(new_parts), collapse = "")
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
  
  old_wd <- getwd()
  setwd(unzip_dir)
  
  if (file.exists(output_file)) {
    file.remove(output_file)
  }
  
  zip(output_file, files = list.files(".", recursive = TRUE, all.files = TRUE), 
      flags = "-r9Xq")
  setwd(old_wd)
  
  message(paste("✅ Output saved to:", output_file))
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