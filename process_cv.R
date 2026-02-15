#!/usr/bin/env Rscript
# =============================================================================
# CV CITATION BADGE GENERATOR — FINAL VERSION (AUTO-CLEANUP)
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(xml2)
  library(tools) 
})

# ======================== CONFIGURATION ========================
CV_SOURCE_PATH <- "/Users/seungjukim/Library/CloudStorage/GoogleDrive-seungju7@illinois.edu/My Drive/PERSONAL/CV/SK_cv.docx"
SCHOLAR_ID <- "JWNJV9UAAAAJ"
OUTPUT_FOLDER <- "_site/cv"
# ===============================================================

# =============================================================================
# 1. API HELPERS
# =============================================================================

get_total_citations <- function(scholar_id) {
  message("📊 Fetching total citations (SerpApi)")
  tryCatch({
    api_key <- Sys.getenv("SERPAPI_KEY")
    if (api_key == "") stop("SERPAPI_KEY not found in environment variables")
    
    res <- GET("https://serpapi.com/search.json", query = list(
      engine = "google_scholar_author",
      author_id = scholar_id,
      api_key = api_key
    ))
    
    if (status_code(res) == 200) {
      data <- fromJSON(content(res, "text", encoding = "UTF-8"))
      
      # Structure: cited_by$table is a data frame where $citations is 
      # itself a nested data frame with $all and $since_2021 columns.
      # Row 1 = citations, Row 2 = h_index, Row 3 = i10_index
      # We need: data$cited_by$table$citations$all[1]
      total <- data$cited_by$table$citations$all[1]
      
      # Debug output
      message("    🔍 Raw total value: ", total)
      message("    🔍 Class: ", class(total))
      
      if (!is.null(total) && !is.na(total)) {
        total <- as.integer(total)
        message("    ✅ Total citations: ", total)
        return(total)
      } else {
        message("    ⚠️ Could not extract total from response")
        return(NA)
      }
    } else {
      message("    ⚠️ API Status: ", status_code(res))
      return(NA)
    }
  }, error = function(e) {
    message("    ❌ Error: ", e$message)
    NA
  })
}

get_citations_dimensions <- function(doi) {
  message("    🔎 DOI: ", doi)
  tryCatch({
    url <- paste0("https://metrics-api.dimensions.ai/doi/", doi)
    res <- GET(url, add_headers(Accept = "application/json"), timeout(10))
    if (status_code(res) == 200) {
      cites <- fromJSON(content(res, "text", encoding = "UTF-8"))$times_cited
      message("       ✅ Citations: ", cites)
      cites
    } else {
      message("       ⚠️  API Status: ", status_code(res))
      NA
    }
  }, error = function(e) {
    message("       ❌ Error: ", e$message)
    NA
  })
}

extract_doi <- function(text) {
  m <- unlist(regmatches(text, gregexpr("10\\.\\d{4,}/[^\\s<>\\[\\]\"']+", text, perl = TRUE)))
  if (!length(m)) return(NA)
  gsub("[.,;:]+$", "", m[1])
}

get_dimensions_url <- function(doi) {
  paste0("https://badge.dimensions.ai/details/doi/", doi)
}

# =============================================================================
# 2. STAGE 1: GENERATE DOCX (FIXED PATHING & FORMATTING)
# =============================================================================

create_badged_docx <- function(input_file, output_docx_path) {
  message("\n📘 STAGE 1: Generating Badged DOCX")
  
  # --- 1. RESOLVE ABSOLUTE PATHS BEFORE UNZIPPING ---
  out_dir <- dirname(output_docx_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  abs_output_path <- file.path(normalizePath(out_dir), basename(output_docx_path))
  
  # --- 2. UNZIP ---
  tmp <- tempdir()
  unzip_dir <- file.path(tmp, "docx_unzipped")
  if (dir.exists(unzip_dir)) unlink(unzip_dir, recursive = TRUE)
  dir.create(unzip_dir)
  unzip(input_file, exdir = unzip_dir, overwrite = TRUE)
  
  doc_path  <- file.path(unzip_dir, "word", "document.xml")
  rels_path <- file.path(unzip_dir, "word", "_rels", "document.xml.rels")
  
  doc_xml  <- read_xml(doc_path)
  rels_xml <- read_xml(rels_path)
  ns <- xml_ns(doc_xml) 
  
  # --- 3. HANDLE [[XX]] ---
  xx_nodes <- xml_find_all(doc_xml, "//w:t[contains(text(), '[[XX]]')]", ns)
  if (length(xx_nodes) > 0) {
    total <- get_total_citations(SCHOLAR_ID) 
    if (!is.na(total)) {
      for (node in xx_nodes) {
        xml_text(node) <- gsub("\\[\\[XX\\]\\]", as.character(total), xml_text(node))
      }
      message("   ✅ Updated [[XX]] -> ", total)
    } else {
      message("   ⚠️ Skipping [[XX]] replacement — could not fetch total citations")
    }
  }
  
  # --- 4. HANDLE BADGES ---
  badge_nodes <- xml_find_all(doc_xml, "//w:t[contains(text(), '[[ADD BADGE]]')]", ns)
  count <- length(badge_nodes)
  
  if (count > 0) {
    next_id <- 20000 
    for (i in seq_along(badge_nodes)) {
      t_node <- badge_nodes[[i]]
      run_node <- xml_parent(t_node)
      p_node   <- xml_parent(run_node)
      doi <- extract_doi(xml_text(p_node)) 
      
      xml_text(t_node) <- gsub("\\[\\[ADD BADGE\\]\\]", "", xml_text(t_node))
      
      if (!is.na(doi)) {
        cites <- get_citations_dimensions(doi) 
        
        # Only add badge if citations > 0
        if (!is.na(cites) && cites > 0) {
          rid <- paste0("rId", next_id)
          next_id <- next_id + 1
          
          # A. Relationship
          xml_add_child(rels_xml, "Relationship",
                        Id = rid,
                        Type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink",
                        Target = get_dimensions_url(doi),
                        TargetMode = "External"
          )
          
          # B. Badge Visuals
          cite_txt <- as.character(cites)
          badge_str <- paste0(
            '<w:hyperlink xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
            'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" r:id="', rid, '">',
            '<w:r>',
            '<w:rPr>',
            '<w:color w:val="0563C1"/>', # Blue
            '<w:u w:val="single"/>',      # Underline
            '</w:rPr>',
            '<w:t xml:space="preserve"> (Citations = ', cite_txt, ')</w:t>',
            '</w:r>',
            '</w:hyperlink>'
          )
          xml_add_sibling(run_node, read_xml(badge_str), .where = "after")
          message(sprintf("   ✅ Badge %d/%d added (rId: %s, cites: %s)", i, count, rid, cite_txt))
        } else {
          message(sprintf("   ⏭️  Badge %d/%d skipped (0 or NA citations)", i, count))
        }
      }
    }
  }
  
  # --- 5. SAVE & ZIP ---
  write_xml(doc_xml, doc_path)
  write_xml(rels_xml, rels_path)
  
  old_wd <- getwd()
  setwd(unzip_dir) 
  
  all_files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if ("[Content_Types].xml" %in% all_files) {
    files_ordered <- c("[Content_Types].xml", setdiff(all_files, "[Content_Types].xml"))
  } else {
    files_ordered <- all_files
  }
  
  if (file.exists(abs_output_path)) unlink(abs_output_path)
  
  zip(abs_output_path, files_ordered, flags = "-r9Xq")
  
  setwd(old_wd)
  
  if (file.exists(abs_output_path)) {
    message("   💾 DOCX Saved (Intermediate): ", abs_output_path)
  } else {
    stop("❌ Error: DOCX was not created at ", abs_output_path)
  }
  
  return(abs_output_path)
}

# =============================================================================
# 3. STAGE 2: CONVERT TO PDF (FIXED APPLESCRIPT)
# =============================================================================

convert_docx_to_pdf <- function(input_docx, output_pdf) {
  message("\n📕 STAGE 2: Converting to PDF")
  
  input_abs <- normalizePath(input_docx)
  
  out_dir <- dirname(output_pdf)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  output_abs <- file.path(normalizePath(out_dir), basename(output_pdf))
  
  if(file.exists(output_abs)) unlink(output_abs)
  
  scpt_path <- file.path(tempdir(), "convert_fixed.scpt")
  
  script_content <- paste0(
    'tell application "Microsoft Word"\n',
    '   activate\n',
    '   set inputPath to "', input_abs, '"\n',
    '   set outputPath to "', output_abs, '"\n',
    '   \n',
    '   open POSIX file inputPath\n',
    '   delay 2\n', 
    '   \n',
    '   set theDoc to active document\n',
    '   save as theDoc file name outputPath file format 17\n',
    '   close theDoc saving no\n',
    'end tell'
  )
  
  cat(script_content, file = scpt_path)
  
  message("   ⏳ Launching Word to convert...")
  tryCatch({
    res <- system(paste("osascript", shQuote(scpt_path)), intern = TRUE, ignore.stderr = FALSE)
    
    if (file.exists(output_abs) && file.size(output_abs) > 0) {
      message("   ✅ PDF Success: ", output_abs)
    } else {
      message("   ❌ PDF Conversion failed.")
      message("      AppleScript Log: ", paste(res, collapse=" "))
    }
  }, error = function(e) {
    message("   ❌ AppleScript Execution Error: ", e$message)
  })
}

# =============================================================================
# MAIN
# =============================================================================

update_cv <- function() {
  message("\n════════ TWO-STAGE CV GENERATOR ════════\n")
  
  # Naming convention: 
  # DOCX is temporary/intermediate (_BADGED suffix)
  # PDF is final (no suffix)
  doc_name <- paste0(file_path_sans_ext(basename(CV_SOURCE_PATH)), "_BADGED.docx")
  pdf_name <- paste0(file_path_sans_ext(basename(CV_SOURCE_PATH)), ".pdf")
  
  final_docx_path <- file.path(OUTPUT_FOLDER, doc_name)
  final_pdf_path  <- file.path(OUTPUT_FOLDER, pdf_name)
  
  # 1. Stage 1: Generate DOCX
  tryCatch({
    create_badged_docx(CV_SOURCE_PATH, final_docx_path)
  }, error = function(e) {
    stop("Stage 1 Failed: ", e$message)
  })
  
  # 2. Stage 2: Convert to PDF
  if (file.exists(final_docx_path)) {
    tryCatch({
      convert_docx_to_pdf(final_docx_path, final_pdf_path)
      
      # 3. Stage 3: Cleanup
      if (file.exists(final_pdf_path) && file.size(final_pdf_path) > 0) {
        message("\n🧹 STAGE 3: Cleanup")
        unlink(final_docx_path)
        message("   🗑️  Deleted intermediate DOCX: ", final_docx_path)
        message("   ✨ Final Output: ", final_pdf_path)
      } else {
        message("\n⚠️  PDF creation failed, keeping DOCX for debugging.")
      }
      
    }, error = function(e) {
      message("Stage 2 Failed: ", e$message)
    })
  } else {
    message("❌ Skipping Stage 2: DOCX file was not found.")
  }
  
  message("\n════════ DONE ════════")
}

update_cv()