# Helper function to convert Google Drive sharing links to direct download links
convert_gdrive_link <- function(link) {
  if(is.null(link) || link == "") {
    return(link)
  }
  
  # Check if it's a Google Drive sharing link
  if(grepl("drive.google.com/file/d/", link)) {
    # Extract the file ID using regex
    file_id <- gsub(".*drive\\.google\\.com/file/d/([^/]+)/.*", "\\1", link)
    
    # Create direct download link
    return(paste0("https://drive.google.com/uc?export=download&id=", file_id))
  }
  
  # Return the original link if it's not a Google Drive link
  return(link)
}

# Function to create a citation with badges and multiple optional icon links
create_citation_with_badges <- function(citation_text, doi, pdf_link = NULL, preregistration_link = NULL, 
                                        osf_link = NULL, preprint_link = NULL, open_access_link = NULL) {
  # Convert PDF link if it's a Google Drive link
  pdf_link <- convert_gdrive_link(pdf_link)
  
  # Create row HTML with the citation
  row_html <- paste0(
    '<tr>',
    '<td style="padding: 10px; width: 100%;">',
    '<div style="text-indent: -1.5em; padding-left: 1.5em;">',
    citation_text  # Use the citation text exactly as provided
  )
  
  # Add a new line for icons if any are provided
  if(!is.null(pdf_link) || !is.null(preregistration_link) || !is.null(osf_link) || 
     !is.null(preprint_link) || !is.null(open_access_link)) {
    row_html <- paste0(row_html, '<br><div style="margin-top: 8px; margin-left: 1.5em;">')
    
    # Add PDF icon if provided
    if(!is.null(pdf_link) && pdf_link != "") {
      row_html <- paste0(
        row_html, 
        '[{{< iconify ic:baseline-picture-as-pdf size=lg >}}](', pdf_link, ') '
      )
    }
    
    # Add preregistration icon if provided
    if(!is.null(preregistration_link) && preregistration_link != "") {
      row_html <- paste0(
        row_html, 
        '[{{< iconify academicons:preregistered size=lg >}}](', preregistration_link, ') '
      )
    }
    
    # Add OSF project icon if provided
    if(!is.null(osf_link) && osf_link != "") {
      row_html <- paste0(
        row_html, 
        '[{{< iconify academicons:osf-square size=lg >}}](', osf_link, ') '
      )
    }
    
    # Add PsyArXiv icon if provided
    if(!is.null(preprint_link) && preprint_link != "") {
      row_html <- paste0(
        row_html, 
        '[{{< iconify academicons:psyarxiv size=lg >}}](', preprint_link, ') '
      )
    }
    
    # Add Open Access icon if provided
    if(!is.null(open_access_link) && open_access_link != "") {
      row_html <- paste0(
        row_html, 
        '[{{< iconify academicons:open-access size=lg >}}](', open_access_link, ') '
      )
    }
    
    row_html <- paste0(row_html, '</div>')
  }
  
  # Close the citation div and cell
  row_html <- paste0(row_html, '</div></td>')
  
  # Add Dimensions badge with increased spacing
  if(!is.na(doi) && doi != "") {
    row_html <- paste0(
      row_html,
      '<td style="padding: 10px; width: 80px; text-align: center;">',  # Increased width more
      '<div style="display: flex; justify-content: center;">',
      sprintf(
        '<span class="__dimensions_badge_embed__" data-doi="%s" data-style="small_circle" data-legend="hover-right"></span>',
        doi
      ),
      '</div>',
      '</td>'
    )
  } else {
    row_html <- paste0(row_html, '<td style="width: 80px;"></td>')
  }
  
  # Add Altmetric badge with further increased spacing
  if(!is.na(doi) && doi != "") {
    row_html <- paste0(
      row_html,
      '<td style="padding: 10px; width: 80px; text-align: center;">',  # Increased width more
      '<div style="display: flex; justify-content: center; margin-left: 20px;">',  # Increased margin
      sprintf(
        '<div style="background-color: white; display: inline-block; border-radius: 50%%; width: 48px; height: 48px; display: flex; align-items: center; justify-content: center; transform: scale(0.92);"><div class="altmetric-embed" data-badge-type="donut" data-badge-popover="right" data-doi="%s"></div></div>',
        doi
      ),
      '</div>',
      '</td>'
    )
  } else {
    row_html <- paste0(row_html, '<td style="width: 80px;"></td>')
  }
  
  # Close the row
  row_html <- paste0(row_html, '</tr>')
  
  return(row_html)
}

# Function to generate a complete publications table
generate_publications_table <- function(citation_list) {
  # Setup for badges and styling
  setup_html <- paste0(
    '<script src="https://badge.dimensions.ai/badge.js" async charset="utf-8"></script>',
    '<script src="https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js" async charset="utf-8"></script>',
    '<style>',
    'table { width: 100%; border-collapse: collapse; table-layout: fixed; }',
    'td { color: inherit; overflow-wrap: break-word; vertical-align: middle; }',
    'tr { border-bottom: 1px solid rgba(255, 255, 255, 0.1); }',
    'td span.__dimensions_badge_embed__, td div.altmetric-embed { margin: 0 auto; display: block; }',
    '</style>'
  )
  
  # Generate rows for each citation
  rows <- sapply(citation_list, function(item) {
    create_citation_with_badges(
      item$citation, 
      item$doi,
      item$pdf_link,
      item$preregistration_link,
      item$osf_link,
      item$preprint_link,
      item$open_access_link
    )
  })
  
  # Combine into complete HTML
  html <- paste0(
    setup_html,
    '<table>',
    paste(rows, collapse = "\n"),
    '</table>'
  )
  
  return(html)
}