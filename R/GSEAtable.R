#' @title Creates a table from GSEA reports
#' @description This function creates a single table with all the GSEA results
#' present in a directory.
#' @import tidyverse
#' @name GSEAtable
#' @param dir Directory where the GSEA results can be found.
#' @return A table with the results for all GSEA analyses.
#' @examples
#' @export

GSEAtable <- function(dir) {
  # All GSEA output reports in dir.
  files <- list.files(dir, recursive = TRUE,
                      pattern = "^gsea_report_for_*.*[^html]$")
  # Read all reports and add two columns with the name of the comparison and the
  # signature used.
  tables <- lapply(files, function(x) {
    comp_dir <- dirname(x)
    signature <- gsub(pattern = ".Gsea.*$", replacement = "" ,
                      tail(unlist(strsplit(comp_dir, "_")), n = 1))
    comparison <- unlist(strsplit(comp_dir, split = paste0("_", signature)))[1]
    t <- read.table(paste0(dir, "/", files[1]), sep = "\t", header = TRUE) %>%
      select(-X) %>% mutate(COMPARISON = comparison, SIGNATURE = signature)
  })
  # Bind all reports, reorder the columns and convert some columns to factors
  # and other to numeric.
  gsea_table <- do.call("rbind", tables) %>%
    relocate(NAME:LEADING.EDGE, .after = last_col()) %>%
    mutate(across(COMPARISON:GS.br..follow.link.to.MSigDB, as.factor),
           across(SIZE:RANK.AT.MAX, as.numeric)) %>%
    relocate(NAME:LEADING.EDGE, .after = last_col())
  return(gsea_table)
}
