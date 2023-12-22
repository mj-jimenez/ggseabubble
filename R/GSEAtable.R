#' @title Creates a table from GSEA reports
#' @description This function creates a single table with all the GSEA results
#' present in a directory.
#' @import tidyverse
#' @name GSEAtable
#' @param dir Directory where the GSEA results can be found.
#' @return A dataframe with the results of all GSEA analyses.
#' @examples
#' @export

GSEAtable <- function(dir) {
  # --- Checks ---
  # Check dir.
  if (length(dir) != 1 | !is.character(dir[1])) {
    stop('dir must be a single string.')
  }
  is.file <- file.exists(dir)
  if (is.file & !dir.exists(dir)) stop('dir must be a directory.')
  else if (!is.file) stop(paste0(dir, ' does not exist.'))
  files <- list.files(dir, recursive = TRUE,
                      pattern = "^gsea_report_for_*.*[^html]$")
  is.report <- length(files > 0)
  if (!is.report) stop(paste0(dir, ' does not contain GSEA reports.'))
  # --- Code ---
  # Read all reports and add two columns with the name of the comparison and the
  # signature used.
  tables <- lapply(files, function(x) {
    comp.dir <- basename(dirname(x))
    signature <- gsub(pattern = ".Gsea.*$", replacement = "" ,
                      tail(unlist(strsplit(comp.dir, "_")), n = 1))
    comparison <- unlist(strsplit(comp.dir, split = paste0("_", signature)))[1]
    t <- read.table(paste0(dir, "/", x), sep = "\t", header = TRUE) %>%
      dplyr::select(-X) %>%
      dplyr::mutate(COMPARISON = comparison, SIGNATURE = signature)
  })
  # Bind all reports, reorder the columns and convert some columns to factors
  # and other to numeric.
  gsea.table <- do.call("rbind", tables) %>%
    dplyr::relocate(NAME:LEADING.EDGE, .after = tidyselect::last_col()) %>%
    dplyr::mutate(dplyr::across(COMPARISON:GS.br..follow.link.to.MSigDB, as.factor),
                  dplyr::across(SIZE:RANK.AT.MAX, as.numeric)) %>%
    dplyr::relocate(NAME:LEADING.EDGE, .after = tidyselect::last_col())
  return(gsea.table)
}
