#' Lexer for a makefile
#' 
#' @param makefile a string containing the relative path of the makefile
#'
#' @return A character vector of the tokens of the makefile.
#' @export
#' 
#' @examples
#' ## NOT RUN
#' ## tokens <- lexer("path/to/makefile")
lexer <- function(makefile) {  
    read <- readLines(makefile)
    no_empty_lines <- read[nzchar(read)]
    no_ws <- trimws(no_empty_lines)
    no_comments <- no_ws[!grepl("^#", no_ws)]
    line_break <- gsub("$", " \\\n", no_comments)
    tokens <- unlist(strsplit(line_break, " "))
    return(tokens)
}
