#' @export
#'
get_html <- function(file_name)
{
  fpath <- system.file("extdata", file_name, package="ImpectServer")

  x <- readChar(fpath, file.info(fpath)$size)

  x
}
