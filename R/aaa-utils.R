.paste_if_defined <- function(x, ...) {
  if (length(x)) {
    return(paste(x, ...))
  }
  return(NULL)
}
