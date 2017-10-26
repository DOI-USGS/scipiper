.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    'USGS Support Package: 
    https://owi.usgs.gov/R/packages.html#support'),
    collapse='\n'))
}

.onLoad <- function(libname, pkgname) {
  
  # set defaults. code inspired by github.com/jennybc/googlesheets
  op <- options()
  op.scipiper <- list(
    scipiper.remake_file = 'remake.yml'
  )
  toset <- !(names(op.scipiper) %in% names(op))
  if(any(toset)) options(op.scipiper[toset])
  
}
