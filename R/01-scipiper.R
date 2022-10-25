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
    scipiper.remake_file = 'remake.yml',
    scipiper.getters_file = 'getters.yml',
    scipiper.dry_put = FALSE,
    scipiper.gd_config_file = "lib/cfg/gd_config.yml",
    scipiper.s3_config_file = "lib/cfg/s3_config.yml",
    scipiper.ind_ext = 'ind',
    scipiper.use_local_aws_credentials = TRUE
  )
  toset <- !(names(op.scipiper) %in% names(op))
  if(any(toset)) options(op.scipiper[toset])
  
}

#' Scipiper session options
#'
#' Several session-level options are stored in R's `.Options` object.
#'
#' \describe{
#'
#' \item{`scipiper.remake_file`}{File path of the main remake YAML file for a
#' project. Used as the default for the `remake_file` argument in many
#' functions.}
#'
#' \item{`scipiper.dry_put`}{Logical, default FALSE. If TRUE, calls to `s3_put` or `gd_put` won't actually push anything to S3 or Google Drive; they'll just pretend they've done it.}
#'
#' \item{`scipiper.s3_config_file`}{File path to a configuration file for an
#' Amazon S3 bucket. Used as the default for the `s3_config_file` argument in
#' many functions (usually those prefixed with `s3_`).}
#'
#' \item{`scipiper.gd_config_file`}{File path to a configuration file for a
#' Google Drive folder. Used as the default for the `gd_config_file` argument in
#' many functions (usually those prefixed with `gd_`).}
#'
#' \item{`scipiper.ind_ext`}{The indicator file extension to recognize, i.e.,
#' the final file extension of files for which `is_ind_file()` should return
#' `TRUE` and which `as_ind_file` should append to the data file name. Used as
#' the default for the `ind_ext` argument in many functions.`}
#'
#' }
#'
#' @docType package
#' @name options
#' @examples
#' # see what's already there
#' getOption('scipiper.remake_file') # 'remake.yml'
#' getOption('scipiper.gd_config_file') # "lib/cfg/gd_config.yml"
#' getOption('scipiper.s3_config_file') # "lib/cfg/s3_config.yml"
#' getOption('scipiper.ind_ext') #'ind'
#' .Options[grep('scipiper', names(.Options), value=TRUE)] # list all scipiper options
#'
#' # change the options - these commands can be placed in your project's .Rprofile
#' # file if you want them to be reapplied in every R session for the project
#' options('scipiper.ind_ext'='st', 'scipiper.remake_file'='remake.yaml')
NULL

#' Scipiper package: Streamlining scientific data analysis workflows
#'
#' Scipiper provides support for:
#'
#' \itemize{
#'
#' \item Storing some project files in a shared S3 bucket or Google Drive folder
#'
#' \item Combining `remake` with a shared remote cache, e.g., shared files in an
#' S3 bucket or a Google Drive folder
#'
#' \item Planning and executing long lists (batches) of similar tasks, e.g.,
#' downloading data for all 50 states or running many models at many sites.
#' Scipiper includes fault tolerance and the ability to pick up partway through
#' a long batch run
#'
#' }
#'
#' See ?scipiper::options for global (session-level) options you can set for use
#' with scipiper.
#'
#' @docType package
#' @name scipiper
NULL
