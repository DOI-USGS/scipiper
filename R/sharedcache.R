#' Guidelines for sharing files in a team data analysis
#'
#' For these purposes, we refer to files stored in a shared location as existing
#' on a 'shared cache'. Although the term 'cache' highlights the changeable
#' nature of the files in this location, shared files could include raw data,
#' munged intermediate data products, model outputs, or even final reports. They
#' could be stored on Amazon S3, on a shared network drive, in a project
#' directory on a computing cluster, or on a local computer to which all team
#' members have file access.
#'
#' @section Cache sharing strategy:
#'
#'   Every file (cached and uncached) is assigned a corresponding lightweight
#'   indicator file of the same name but with the additional extension '.st'.
#'
#'   remake.yml targets should declare dependencies on the indicator files
#'   rather than among the data files. The data files can be included as targets
#'   in the remake but should only depend on their own indicator files, and the
#'   target command should not generate or update the data file from other data
#'   files; instead, it should simply pull an existing cached version of the
#'   file into local storage.
#'
#'   Behind the scenes, `remake` creates an additional build status file for
#'   each target that gets made. Most of these stay local, but calling
#'   [scmake()] instead of [remake::make()] will create git-committable versions
#'   of certain build status files (exactly those corresponding to indicator
#'   files) in the build/status subdirectory of your project.
#'
#'   Always git commit the indicator files and the build/status files, but not
#'   the data files.
#'
#'   Your collaborators will generally be unaware of any changes to the cache
#'   until you commit and they pull a changed indicator file. The only exception
#'   is when they need to download a file from the cache (with need based on the
#'   files already on their computers and the targets they're trying to build) -
#'   if you've changed what's in the cache, they'll get the new version even if
#'   you haven't yet announced the change via git. In the probably-rare cases
#'   where this inconsistency will create problems, it's a good idea to
#'   communicate with teammates by other channels (email, talking, etc.) while
#'   the cache-indicator mismatch persists.
#'
#'   As a team member, it's your responsibility to git pull and push as often as
#'   is practical, especially when you might be obtaining or altering cached
#'   files. An up-to-date shared understanding of what's in the cache will
#'   reduce the amount of redundant processing that's required on everybody's
#'   computers.
#'
#' @md
#' @name sharedcache
NULL
