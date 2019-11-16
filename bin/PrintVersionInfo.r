
#' Print the version information
#'
#' @param ver Version string.
#' @param datestr Release date string
#'
#' @return
#' @export
#'
#' @examples
printVersionInfo <-function(ver, datestr)
{
	message("A tool for calculating daily mean LST from MODIS observations")
	message("Version: ",ver)
	message("Authors: Zhuotong Nan (giscn@msn.com); ", "Xue Hui; ", "Yang Ben")
	message("https://permalab.nanzt.info")

	message(datestr)
#	message("\n")
}
