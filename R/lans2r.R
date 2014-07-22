
#' Load LANS data
#' 
#' Load Look at NanoSIMS data into R for easy processing and plotting
#' 
#' For loading Look@NanoSIMS data, provide either a data frame with the analysis information or the name of an excel file with a table of analysis information (must be in the first sheet). Either one must have at least one column names 'folder' which points to the LANS folder relative to base_dir. All other information is completely up to you and will be included in all data frames imported from LANS so it is easy to process the data.
#' 
#' @param ions vector of ions to load, has to match the ion data .mat file names (e.g. c('12C', '13C', '14N12C', ...))
#' @param info data frame with analysis information
#' @param info_file name of the information .xlsx file (path relative to base_dir)
#' @param base_dir the base directory, all folder and file paths must be relative to this folder, set to the working directory by default
#' @param load_zstack whether to load single plane information (*-z.dat files need to be exported in the dat directory)
#' @param load_ion_images whether to load the ion image data (more precisely whether to store them since they are loaded anyways to calculate ROI counts), makes the returned object smaller not to load them
lans2r <- function(ions,
                   info = data.frame(), info_file = NULL, base_dir = getwd(),
                   load_zstack = FALSE, load_ion_images = TRUE
                   ) {
  
  if (nrow(info) > 0 && !is.null(info_file))
    stop("Both an info data frame and info file name are supplied. Don't know which one to use.")
  else if (!is.null(info_file)) {
    if (!grepl(".xlsx$", info_file))
      stop("Sorry, only excel files (ending in .xlsx) are currently supported as info files.")
    if ("xlsx" %in% rownames(installed.packages()) == FALSE)
      stop("The xslx package is not installed. Please install and then try again: install.packages('xlsx', depen=T)")
    if (!file.exists(info_file_path <- file.path(base_dir, info_file)))
      stop("The info file at '", info_file_path, "' does not exist. Please check that the file name and base_dir are correct.")
    library(xlsx)    
    info <- read.xlsx(info_file_path, sheetIndex=1, startRow=1, stringsAsFactors = FALSE, header=TRUE, check.names=FALSE)  
  }
  
  if (!("folder" %in% names(info)))
    stop("The analysis info does not contain a 'folder' column. Don't know where to find the LANS data.")
  
  # to do
  # - load the first .mat ion file, get xyscale, noplanes and maybe vscale
  # - get the CELLS field to get ROIs (see experimenting_wirh_r_matlab for details on a nice plot), normalize pixesl to xyscale
  # - get the IM field to get the whole images (also offer some nice plotting options)
  # - calculate ROI counts from .mat files using all the CELLS (should be easy with a creative merge and some ddply magic)
  # - if set to TRUE, load -z.dat zstack data from dat folder and transform to the same shape as the other data frames
  # - store ion_map, rois (planes = noplanes), rois_planes, cells in a list structure with class = "nanosims", see below
  # - define a bunch of specific methods to run calculations on this object and some plotting
  # --> e.g., plot rois, plot images (rescale all ion images?), calculate_fractional_abundances (see other one)
  
#   # STRUCTURE EXAMPLE (this way can define 'nanosims' specific methods)
#   p <- structure(list(
#     data = data,
#     layers = list(),
#     scales = Scales$new(),
#     mapping = mapping,
#     theme = list(),
#     coordinates = coord_cartesian(),
#     facet = facet_null(),
#     plot_env = environment
#   ), class = c("nanosims", "lans2r"))
  info
}



d<-lans2r(c('12C', '13C', '14N12C'),
          info_file = "20140525_nanosims_runlist.xlsx", base_dir = "/Users/sk/Dropbox/Science/Projects/PhD/manuscripts/ms_slow_growth_in_cf/data")
