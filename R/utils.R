

#' inner function of 'compute_elapsed_time'
#'
#' @param secs a numeric value specifying the seconds
#' @param estimated a boolean. If TRUE then the output label becomes the 'Estimated time'
#' @return a character string showing the estimated or elapsed time
#'
#' @keywords internal

inner_elapsed_time = function(secs, estimated = FALSE) {
  tmp_hours = as.integer((secs / 60) / 60)
  tmp_hours_minutes = (secs / 60) %% 60
  tmp_seconds = secs %% 60
  est_verb = ifelse(estimated, "Estimated time: ", "Elapsed time: ")
  res_out = paste(c(est_verb, tmp_hours, " hours and ", as.integer(tmp_hours_minutes), " minutes and ", as.integer(tmp_seconds), " seconds."), collapse = "")
  return(res_out)
}


#' elapsed time in hours & minutes & seconds
#'
#' @param time_start a numeric value specifying the start time
#' @return It does not return a value but only prints the time in form of a character string in the R session
#'
#' @keywords internal

compute_elapsed_time = function(time_start) {
  t_end = proc.time()
  time_total = as.numeric((t_end - time_start)['elapsed'])
  time_ = inner_elapsed_time(time_total)
  cat(time_, "\n")
}


#' Download the elevation .tif files that intersect with an input sf-AOI
#'
#' @param sf_or_file either an 'sf' or a .geojson file specifying the AOI (Area of Interest) for which the DEM files should be downloaded
#' @param dir_save_tifs a valid path to a directory where the .tif files should be saved
#' @param resolution an integer value specifying the elevation resolution. The Copernicus DEM data currently includes 90 meter and 30 meter resolution DEM's
#' @param crs_value an integer value specifying the CRS value of the DEM's which is by default 4326
#' @param threads an integer value specifying the number of threads to use mainly to read the .csv files from the 'inst' directory
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return a list object of length 2
#'
#' @importFrom glue glue
#' @importFrom sf st_read st_intersects
#' @importFrom utils flush.console
#'
#'
#' @references
#'
#' https://registry.opendata.aws/copernicus-dem/
#'
#' https://copernicus-dem-30m.s3.amazonaws.com/readme.html
#'
#' @export
#'
#' @details
#'
#' Based on a sample of images that I downloaded for the 90 meter resolution each file is approx. 5 MB which means in total I'll have to download 130 GB of
#' data (for more than 20.000 files of all land areas worldwide), therefore download the data based on intersection of the input AOI data and the existing tile-grid of the DEM
#'
#' The 30 meter resolution .tif images are bigger in size but visually better (1.7 MB compared to 13 MB - the time to download is approx. 20 sec. compared to 1 min. and 10 sec. for a sample use case)
#'
#' @examples
#'
#' \dontrun{
#'
#' #....................................
#' # create a directory to save the .tif
#' # files and a WKT of a samplel AOI
#' #....................................
#'
#' DIR_SAVE = file.path(Sys.getenv('HOME'), 'DIR_SAVE_DEM')
#' if (!dir.exists(DIR_SAVE)) dir.create(DIR_SAVE)
#'
#' WKT='POLYGON((61.5234 27.0591, 63.6328 27.0591, 63.6328 28.1495, 61.5234 28.1495, 61.5234 27.0591))'
#'
#' sf_obj = sf::st_as_sfc(WKT, crs = 4326)
#' sf_obj = sf::st_make_valid(sf_obj)
#'
#' #.............
#' # 90 meter DEM
#' #.............
#'
#' save_matches = CopernicusDEM::aoi_geom_save_tif_matches(sf_or_file = sf_obj,
#'                                                         dir_save_tifs = DIR_SAVE,
#'                                                         resolution = 90,
#'                                                         crs_value = 4326,
#'                                                         threads = parallel::detectCores(),
#'                                                         verbose = TRUE)
#'
#' #.............
#' # 30 meter DEM
#' #.............
#'
#' save_matches = CopernicusDEM::aoi_geom_save_tif_matches(sf_or_file = sf_obj,
#'                                                         dir_save_tifs = DIR_SAVE,
#'                                                         resolution = 30,
#'                                                         crs_value = 4326,
#'                                                         threads = parallel::detectCores(),
#'                                                         verbose = TRUE)
#' }


aoi_geom_save_tif_matches = function(sf_or_file,
                                     dir_save_tifs,
                                     resolution = 90,
                                     crs_value = 4326,
                                     threads = parallel::detectCores(),
                                     verbose = FALSE) {

  if (verbose) t_start = proc.time()

  default_URL = glue::glue('s3://copernicus-dem-{resolution}m')

  relevant_ext = c('_csv.rds', '_geojson.rds')
  relevant_files = as.character(glue::glue("dem{resolution}mGrid{relevant_ext}"))

  sys_files = system.file(glue::glue('{relevant_files}'), package = 'CopernicusDEM')

  # dtbl_csv = data.table::fread(sys_files[1], stringsAsFactors = F, header = T, nThread = threads)
  # sf_shp_gj = sf::st_read(sys_files[2], crs = crs_value, quiet = !verbose)

  dtbl_csv = readRDS(sys_files[1])
  sf_shp_gj = readRDS(sys_files[2])

  if (!inherits(sf_or_file, c('sf', 'sfc'))) {
    if (file.exists(sf_or_file)) {
      sf_or_file = sf::st_read(sf_or_file, crs = crs_value, quiet = !verbose)
    }
    else {
      stop("The input parameter 'sf_or_file' is neither an 'sf' object nor an '.shp' or '.geojson' file!", call. = F)
    }
  }

  inters_geom = suppressMessages(sf::st_intersects(sf_or_file, sf_shp_gj))
  inters_geom = data.frame(inters_geom, stringsAsFactors = F)
  inters_geom = inters_geom$col.id
  if (length(inters_geom) == 0) stop(glue::glue("The input 'sf_or_file' parameter does not intersect with any of the  '{relevant_files[2]}'  DEM files!"), call. = F)

  keep_dems = as.character(sf_shp_gj[inters_geom, ]$id)

  idx_dtbl = which(dtbl_csv$dem_dirs %in% keep_dems)

  if (length(idx_dtbl) == 0) stop(glue::glue("There are no matches between extraced DEM of the AOI ( '{paste(keep_dems, collapse = ', ')}' ) and the grid-files saved in '{sys_files[1]}'!"), call. = F)

  subs_aoi = dtbl_csv[idx_dtbl, ]

  tifs_aoi = file.path(default_URL, subs_aoi$dem_dirs, subs_aoi$dem_tifs)
  LEN = length(tifs_aoi)

  if (threads > 1 & LEN > 1) {
    if (verbose) cat(glue::glue("Parallel download of the  {LEN}  .tif files using  {threads}  threads starts ..."), '\n')
    par_downl = parallel::mclapply(1:LEN, function(FILE) {
      save_file_iter = system2(command = 'aws', args = c( 's3', 'cp', tifs_aoi[FILE], file.path(dir_save_tifs, basename(tifs_aoi[FILE]))), stdout = T, stderr = T)
    }, mc.cores = threads)
  }
  else {
    if (verbose) cat("The single .tif file will be downloaded ...\n")
    for (FILE in 1:LEN) {

      if (verbose) {
        message("File: ", FILE, "/", LEN, " will be downloaded ...\r", appendLF = FALSE)
        utils::flush.console()
      }

      save_file_iter = system2(command = 'aws', args = c( 's3', 'cp', tifs_aoi[FILE], file.path(dir_save_tifs, basename(tifs_aoi[FILE]))), stdout = T, stderr = T)
    }
  }

  if (verbose) compute_elapsed_time(t_start)

  return(list(csv_aoi = subs_aoi, tif_aoi = tifs_aoi))
}



#' Create a VRT file from .tif files
#'
#' @param dir_tifs a valid path to a directory where the .tif files are saved
#' @param output_path_VRT a valid path to a file where the VRT will be saved
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return it doesn't return an object but it saves the output to a file
#'
#' @importFrom glue glue
#' @importFrom gdalUtils gdalbuildvrt
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #....................................
#' # create a directory to save the .tif
#' # files and a WKT of a samplel AOI
#' #....................................
#'
#' DIR_SAVE = file.path(Sys.getenv('HOME'), 'DIR_SAVE_DEM')
#' if (!dir.exists(DIR_SAVE)) dir.create(DIR_SAVE)
#'
#' WKT='POLYGON((61.5234 27.0591, 63.6328 27.0591, 63.6328 28.1495, 61.5234 28.1495, 61.5234 27.0591))'
#'
#' sf_obj = sf::st_as_sfc(WKT, crs = 4326)
#' sf_obj = sf::st_make_valid(sf_obj)
#'
#' #......................
#' # download 90 meter DEM
#' #......................
#'
#' save_matches = CopernicusDEM::aoi_geom_save_tif_matches(sf_or_file = sf_obj,
#'                                                         dir_save_tifs = DIR_SAVE,
#'                                                         resolution = 90,
#'                                                         crs_value = 4326,
#'                                                         threads = parallel::detectCores(),
#'                                                         verbose = TRUE)
#'
#' #............................
#' # create a .VRT file from the
#' # 90m downloaded .tif files
#' #............................
#'
#' VRT_out = as.character(glue::glue("{DIR_SAVE}.vrt"))
#'
#' res_vrt = CopernicusDEM::create_VRT_from_dir(dir_tifs = DIR_SAVE,
#'                                              output_path_VRT = VRT_out,
#'                                              verbose = TRUE)
#'
#' #.................................................
#' # load the saved .vrt file as raster (which might
#' # consist of multiple files - mosaic) and plot it
#' #.................................................
#'
#' rst = raster::raster(VRT_out)
#' sp::plot(rst)
#'
#' }

create_VRT_from_dir = function(dir_tifs,
                               output_path_VRT,
                               verbose = FALSE) {

  lst_vrt = list.files(dir_tifs, pattern = '.tif', full.names = T)

  if (verbose) cat(glue::glue("The VRT Mosaic will be built from  {length(lst_vrt)}   .tif files and will be saved in  '{output_path_VRT}' ..."), '\n')
  vrt_mosaic = gdalUtils::gdalbuildvrt(gdalfile = lst_vrt,
                                       output.vrt = output_path_VRT,
                                       separate = FALSE,
                                       verbose = verbose)
}

