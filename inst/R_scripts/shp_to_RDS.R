
####################################################################################
# First Download the 'grid30.zip' and 'grid90.zip' files from my Datasets repository   [ https://github.com/mlampros/DataSets ]
####################################################################################

#.............................................................   Pre-Processing code snippets to save 1st. the .csv and .geojson files and then 2nd. to convert to .RDS objects
# Copernicus DEM products of either resolution 30 or 90
# References:
#
#  - https://registry.opendata.aws/copernicus-dem/
#  - https://copernicus-dem-30m.s3.amazonaws.com/readme.html
#.............................................................


aws_s3_DEM = function(resolution = 90,
                      csv_file_save = NULL,
                      verbose = FALSE) {
  if (verbose) {
    t_start = proc.time()
    cat("The available .tif files will be recursively retrieved ...\n")
  }

  default_URL = glue::glue('s3://copernicus-dem-{resolution}m/')
  inner_res = system2(command = 'aws', args = c( 's3', 'ls', default_URL, '--recursive'), stdout = T, stderr = T)

  inner_res_spl = as.vector(unlist(lapply(strsplit(inner_res, ' '), function(x) {
    trimws(x[length(x)], which = 'both')
  })))

  ext_fls = trimws(tools::file_ext(inner_res_spl), which = 'both')
  # table(ext_fls)

  idx_tif = which(ext_fls == 'tif')
  inner_res_spl = inner_res_spl[idx_tif]

  if (verbose) cat(glue::glue("From the initial  {length(ext_fls)}  files  {length(inner_res_spl)}  files are of .tif extension and will be kept!"), '\n')

  dem_DIRS = as.vector(unlist(lapply(1:length(inner_res_spl), function(x) {
    dirname(inner_res_spl[x])
  })))

  dem_TIFFS = as.vector(unlist(lapply(1:length(inner_res_spl), function(x) {
    basename(inner_res_spl[x])
  })))

  inner_res_spl = data.table::setDT(list(dem_dirs = dem_DIRS, dem_tifs = dem_TIFFS))

  if (!is.null(csv_file_save)) {
    cat(glue::glue("The DEM of resolution {resolution}  will be saved in  '{csv_file_save}'"), '\n')
    data.table::fwrite(inner_res_spl, file = csv_file_save, row.names = F)
  }

  if (verbose) sentinel.utils::compute_elapsed_time(t_start)

  return(inner_res_spl)
}


#...................................................................
# save the DEM's of resolution 30 and 90 using the previous function
#...................................................................

dem90 = aws_s3_DEM(resolution = 90,
                   csv_file_save = '/CopernicusDEM/inst/dem90mGrid.csv',
                   verbose = TRUE)


dem30 = aws_s3_DEM(resolution = 30,
                   csv_file_save = '/CopernicusDEM/inst/dem30mGrid.csv',
                   verbose = TRUE)


#................................................
# convert the initial .shp files to .geojson to
# reduce the file size from approx. 80 MB to 4 MB
#................................................


shp_to_geojson = function(shp_file_path,
                          save_geojson_DIR,
                          crs_value = 4326,
                          verbose = FALSE) {

  dat = sf::st_read(shp_file_path, crs = crs_value, quiet = !verbose)
  sf::st_write(dat, dsn = file.path(save_geojson_DIR, gsub('.shp', '.geojson', basename(shp_file_path))),
               layer = "",
               driver = "GeoJSON",
               quiet = !verbose,
               append = TRUE)
}



#........................
# save the .geojson files   [ the 'grid30.zip' and 'grid90.zip' files include the required .shp files ]
#........................

gj90 = shp_to_geojson(shp_file_path = '/CopernicusDEM/inst/grid90/dem90mGrid.shp',
                      save_geojson_DIR = '/CopernicusDEM/inst',
                      crs_value = 4326,
                      verbose = TRUE)


gj30 = shp_to_geojson(shp_file_path = '/CopernicusDEM/inst/grid30/dem30mGrid.shp',
                      save_geojson_DIR = '/CopernicusDEM/inst',
                      crs_value = 4326,
                      verbose = TRUE)


#.................................................
# convert the .geojson and .csv files
# to .RDS to reduce the objects' size
# Reference:
#    https://stackoverflow.com/a/21370602/8302386
#.................................................

geojson_csv_to_RDS = function(dem_csv,
                              dem_geojson,
                              file_out_csv,
                              file_out_geojson,
                              compress = 'gzip') {            # 'gzip' is a good trade-off between compress size and upload time (see: https://data.nozav.org/old/posts/2016/02/compression_benchmark.html)

  dtbl_csv = data.table::fread(dem_csv, stringsAsFactors = F, header = T, nThread = parallel::detectCores())
  gj_obj = sf::st_read(dem_geojson, crs = 4326, quiet = TRUE)

  saveRDS(dtbl_csv,
          file = file_out_csv,
          compress = compress)

  saveRDS(gj_obj,
          file = file_out_geojson,
          compress = compress)
}


#.................................
# run the function for the 4 files
#.................................

res_csv_gj_30 = geojson_csv_to_RDS(dem_csv = '/CopernicusDEM/inst/dem30mGrid.csv',
                                   dem_geojson = '/CopernicusDEM/inst/dem30mGrid.geojson',
                                   file_out_csv = '/CopernicusDEM/inst/dem30mGrid_csv.rds',
                                   file_out_geojson = '/CopernicusDEM/inst/dem30mGrid_geojson.rds',
                                   compress = 'gzip')


res_csv_gj_90 = geojson_csv_to_RDS(dem_csv = '/CopernicusDEM/inst/dem90mGrid.csv',
                                   dem_geojson = '/CopernicusDEM/inst/dem90mGrid.geojson',
                                   file_out_csv = '/CopernicusDEM/inst/dem90mGrid_csv.rds',
                                   file_out_geojson = '/CopernicusDEM/inst/dem90mGrid_geojson.rds',
                                   compress = 'gzip')

