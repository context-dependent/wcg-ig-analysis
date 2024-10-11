z_path <- function(...) {
  file.path("Z:/WCG Inclusion Group Research", ...)
}

find_cache_file <- function(obj_id, date = "latest", extension = ".rds") {
  cache_dir <- z_path("surveys", "cache", obj_id)

  if(!fs::dir_exists(cache_dir)) {
    return(NULL)
  }

  cache_files <- cache_dir |> 
    fs::dir_ls() |> 
    sort()

  if (length(cache_files) == 0) {
    return(NULL)
  }
  if (date != "latest") {
    cache_files <- grep(strftime(lubridate::ymd(date), formate = "%Y%m%d"), cache_files, value = TRUE)
  }
  return(dplyr::last(cache_files))
} 

new_cache_file <- function(obj_id, extension = ".rds") {
  obj_dir <- z_path("surveys", "cache", obj_id)
  if(!fs::dir_exists(obj_dir)) {
    fs::dir_create(obj_dir, recurse = TRUE)
  }
  
  file.path(
    obj_dir, 
    glue::glue("{strftime(lubridate::now(), format = '%Y%m%d%H%M%S')}{extension}")
  )
}

dl_button <- function(x, name) {
  x |> 
    downloadthis::download_this(
      output_name = glue::glue("sk-overview_{name}"), 
      output_extension = ".csv", 
      button_label = "Download data as CSV", 
      button_type = "default", 
      has_icon = TRUE, 
      icon = "fa fa-save"
    )
}
