save_survey <- function(qx_survey_id) {
  d <- bpqx::fetch_responses(qx_survey_id)
  cache_file <- new_cache_file(qx_survey_id)
  readr::write_rds(d, cache_file)
}

load_survey <- function(qx_survey_id, fetch = FALSE, date = "latest") {
  cache_file <- find_cache_file(qx_survey_id, date)
  
  if (is.null(cache_file) || fetch) {
    save_survey(qx_survey_id)
    return(readr::read_rds(find_cache_file(qx_survey_id, date = "latest")))
  } else {
    return(readr::read_rds(cache_file))
  }
}

load_qx_surveys <- function(fetch = FALSE, date = "latest") {
  survey_info <- readr::read_rds(here::here("data/survey_info.rds"))
  survey_info |> 
    mutate(data = purrr::map(id, ~load_survey(.x, fetch = fetch, date = date))) 
}

load_panel_data <- function() {
  haven::read_sav(z_path("surveys", "panel", "lw31077_001a_18-OCT-24.sav"))
}
