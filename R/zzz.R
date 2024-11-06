z_path <- function(...) {
  file.path("Z:/WCG Inclusion Group Research", ...)
}

find_cache_file <- function(obj_id, date = "latest", extension = ".rds") {
  cache_dir <- z_path("surveys", "cache", obj_id)

  if (!fs::dir_exists(cache_dir)) {
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
  if (!fs::dir_exists(obj_dir)) {
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
      output_name = name,
      output_extension = ".csv",
      button_label = "Download data as CSV",
      button_type = "default",
      has_icon = TRUE,
      icon = "fa fa-save"
    )
}

st_ig_tab <- function(
    d, l,
    var_pattern, val_transform = ~.x,
    agg_fun = mean, svy = c("staff", "stakeholder"),
    color_code = TRUE,
    title = NULL, subtitle = NULL,
    palette = "magma",
    browse = FALSE) {
  if (browse) browser()

  d_ <- d |>
    tidyr::unnest(ig_loop) |>
    dplyr::filter(ig_served) |>
    dplyr::select(response_id, ig_short, matches(var_pattern)) |>
    tidyr::pivot_longer(
      cols = matches(var_pattern),
      names_to = "var",
      names_transform = list(
        var = function(x) {
          l$ig_insights[x] |>
            stringr::str_extract("(?<=ers - ).*?$") |>
            stringr::str_replace("\\[Field-1\\]", "[X]")
        }
      ),
      values_to = "val",
      values_transform = list(
        val = val_transform
      )
    ) |>
    dplyr::filter(!is.na(val))

  rows <- NULL
  tag <- "full"

  if (is.logical(d_$val) || is.numeric(d_$val)) {
    d__ <- d_ |>
      dplyr::group_by(
        ig_short,
        var
      ) |>
      dplyr::summarize(
        x = agg_fun(val),
        .groups = "drop_last"
      )

    d___ <- d__ |>
      tidyr::pivot_wider(names_from = ig_short, values_from = x)

    tag <- "agg"
  } else {
    d__ <- d_ |>
      dplyr::group_by(
        ig_short,
        var,
        val
      ) |>
      dplyr::summarize(
        n = dplyr::n(),
        .groups = "drop_last"
      ) |>
      dplyr::mutate(
        x = n / sum(n)
      )

    d___ <- d__ |>
      dplyr::select(-n) |>
      tidyr::pivot_wider(names_from = val, values_from = x)

    rows <- "ig_short"
  }

  dom <- range(d__[["x"]])

  t <- d___ |>
    dplyr::group_by(var) |>
    gt::gt(rowname_col = rows) |>
    gtExtras::gt_theme_538() |>
    gt::tab_source_note(
      dl_button(
        name = glue::glue("xp-{svy[1]}-{var_pattern}-{tag}"),
        x = d___
      )
    ) |>
    gt::fmt_missing() |>
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(weight = "bold")
      )
    )

  if (dom[1] >= 0 && dom[2] <= 1) {
    t <- t |> gt::fmt_percent(
      columns = c(-var),
      decimals = 0
    )
  } else {
    t <- t |> gt::fmt_number(
      columns = c(-var),
      decimals = 0,
      force_sign = TRUE
    )
  }

  if (!is.null(title)) {
    t <- t |> gt::tab_header(title = title, subtitle = subtitle)
  }

  if (color_code) {
    t <- t |>
      gt::data_color(
        columns = c(-var),
        palette = palette,
        domain = dom
      )
  }
  t
}
