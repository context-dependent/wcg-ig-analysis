clean_client <- function(d, bot_threshold = .9) {
  dc <- d |> unpack_client(bot_threshold)

  index <- dc |> dplyr::select(response_id)
  demos <- client_demos(dc)
  lfs <- clean_lfs(dc, dist = "client")
  es <- client_es(dc)

  index |>
    dplyr::left_join(demos, by = "response_id") |>
    dplyr::left_join(lfs, by = "response_id") |>
    dplyr::left_join(es, by = "response_id")
}

unpack_client <- function(d, bot_threshold = .9) {
  d |>
    dplyr::filter(name |> stringr::str_detect("Client")) |>
    tidyr::unnest(data) |>
    dplyr::filter(
      is.na(Q_RecaptchaScore) | (Q_RecaptchaScore >= bot_threshold),
      !is.na(igs_dob_year),
      Q_BallotBoxStuffing == "",
      DistributionChannel != "preview",
      gta_screener_1 %in% c("", "Toronto, ON, Canada"),
      Progress == 100
    ) |>
    janitor::clean_names()
}

client_demos <- function(dc) {
  race_labels <- dc |>
    dplyr::select(matches("^igs_race_([0-9]$|10)")) |>
    labelled::var_label() |>
    purrr::map(~
      stringr::str_c(
        "igs_race_",
        stringr::str_extract(.x, "(?<=\\? ).*?(?=\\s\\()") |>
          snakecase::to_snake_case()
      ))

  race_renamer <- names(race_labels) |>
    as.list() |>
    purrr::set_names(race_labels)

  res <- dc |>
    dplyr::rename(
      demo_benefits_ever_ei = demo_benefits_ever_1,
      demo_benefits_ever_ow = demo_benefits_ever_2,
      demo_benefits_ever_odsp = demo_benefits_ever_3,
      demo_benefits_now_ei = demo_benefits_now_1,
      demo_benefits_now_ow = demo_benefits_now_2,
      demo_benefits_now_odsp = demo_benefits_now_3
    ) |>
    mutate(
      dplyr::across(
        matches("^demo_benefits_(ever|now)_(ei|ow|odsp)$"),
        ~ .x == "Yes"
      ),
      demo_parent = demo_children_u6 == "Yes",
      demo_benefits_now_ei = dplyr::coalesce(demo_benefits_now_ei, demo_benefits_ever_ei),
      demo_benefits_now_ow = dplyr::coalesce(demo_benefits_now_ow, demo_benefits_ever_ow),
      demo_benefits_now_odsp = dplyr::coalesce(demo_benefits_now_odsp, demo_benefits_ever_odsp),
      demo_age = lubridate::ymd(glue::glue("{igs_dob_year}-{igs_dob_month}-15")) |>
        lubridate::interval(lubridate::as_date(end_date)) |>
        lubridate::as.period() |>
        lubridate::year(),
      ig_youth = demo_age < 30,
      ig_newcomer = case_when(
        is.na(igs_immigrant) ~ NA,
        igs_year_landed %in% "2019 or later" ~ TRUE,
        TRUE ~ FALSE
      ),
      ig_disability = igs_disability == "Yes" | (demo_benefits_ever_odsp %in% "Yes"),
      ig_francophone = case_when(
        is.na(igs_lang_first) ~ NA,
        igs_lang_first == "English" ~ FALSE,
        igs_lang_first == "French" ~ TRUE,
        igs_lang_english %in% "Yes" & igs_lang_fols %in% c("English", "Both") ~ FALSE,
        TRUE ~ TRUE
      ),
      ig_indigenous = igs_indigenous == "Yes",
      ig_social_assistance = demo_benefits_now_ow | demo_benefits_now_odsp
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      igs_race_nsel = sum(!is.na(c_across(matches("igs_race"))))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ig_racialized = case_when(
        igs_race_nsel == 0 ~ FALSE,
        igs_race_nsel > 1 ~ TRUE,
        !is.na(igs_race_10) ~ FALSE,
        TRUE ~ TRUE
      ),
      ig_total = TRUE,
      dplyr::across(
        matches("^igs_race_([0-9]$|10)"),
        ~ case_when(
          igs_race_nsel == 0 ~ NA,
          TRUE ~ !is.na(.x)
        )
      )
    ) |>
    dplyr::rename(!!!race_renamer)

  labelled::var_label(res) <- list(
    ig_youth = "Youth (< 30) with Higher Support Needs",
    ig_newcomer = "Newcomer (2019 or later)",
    ig_disability = "Experiencing Disability",
    ig_francophone = "Francophone",
    ig_indigenous = "Indigenous",
    ig_racialized = "Racialized",
    ig_social_assistance = "Receiving Social Assistance",
    ig_total = "Z) Total"
  )

  res |>
    dplyr::select(
      response_id,
      matches("^ig_"),
      matches("^demo_")
    )
}

clean_lfs <- function(d, dist = c("client", "panel"), browse = FALSE) {
  if (browse) browser()
  d_ <- d |>
    dplyr::mutate(
      lfs_employed = lfs_employed == "Yes",
      lfs_educ_enrolled = lfs_educ_enrolled == "Yes",
      lfs_neet = !(lfs_employed | lfs_educ_enrolled),
      lfs_n_jobs = dplyr::case_when(
        is.na(lfs_employed) ~ NA_integer_,
        !lfs_employed ~ 0L,
        TRUE ~ as.integer(lfs_n_jobs)
      ),
      lfs_unemp_spell = forcats::fct_expand(lfs_unemp_spell, "Currently employed"),
      lfs_unemp_spell = dplyr::case_when(
        lfs_employed ~ "Currently employed",
        TRUE ~ lfs_unemp_spell
      )
    )

  job_cols <- dist[1] |> 
    switch(
      client = r"(^a(\d)_job_(.*)$)",
      panel = r"(^job_(\d)_(.*)$)",
      {stop("Invalid distribution, please set dist='client' or dist='panel'")}
    )

  d__ <- d_ |>
    dplyr::select(
      response_id,
      lfs_n_jobs,
      matches(job_cols)
    ) |>
    tidyr::pivot_longer(
      cols = matches(job_cols),
      names_pattern = job_cols,
      names_to = c("i", ".value")
    ) |>
    dplyr::mutate(
      i = as.integer(i)
    ) |>
    dplyr::filter(lfs_n_jobs >= i) 

  if (dist[1] == "client") {
    d__ <- d__ |>
      dplyr::rename(
        benefits_health = benefits_1,
        benefits_dental = benefits_2,
        benefits_disability = benefits_3,
        benefits_pension = benefits_4,
        benefits_pto = benefits_5,
        is_casual = precarity_1,
        is_temporary = precarity_2,
        is_seasonal = precarity_3,
        sat_overall = satisfaction_overall,
        sat_advancement = satisfaction_advancement,
        sat_precarity = satisfaction_precarity,
        start_month = start_date_5_1,
        start_year = start_date_5_2,
      ) |> 
      dplyr::mutate(
        dplyr::across(
          matches("^benefits_"),
          ~ !is.na(.x)
        )
      )
  } else {
    d__ <- d__ |> 
      dplyr::mutate(
        dplyr::across(
          matches("benefits_"), 
          ~ stringr::str_detect(.x, "NO TO:", negate = TRUE)
        )
      )
  }

  d__ <- d__ |> 
    dplyr::mutate(
      dplyr::across(
        matches("^is_"),
        ~ .x == "Yes"
      ),
      start_date = lubridate::ymd(glue::glue("{readr::parse_number(as.character(start_year))}-{start_month}-15")),
      tenure = lubridate::interval(start_date, lubridate::ymd("2024-10-01")) |>
        lubridate::as.period(),
      salary = salary,
      annualized_earnings = dplyr::case_when(
        has_hourly_wage == "Yes" ~ hourly_wage * hours * 52,
        report_salary == "Weekly" ~ salary * 52,
        report_salary |> stringr::str_detect("Biweekly") ~ salary * 26,
        report_salary |> stringr::str_detect("Semimonthly") ~ salary * 24,
        report_salary == "Monthly" ~ salary * 12,
        report_salary == "Yearly" ~ salary,
        TRUE ~ NA_real_
      ),
      effective_hourly = annualized_earnings / (hours * 52)
    )

  job_summary <- d__ |>
    dplyr::group_by(response_id) |>
    dplyr::summarize(
      dplyr::across(
        matches("^benefits_|^is_"),
        list(
          any = ~ any(.x, na.rm = TRUE),
          primary = ~ dplyr::first(.x)
        )
      ),
      dplyr::across(
        matches("^sat_"),
        list(
          primary = ~ dplyr::first(.x),
          mean = ~ mean(as.integer(.x), na.rm = TRUE)
        )
      ),
      annualized_earnings_total = sum(annualized_earnings, na.rm = TRUE),
      annualized_earnings_primary = annualized_earnings[1],
      hours_total = sum(hours, na.rm = TRUE), 
      hours_primary = hours[1],
      effective_hourly_total = weighted.mean(effective_hourly, hours, na.rm = TRUE),
      effective_hourly_primary = effective_hourly[1],
      tenure_primary = dplyr::first(tenure),
      tenure_months_total = dplyr::case_when(
        all(is.na(tenure)) ~ NA,
        TRUE ~ max(lubridate::year(tenure) * 12 + lubridate::month(tenure), na.rm = TRUE)
      )
    ) |> 
    dplyr::rename_at(
      vars(-response_id), 
      ~ stringr::str_c("job_", .x)
    )

  res <- d_ |>
    dplyr::select(
      response_id,
      lfs_employed,
      lfs_educ_enrolled,
      lfs_neet,
      lfs_educ_type,
      lfs_unemp_spell,
      lfs_n_jobs
    ) |>
    dplyr::left_join(job_summary, by = "response_id")

  res
}

client_es <- function(dc) {
  dc |>
    dplyr::select(
      response_id,
      matches("^(eng_|ux_|explore_|progress_|start_|succeed_)")
    )
}

panel_demos <- function(d) {
  d_ <- d |> 
    dplyr::mutate(
      dplyr::across(
          matches("^benefits_(ei|ow|odsp)_(now|ever)"),
          ~ .x %in% "Yes"
      ),
      dplyr::across(
          matches("^(demo_(race|disability)|ig_)"),
          ~ .x |> stringr::str_detect("^NO TO", negate = TRUE)
      ),
      ig_indigenous = demo_race_indigenous,
      ig_social_assistance = benefits_ow_now | benefits_odsp_now,
      ig_francophone = FALSE,
      ig_total = TRUE
  )

  labelled::var_label(d_) <- list(
      ig_youth = "[Y] Youth (< 30) with Higher Support Needs",
      ig_newcomer = "[N] Newcomer (2019 or later)",
      ig_disability = "[D] Experiencing Disability",
      ig_francophone = "[F] Francophone",
      ig_indigenous = "[I] Indigenous",
      ig_racialized = "[R] Racialized",
      ig_social_assistance = "[S] Receiving Social Assistance",
      ig_total = "[Z] Total"
  )

  res <- d_ |> 
    dplyr::select(
      response_id, 
      matches("^ig_"), 
      matches("^demo_"),
      matches("^educ_"), 
      matches("^benefits_")
    )

  res
}

panel_es <- function(d) {
  d_ <- d |> 
    dplyr::select(
      response_id, 
      matches("^es_")
    )

  res <- d_ |> 
    dplyr::mutate(
      es_client = es_experience != "I have never received employment services", 
      es_is_aware = es_client | es_aware %in% "Yes", 
      es_nps_group = case_when(
        is.na(es_nps) ~ NA_character_,
        as.integer(es_nps) <= 7 ~ "Detractor", 
        as.integer(es_nps) >= 10 ~ "Promoter",
        TRUE ~ "Passive"
      ) |> forcats::fct_relevel("Detractor", "Passive", "Promoter")
    )

  res
}

rename_panel <- function(d, dict = here::here("data/panel-dict.csv")) {
  pr <- readr::read_csv(dict) |>
    dplyr::filter(!is.na(var_bp)) |>
    dplyr::mutate(var_leger = purrr::set_names(var_leger, var_bp)) |>
    dplyr::pull(var_leger)

  d |> dplyr::select(!!!pr) |>
    haven::as_factor()
  
  
}

panel_outreach <- function(d) {
  d |> 
    dplyr::select(
      response_id, 
      matches("^loc|^digi")
    ) |> 
    dplyr::mutate(
      dplyr::across(
        matches("^loc|^digi"),
        ~ str_detect(.x, "NO TO:", negate = TRUE)
      )
    )
}

clean_panel <- function(d, browse = FALSE) {
  d_ <- rename_panel(d)

  index <- d_ |> dplyr::select(response_id)
  demos <- panel_demos(d_)
  lfs <- clean_lfs(d_, dist = "panel", browse = browse)
  es <- panel_es(d_)
  out <- panel_outreach(d_)

  res <- index |>
    dplyr::left_join(demos, by = "response_id") |>
    dplyr::left_join(lfs, by = "response_id") |> 
    dplyr::left_join(es, by = "response_id") |> 
    dplyr::left_join(out, by = "response_id")

  res
}
