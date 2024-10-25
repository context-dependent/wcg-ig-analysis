clean_client <- function(d, bot_threshold = .9) {
  dc <- d |> unpack_client(bot_threshold)

  index <- dc |> dplyr::select(response_id)
  demos <- client_demos(dc)
  lfs <- client_lfs(dc)
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

client_lfs <- function(dc) {
  res <- dc |>
    dplyr::mutate(
      lfs_employed = lfs_employed == "Yes",
      lfs_educ_enrolled = lfs_educ_enrolled == "Yes",
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

  job <- res |>
    dplyr::select(
      response_id,
      lfs_n_jobs,
      matches("a\\d_job")
    ) |>
    tidyr::pivot_longer(
      cols = matches("a\\d_job"),
      names_pattern = "a(\\d)_job_(.*)",
      names_to = c("i", ".value")
    ) |>
    dplyr::mutate(
      i = as.integer(i)
    ) |>
    dplyr::filter(lfs_n_jobs >= i) |>
    dplyr::rename(
      job_benefits_health = benefits_1,
      job_benefits_dental = benefits_2,
      job_benefits_disability = benefits_3,
      job_benefits_pension = benefits_4,
      job_benefits_pto = benefits_5,
      job_is_casual = precarity_1,
      job_is_temporary = precarity_2,
      job_is_seasonal = precarity_3,
      job_sat_overall = satisfaction_overall,
      job_sat_advancement = satisfaction_advancement,
      job_sat_precarity = satisfaction_precarity,
    ) |>
    dplyr::mutate(
      dplyr::across(
        matches("^job_benefits_"),
        ~ !is.na(.x)
      ),
      dplyr::across(
        matches("^job_is_"),
        ~ .x == "Yes"
      ),
      start_year = readr::parse_number(as.character(start_date_5_2)),
      job_start_date = lubridate::ymd(glue::glue("{start_year}-{start_date_5_1}-15")),
      job_tenure = lubridate::interval(job_start_date, lubridate::ymd("2024-10-01")) |>
        lubridate::as.period(),
      salary = salary,
      job_annualized_earnings = dplyr::case_when(
        has_hourly_wage == "Yes" ~ hourly_wage * hours * 52,
        report_salary == "Weekly" ~ salary * 52,
        report_salary |> stringr::str_detect("Biweekly") ~ salary * 26,
        report_salary |> stringr::str_detect("Semimonthly") ~ salary * 24,
        report_salary == "Monthly" ~ salary * 12,
        report_salary == "Yearly" ~ salary,
        TRUE ~ NA_real_
      ),
      job_effective_hourly = job_annualized_earnings / (hours * 52)
    )

  job_summary <- job |>
    dplyr::group_by(response_id) |>
    dplyr::summarize(
      dplyr::across(
        matches("^job_benefits_|^job_is_"),
        list(
          any = ~ any(.x, na.rm = TRUE),
          primary = ~ dplyr::first(.x)
        )
      ),
      dplyr::across(
        matches("^job_sat_"),
        list(
          primary = ~ dplyr::first(.x),
          mean = ~ mean(as.integer(.x), na.rm = TRUE)
        )
      ),
      job_annualized_earnings_total = sum(job_annualized_earnings, na.rm = TRUE),
      job_annualized_earnings_primary = job_annualized_earnings[1],
      job_effective_hourly_total = weighted.mean(job_effective_hourly, hours, na.rm = TRUE),
      job_effective_hourly_primary = job_effective_hourly[1],
      job_tenure_primary = dplyr::first(job_tenure),
      job_tenure_total = dplyr::case_when(
        all(is.na(job_tenure)) ~ NA,
        TRUE ~ max(job_tenure, na.rm = TRUE)
      )
    )

  res |>
    dplyr::select(
      response_id,
      lfs_employed,
      lfs_educ_enrolled,
      lfs_educ_type,
      lfs_unemp_spell,
      lfs_n_jobs
    ) |>
    dplyr::left_join(job_summary, by = "response_id")
}

client_es <- function(dc) {
  dc |>
    dplyr::select(
      response_id,
      matches("^(eng_|ux_|explore_|progress_|start_|succeed_)")
    )
}
