country_panel <- within(country_panel,
    year <- ifelse(is.na(year) & !is.na(year2), round(year2 / 10), year)
)