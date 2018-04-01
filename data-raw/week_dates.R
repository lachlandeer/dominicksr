library(lubridate)
library(magrittr)
library(dplyr)
library(purrr)
library(timeDate)

start_date <- dmy("14-09-1989")
end_date   <- start_date + days(6)
n_weeks    <- 400

# generate week start and end dates
start_of_week           <- as.data.frame(seq(start_date, by = "week", length.out = n_weeks))
colnames(start_of_week) <- "start_of_week"

end_of_week           <- as.data.frame(seq(end_date, by = "week", length.out = n_weeks))
colnames(end_of_week) <- "end_of_week"

week_id           <- as.data.frame(seq(1, by = 1, length.out = n_weeks))
colnames(week_id) <- "week_id"

# create data frame of week start and week end dates
week_start_date <- bind_cols(list(week_id, start_of_week))
week_end_date   <- bind_cols(list(week_id, end_of_week))

week_dates <- inner_join(week_start_date, week_end_date, by = "week_id")

rm(start_of_week, end_of_week, week_id, week_start_date, week_end_date)

# now get potential public holidays
min_year <- year(min(week_dates$start_of_week))
max_year <- year(max(week_dates$start_of_week))
all_years <- seq(from = min_year, to = max_year, by =1)

us_holidays <- timeDate::listHolidays(pattern = "US")

args <- list(holiday_type = us_holidays, current_year = all_years)

holiday_occurence <- cross_df(args)

for(i in 1:nrow(holiday_occurence)) {
    # holiday is an S4 class which makes life a little difficult
    res <- holiday(year = holiday_occurence$current_year[i], Holiday = holiday_occurence$holiday_type[i])
    res2 <- format(res@Data)
    holiday_occurence$holiday_date[i] <- res2
    }

holiday_occurence  <- holiday_occurence %>%
                        mutate(holiday_date = ymd(holiday_date))

# We need to add halloween

halloween <- "USHalloween"
halloween_occurence <- cross_df(list(holiday_type = halloween, current_year = all_years))
halloween_occurence <- halloween_occurence %>%
                            mutate(holiday_date = ymd(stringr::str_c(current_year, "-10-31")))

holiday_occurence <- bind_rows(holiday_occurence, halloween_occurence)

# join holidays to week data
# it's painful since its an based on whether a holiday is in an interval

week_id_holidays <- week_dates %>%
                        mutate(dummy=TRUE) %>%
                        left_join(holiday_occurence %>% mutate(dummy=TRUE)) %>%
                        filter(holiday_date <= end_of_week & holiday_date >= start_of_week) %>%
                        select(week_id, holiday_type, holiday_date)  %>%
                        # combine multiple holidays in same week
                        group_by(week_id) %>%
                        summarise(holiday_type = paste(holiday_type, collapse = ", "),
                                  holiday_date = paste(holiday_date, collapse = ", "))

week_dates <- week_dates %>%
                    left_join(week_id_holidays, by = "week_id")

# save
readr::write_csv(week_dates, "data-raw/week_dates.csv")
devtools::use_data(week_dates, overwrite = TRUE, compress = 'xz')
