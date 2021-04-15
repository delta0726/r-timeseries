# Title     :
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       :



# ＜ポイント＞





# ＜構文＞
# tk_make_weekday_sequence(
#   start_date,
#   end_date,
#   remove_weekends = TRUE,
#   remove_holidays = FALSE,
#   calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH"),
#   skip_values = NULL,
#   insert_values = NULL
# )


tk_make_weekday_sequence("2017", "2018", remove_holidays = TRUE)

tk_make_weekday_sequence("2017-01", "2017-02", remove_holidays = FALSE)