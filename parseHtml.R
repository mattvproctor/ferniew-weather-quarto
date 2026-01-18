parse_html <- function(){

  #####

  library(rvest)

  html_file <- read_html("https://mountainweather.ca/data/STUDYPLOT72.HTM")

  html_table <- html_elements(html_file, "pre")

  data_frame_space_delim <- html_text2(html_table) %>%
    stringr::str_split("\r\n") %>%
    as.data.frame()

  names(data_frame_space_delim) <- "All"

  data_frame_space_delim_trim <- data_frame_space_delim %>% dplyr::filter(stringr::str_detect(All, "-")) %>%
    dplyr::filter(!stringr::str_detect(All, "---"))

  column_vector <- c(
    "blank",
    "Date",
    "Time",
    "PresTempAir",
    "2DayMaxTempAir",
    "2DayMinTempAir",
    "24HMaxTempAir",
    "24HMinTempAir",
    "RelativeHumidity%",
    "HourlyPrecipMM",
    "2DayPrecipMM",
    "24HPrecipMM",
    "Barom",
    "HS",
    "2DayVirtualHN",
    "24HVirtualHN",
    "AvgWindSpeed",
    "WindDrCompass",
    "WindDrDegrees"
  )

  data_frame <- data_frame_space_delim_trim %>%
    tidyr::separate("All", column_vector, sep = "\\s+") %>%
    dplyr::select(-blank) %>%
    dplyr::mutate(Date = substr(Date, 2, nchar(Date)), Time = substr(Time, 1, nchar(Time)-1), DateTime = paste(Date, Time)) %>%
    dplyr::mutate(PresTempAir = as.numeric(PresTempAir),
                  Timestamp = lubridate::ymd_hms(DateTime),
                  HourlyPrecipMM = as.numeric(HourlyPrecipMM),
                  HourlyCumulativePrecipMM = cumsum(HourlyPrecipMM),
                  PrecipType = ifelse(PresTempAir > 0.5, "Rain", ifelse(PresTempAir < -1, "Snow", "Mixed")),
                  HourlyRainMM = ifelse(PrecipType == "Rain", HourlyPrecipMM, 0),
                  HourlyMixedMM = ifelse(PrecipType == "Mixed", HourlyPrecipMM, 0),
                  HourlySnowMM = ifelse(PrecipType == "Snow", HourlyPrecipMM, 0),
                  HourlyRainCumulativeMM = cumsum(HourlyRainMM),
                  HourlyMixedCumulativeMM = cumsum(HourlyMixedMM),
                  HourlySnowCumulativeMM = cumsum(HourlySnowMM),
                  WindDrDegrees = as.numeric(WindDrDegrees),
                  AvgWindSpeed = as.numeric(AvgWindSpeed))





  
  # highlight_table

  last_6_hours = tail(data_frame, 6)
  last_12_hours = tail(data_frame, 12)
  last_24_hours = tail(data_frame, 24)
  last_48_hours = tail(data_frame, 48)

  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  highlight_table = tibble::tibble(
    Timeframe = c("6 Hour", "12 Hour", "24 Hour", "2 Day", "3 Day"),
    "Avg Temp (°C)" = c(mean(last_6_hours$PresTempAir), mean(last_12_hours$PresTempAir), mean(last_24_hours$PresTempAir), mean(last_48_hours$PresTempAir), mean(data_frame$PresTempAir)),
    "Max Temp (°C)" = c(max(last_6_hours$PresTempAir), max(last_12_hours$PresTempAir), max(last_24_hours$PresTempAir), max(last_48_hours$PresTempAir), max(data_frame$PresTempAir)),
    "Min Temp (°C)" = c(min(last_6_hours$PresTempAir), min(last_12_hours$PresTempAir), min(last_24_hours$PresTempAir), min(last_48_hours$PresTempAir), min(data_frame$PresTempAir)),
    "Total Precip (mm)" = c(sum(last_6_hours$HourlyPrecipMM), sum(last_12_hours$HourlyPrecipMM), sum(last_24_hours$HourlyPrecipMM), sum(last_48_hours$HourlyPrecipMM), sum(data_frame$HourlyPrecipMM)),
    "- Snow (cm)" = c(sum(last_6_hours$HourlySnowMM), sum(last_12_hours$HourlySnowMM), sum(last_24_hours$HourlySnowMM), sum(last_48_hours$HourlySnowMM), sum(data_frame$HourlySnowMM)),
    "- Rain (mm)" = c(sum(last_6_hours$HourlyRainMM), sum(last_12_hours$HourlyRainMM), sum(last_24_hours$HourlyRainMM), sum(last_48_hours$HourlyRainMM), sum(data_frame$HourlyRainMM)),
    "- Mixed / Wet Snow (mm)" = c(sum(last_6_hours$HourlyMixedMM), sum(last_12_hours$HourlyMixedMM), sum(last_24_hours$HourlyMixedMM), sum(last_48_hours$HourlyMixedMM), sum(data_frame$HourlyMixedMM)),
    "Avg Wind Speed (m/s)" = c(mean(last_6_hours$AvgWindSpeed), mean(last_12_hours$AvgWindSpeed), mean(last_24_hours$AvgWindSpeed), mean(last_48_hours$AvgWindSpeed), mean(data_frame$AvgWindSpeed)),
    "Max Wind Speed (m/s)" = c(max(last_6_hours$AvgWindSpeed), max(last_12_hours$AvgWindSpeed), max(last_24_hours$AvgWindSpeed), max(last_48_hours$AvgWindSpeed), max(data_frame$AvgWindSpeed)),
    "Prevelant Wind Direction" = c(get_mode(last_6_hours$WindDrCompass), get_mode(last_12_hours$WindDrCompass), get_mode(last_24_hours$WindDrCompass), get_mode(last_48_hours$WindDrCompass), get_mode(data_frame$WindDrCompass))

  )

  highlight_table <- highlight_table %>%
    dplyr::mutate(`Avg Temp (°C)` = round(`Avg Temp (°C)`, 1),
                  `Avg Wind Speed (m/s)` = round(`Avg Wind Speed (m/s)`, 1))

  highlight_table_transpose = as.data.frame(t(highlight_table))
  highlight_table_transpose_headers = highlight_table_transpose[-1, ] %>% 
    setNames(unlist(highlight_table_transpose[1, ])) # %>%
    # dplyr::mutate(Indicator = c(tail(names(highlight_table),9))) %>%
    # dplyr::relocate(Indicator)



  griz_cam_image = magick::image_read("https://secure.skircr.com/cams/fecam6/image.jpg")

  lakeman_html = read_html("https://skifernie.com/conditions/alpine-weather/") %>%
    html_elements("#tblalpineWeatherReport") %>%
    html_text2()
  
  
  # Avalanche Canada API
  library(httr)
  library(jsonlite)

  res <- VERB("GET", url = "api.avalanche.ca/forecasts/en/products/point?lat=49.46172799815629&long=-115.11955649578978")

  

  avi_content <- content(res, 'text')
  avi_bulletin <- fromJSON(avi_content)




  return_vector <- list(
    "highlight_table" = highlight_table_transpose_headers,
    "data_today" = data_frame,
    "griz_cam_image" = griz_cam_image,
    "Lakeman_forecast" = lakeman_html,
    "avi_canada" = avi_bulletin
  )

  return(
    return_vector
  )

}


# wind_data = data_frame %>% dplyr::select(AvgWindSpeed, WindDrDegrees, WindDrCompass, Timestamp) %>%
#   dplyr::arrange(WindDrDegrees)

# plotly::plot_ly(wind_data, type='barpolar', r=~AvgWindSpeed, theta=~WindDrCompass, color=~AvgWindSpeed) %>%
#   plotly::layout(polar=list(angularaxis=list(direction="clockwise")))

# print(plotly::wind)

# plotly::plot_ly(plotly::wind, type='barpolar', r=~r, theta=~t, color=~nms) %>%
#   plotly::layout(polar=list(angularaxis=list(direction="clockwise")))

