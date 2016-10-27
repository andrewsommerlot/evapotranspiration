#===========================================================================

# Edited et functions from Evapotranpiration package, and make_et_file function
# written by Andrew Sommerlot <asommerlot@chesapeakbay.net>
# October 27 2016

#===========================================================================

#***************************************************************************
#***************************************************************************

#===========================================================================

# modfified hargraves function

#===========================================================================

et_hargraves_cpb = function (data, constants, ts = "daily", type = "empirical", ...)
{
  if (is.null(data$Tmax) | is.null(data$Tmin)) {
    stop("Required data missing for 'Tmax.daily' and 'Tmin.daily', or 'Temp.subdaily'")
  }
  Ta <- (data$Tmax + data$Tmin)/2
  Ta[Ta < -17.8] <- -17.7999
  P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
  delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta + 237.3)^2)
  gamma <- 0.00163 * P/constants$lambda
  d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
  delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
  w_s <- acos(-tan(constants$lat_rad) * tan(delta2))
  N <- 24/pi * w_s
  R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) *
                                               sin(delta2) + cos(constants$lat_rad) * cos(delta2) *
                                               sin(w_s))
  C_HS <- 0.00185 * (data$Tmax - data$Tmin)^2 - 0.0433 * (data$Tmax -
                                                           data$Tmin) + 0.4023

  if(type == "empirical") {
    ET_HS.Daily <- 0.0135 * C_HS * R_a/constants$lambda * (data$Tmax -
                                                           data$Tmin)^0.5 * (Ta + 17.8)
  }
  if(type == "measured") {
    ET_HS.Daily <- 0.0135 * C_HS * data$Rs/constants$lambda * (data$Tmax -
                                                             data$Tmin)^0.5 * (Ta + 17.8)
  }
  if(type == "mean_cor") {
    ET_HS.Daily <- 0.0135 * C_HS * (mean(R_a)/mean(data$Rs)) * data$Rs/constants$lambda * (data$Tmax -
                                                             data$Tmin)^0.5 * (Ta + 17.8)
  }

  if(type == "bhatt") {
    Ra_bhatt = data$Rs / (constants$as + 0.45 * data$n / N) # 0.45 is bs, estimated for CBW

    ET_HS.Daily <- 0.0135 * C_HS * Ra_bhatt/constants$lambda * (data$Tmax -
                                                              data$Tmin)^0.5 * (Ta + 17.8)
  }
  ET.Daily <- ET_HS.Daily
  ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily,
                                               "%m/%y"), FUN = sum)
  ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.daily,
                                                               "%m/%y"))), FUN = sum)
  ET.MonthlyAve <- ET.AnnualAve <- NULL
  for (mon in min(as.POSIXlt(data$Date.daily)$mon):max(as.POSIXlt(data$Date.daily)$mon)) {
    i = mon - min(as.POSIXlt(data$Date.daily)$mon) + 1
    ET.MonthlyAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$mon ==
                                        mon])
  }
  for (year in min(as.POSIXlt(data$Date.daily)$year):max(as.POSIXlt(data$Date.daily)$year)) {
    i = year - min(as.POSIXlt(data$Date.daily)$year) + 1
    ET.AnnualAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$year ==
                                       year])
  }
  ET_formulation <- "Hargreaves-Samani"
  ET_type <- "Reference Crop ET"
  message(ET_formulation, " ", ET_type)
  message("Evaporative surface: reference crop")
  results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly,
                  ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve,
                  ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation,
                  ET_type = ET_type)
  if (ts == "daily") {
    res_ts <- ET.Daily
  }
  else if (ts == "monthly") {
    res_ts <- ET.Monthly
  }
  else if (ts == "annual") {
    res_ts <- ET.Annual
  }
  message("Timestep: ", ts)
  message("Units: mm")
  message("Time duration: ", time(res_ts[1]), " to ", time(res_ts[length(res_ts)]))
  if (NA %in% res_ts) {
    message(length(res_ts), " ET estimates obtained; ", length(which(is.na(res_ts))),
            " NA output entries due to missing data")
    message("Basic stats (NA excluded)")
    message("Mean: ", round(mean(res_ts, na.rm = T), digits = 2))
    message("Max: ", round(max(res_ts, na.rm = T), digits = 2))
    message("Min: ", round(min(res_ts, na.rm = T), digits = 2))
  }
  else {
    message(length(res_ts), " ET estimates obtained")
    message("Basic stats")
    message("Mean: ", round(mean(res_ts), digits = 2))
    message("Max: ", round(max(res_ts), digits = 2))
    message("Min: ", round(min(res_ts), digits = 2))
  }
  for (i in 1:length(results)) {
    namer <- names(results[i])
    write.table(as.character(namer), file = "ET_HargreavesSamani.csv",
                dec = ".", quote = FALSE, col.names = FALSE, row.names = F,
                append = TRUE, sep = ",")
    write.table(data.frame(get(namer, results)), file = "ET_HargreavesSamani.csv",
                col.names = F, append = T, sep = ",")
  }
  invisible(results)
}

#===========================================================================

# make et file function

#===========================================================================

make_hargraves_file = function(land_seg,
                        elevation,
                        latitude,
                        read_loc,
                        save_loc,
                        start_date = '',
                        end_date = '',
                        method = 'empirical') {

  require(Evapotranspiration)
  require(tidyr)
  data(constants)

  base_tmp <- read.csv(paste(read_loc, '/', land_seg, '.TMP', sep = ''), head = FALSE)
  base_rad <- read.csv(paste(read_loc, '/', land_seg, '.RAD', sep = ''), head = FALSE)

  # get min/max
  base_tmp_ag <- tidyr::unite_(base_tmp, paste(colnames(base_tmp)[c(1,2,3)], collapse="-"), colnames(base_tmp)[c(1,2,3)])
  base_tmp_ag[,1] <- as.Date(gsub('_', '-', base_tmp_ag[,1]))
  st <- which(base_tmp_ag$`V1-V2-V3` == start_date)[1]
  ed <- which(base_tmp_ag$`V1-V2-V3` == end_date)[length(which(base_tmp_ag$`V1-V2-V3` == end_date))]
  base_tmp_ag <- base_tmp_ag[st:ed,]

  # get sunshine hours from solar radiation data.
  base_rad_ag <- tidyr::unite_(base_rad, paste(colnames(base_rad)[c(1,2,3)], collapse="-"), colnames(base_rad)[c(1,2,3)])
  base_rad_ag[,1] <- as.Date(gsub('_', '-', base_rad_ag[,1]))
  st <- which(base_rad_ag$`V1-V2-V3` == start_date)[1]
  ed <- which(base_rad_ag$`V1-V2-V3` == end_date)[length(which(base_rad_ag$`V1-V2-V3` == end_date))]
  base_rad_ag <- base_rad_ag[st:ed,]

  count_hours = function(x) {
    all = sum(x>0)
    return(all)
  }

  base_rad_hrs = aggregate.data.frame(base_rad_ag[,3], by = list(base_rad_ag[,1]), FUN = 'count_hours')
  base_hrs = data.frame(date = base_rad_hrs[,1], nhrs = base_rad_hrs[,2] )

  # for julian day ..
  jday <- as.POSIXlt(base_tmp_ag$`V1-V2-V3`, format = "%d%b%y")$yday

  df1 <- base_tmp_ag
  df1$Year <- as.numeric(format(base_tmp_ag$`V1-V2-V3`, '%Y'))
  df1$Month <- as.numeric(format(base_tmp_ag$`V1-V2-V3`, '%m'))
  df1$Day <- as.numeric(format(base_tmp_ag$`V1-V2-V3`, '%d'))
  df1$Hour <- base_tmp_ag$V4

  df2 <- merge(df1, base_hrs, by.x = 'V1-V2-V3', by.y = 'date', all.x <- TRUE)
  df2$rad <- base_rad_ag[,3]
  df2$sdtemp <- base_tmp_ag$V5
  df2$julian <- jday

  # requries RH.subdaily, and Tdew. dummy values added below
  # get it in final form matching the evapo library .........
  base_data <- data.frame(Year = df2$Year,
                         Month = df2$Month,
                         Day = df2$Day,
                         Hour = df2$Hour,
                         Julian = df2$julian,
                         Temp.subdaily = df2$sdtemp,
                         Rs.subdaily = df2$rad*1.00416, # langlys per day to MJ/m^2/day
                         n.daily = df2$nhrs,
                         RH.subdaily = rep(50, nrow(df2)), # dummies for RH
                         Tdew.subdaily = rep(10, nrow(df2)) # dummies for Tdew
    )

  deg2rad <- function(deg) {
    rad <- deg * pi/180
    return(rad)
  }

  # update constants
  constants$lat <- latitude
  constants$lat_rad <- deg2rad(latitude)
  constants$as <- 0.23        #(Roderick, 1999, page 181)
  constants$bs <- 0.5  # I left these for now, not sure if they are good or not ....
  constants$Elev <- elevation # meters gaithersburg md
  constants$z <- 10 # meters

  base_input <- ReadInputs(base_data, constants,
                           stopmissing=c(1,1,1),
                           timestep="subdaily")

  har_est <- et_hargraves_cpb(base_input, constants = constants,  ts="daily", type = method)
  har_out <- data.frame(date = index(har_est$ET.Daily), pet = har_est$ET.Daily)
  row.names(har_out) = NULL

  daily_rad <- aggregate.data.frame(base_rad_ag[,3], by = list(base_rad_ag[,1]), FUN = sum)
  rad_ag <- merge(base_rad_ag, daily_rad, by.x = 'V1-V2-V3', by.y = 'Group.1', all.x = TRUE)
  rad_ag <- merge(rad_ag, har_out, by.x = 'V1-V2-V3', by.y = 'date', all.x = TRUE)
  rad_ag$hourly.pet = rad_ag$pet*(rad_ag$V5/rad_ag$x) / 25.4

  month <- as.numeric(format(rad_ag$`V1-V2-V3`, '%m'))
  day <- as.numeric(format(rad_ag$`V1-V2-V3`, '%d'))
  year <- as.numeric(format(rad_ag$`V1-V2-V3`, '%Y'))
  hour = rad_ag$V4
  specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

  har_print <- data.frame(year = year,
                         month = month,
                         day = day,
                         hour = hour,
                         pet = specify_decimal(rad_ag$hourly.pet, 10)
  )

  path <- paste(save_loc, '/', method, '/', land_seg, '.PET', sep = '')

  # check if the directory exists, if not make one, if so do nothing.
  tpath = paste(save_loc, '/', method, sep = '')
  if (file.exists(tpath)){
  } else {
    dir.create(tpath)
  }

  write.table(har_print, path, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
  cat(paste('File written to:', path))

  return(0)

}