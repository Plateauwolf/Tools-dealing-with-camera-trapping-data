# Function is to calculate sunrise/sunset time and convert detection time to suntime.
# The functions will generate the following values:
# sunrise, sunrise.h, sunset, sunset.h, these are the calculated sunrise and sunset time.
# suntime is the converted time
# suntimeR is the suntime expressed in radius way.

suntime.func <- function(dete){
  library(maptools)
  library(lubridate)
  sun.tmp <- data.frame()
  for (i in 1:nrow(dete)){
    tmp1 <- matrix(as.numeric(dete[i, c("longitude","latitude")]), nrow = 1)
    tmp1 <- SpatialPoints(tmp1, proj4string = CRS("+proj=longlat +datum=WGS84"))
    di <- as.POSIXct(dete[i, "date"], tz = "Asia/Taipei")
    tmp2 <- sunriset(tmp1, di, direction = "sunrise", POSIXct.out = TRUE)
    sunrise <- format(tmp2$time, "%H:%M:%S")
    tmp3 <- hms(sunrise)
    sunrise.h <- hour(tmp3) + minute(tmp3)/60 + second(tmp3)/3600
    tmp4 <- sunriset(tmp1, di, direction = "sunset", POSIXct.out = TRUE)
    sunset <- format(tmp4$time, "%H:%M:%S")
    tmp5 <- hms(sunset)
    sunset.h <- hour(tmp5) + minute(tmp5)/60 + second(tmp5)/3600
    tmp6 <- cbind(sunrise, sunrise.h, sunset, sunset.h)
    sun.tmp <- rbind(sun.tmp, tmp6)
  }
  dete <- cbind(dete, sun.tmp)
  rm(sun.tmp,tmp1,di,tmp2,tmp3,sunrise,sunrise.h,tmp4,sunset,tmp5,sunset.h,tmp6,i)
  # Calculate suntime
  suntime.tmp = data.frame()
  for (i in 1:nrow(dete)){
    sunrise = dete$sunrise.h[i]
    sunrise = as.numeric(as.character(sunrise))
    sunset = dete$sunset.h[i]
    sunset = as.numeric(as.character(sunset))
    nighttime = (24 - sunset) + sunrise
    daytime = sunset - sunrise
    detec.hi = dete$detec.h[i]
    if (detec.hi >= sunrise & detec.hi <= sunset){
      suntime = 6 + (detec.hi - sunrise)/(daytime/12)
    }
    else if (detec.hi > sunset){
      suntime = 18 + (detec.hi - sunset)/(nighttime/12)
      if (suntime <= 24){
        suntime = suntime
      }
      else {
        suntime = suntime - 24
      }
    }
    else {
      suntime = sunrise - (sunrise - detec.hi)/(nighttime/12)
      if (suntime > 0){
        suntime = suntime
      }
      else {
        suntime = suntime + 24
      }
    }
    suntimeR = suntime/12*pi
    tmp = cbind(suntime, suntimeR)
    suntime.tmp = rbind(suntime.tmp, tmp)
  }
  dete = cbind(dete, suntime.tmp)
}