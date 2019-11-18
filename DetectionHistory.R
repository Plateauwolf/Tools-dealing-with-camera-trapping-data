#######################################################################################################
# This function is used to generate 0/1 matrix for occupancy analysis. it will also generate a matrix
# which is composed of Julian Day for each survey. It has 4 arguments to set:
# 1) CamLocation
#    This argument specifys the data that contains your camera location entries.
#    The data should have 4 columns named by:
#         "location"---Camera locations
#         "start"---Start date of camera working
#         "end"---End date of camera working
#         "effort"---days of camera working, i.e. the difference between end
#                    and start.
# 2) photoList
#    This argument specifys the data that contains your image entries. The data
#    should have 3 columns named by:
#         "location"---Camera locations
#         "date"---date of images
#         "species"---name of species
# 3) sessionLen
#    It sets by how many days you want to divide you data into occasions. The
#    default value is 15 days, but you can set it as any length (in days) as
#    you want.
# 4) speciesname
#    It chooses the species that you want to generte the matrix for, especially
#    your photolist includes records for multiple species.
# 5) julian.start
#    The start date with which we calculate Julian Day. It should be input as a character,
#    like julian.start = "2017-09-1 9:15:00"
# 6) multiyear
#    This parameter means if your survey happens in multiple years. Normally we want to know
#    Julian day from a start date in the corresponding year, like Julian day of 
#    "2017-09-1 9:15:00" to "07-1 9:15:00" of year 2017 (say it is the start of breeding season).
#    If surveys happened in multiple years, calculate Julian day of "2017-09-1 9:15:00" to 
#    "2016-07-1 9:15:00" will not be what we want. So set multiyear = TRUE.
#    Otherwise, set multiyear = FALSE 
# The function starts--------------------------------------------------------
DetectionHistory <- function(CamLocation, photoList, 
                             sessionLen = 15, speciesname, julian.start, multiyear){
#  CamLocation <- read.csv("CamLocation.csv", header = TRUE)
  require(progress)
  print(speciesname)
  tmpoccasion <- ceiling(max(CamLocation$effort)/sessionLen)
  DH <- matrix(data = NA, nrow = dim(CamLocation)[1],
               ncol = tmpoccasion,
               dimnames = list(NULL, paste("Occasion", c(1:tmpoccasion))))
  if(julian.start!=""){
    julian.day <- DH
  }
  photoList$date <- as.POSIXct(photoList$date, tz="Asia/Taipei")
  photoList$location <- as.character(photoList$location)
  CamLocation$location <- as.character(CamLocation$location)
  pr <- 0 #to store how many circles there are
  for (i in 1:dim(CamLocation)[1]){
    pr <- pr + ceiling(CamLocation$effort[i]/sessionLen)
  }
  pb <- progress_bar$new(format = "  processing [:bar] :percent eta: :eta",
                         total = pr, clear = FALSE, width= 60)
  for (i in 1:dim(CamLocation)[1]){
     #print(CamLocation$location[i])
     startD <- as.POSIXct(CamLocation$start[i], tz="Asia/Taipei")
     endD <- startD + sessionLen*24*3600
     for (j in 1: ceiling(CamLocation$effort[i]/sessionLen)){
       #print(startD);print(endD)
       tmpdetection <- subset(photoList, photoList$species == speciesname&
                              photoList$location == CamLocation$location[i]&
                              photoList$date >= startD&
                              photoList$date <= endD)
       midday <- mean.POSIXct(x = c(startD,endD))
       tmpjulian <- round(as.numeric(julian(x = midday, origin = julian.start)))
       if(multiyear){
         tmpjulian <- tmpjulian%%365
       }else{
         tmpjulian <- tmpjulian
       }
       if (dim(tmpdetection)[1] == 0){
         if (j < ceiling(CamLocation$effort[i]/sessionLen)){
           DH[i,j] = 0
           if(julian.start!=""){julian.day[i,j] = tmpjulian}
         }
         else if (endD > as.POSIXct(CamLocation$end[i], tz="Asia/Taipei")){
           DH[i,j] = NA
           if(julian.start!=""){julian.day[i,j] = NA}
         }
         else {
           DH[i,j] = 0
           if(julian.start!=""){julian.day[i,j] = tmpjulian}
         }
       }
       else{
         DH[i,j] = 1
         if(julian.start!=""){julian.day[i,j] = tmpjulian}
       }
       startD <- startD + sessionLen*24*3600
       endD <- endD + sessionLen*24*3600
       pb$tick()
     }
  }
  rownames(DH) <- CamLocation$location
  rownames(julian.day) <- CamLocation$location
  if (is.na(sum(DH[,tmpoccasion]))){
    DH <- DH[,-tmpoccasion]
    julian.day <- julian.day[,-tmpoccasion]
  }
  if(julian.start!=""){
    return(list(DH,julian.day))
  }else{
    return(DH)
  }
}