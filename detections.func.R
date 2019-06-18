# Description of detections.func()

# This function is used to extract detections from camera trapping data
# We extract detections, but not use photos; because consecutive photoes are 
# often correlated, which bias activity pattern

# This function has 2 inputs.

# 1) "distance (in minutes)" 
# is the interval used when dividing detections. For instance, when 30 minutes
# is used, and we have two groups of consecutive photoes recorded in 2014/11/1:
        # Group A 06:11 06:14 06:18 06:20
        # Group B 06:55 06:54 06:58 07:00
# The difference between last photo of group A (06:20) and the first photo of 
# group B (06:55) is > 30 minutes, and difference between consecutive photoes in
# each group is <= 30 minutes, then group A and group B are 2 detections. This
# function calculated the detection time as the middle of each group.

# 2) "activity" 
# is the camera trapping data, in which we want to extract detections. 
# In the data, the following columns are need:
#    "sessions", representing diffent survey locations/sessions
#    "capture_time", the time each photo was taken in format "2011/11/1 19:48"
#
# This function needs package "lubridate"
#
# Output
# The function will generate a table of all the independent detections.
# The number of rows is the number of independent detections.
# It will add 3 new columns:
# "timelap": it is a byproduct, not useful;
# "detec.time": the time of this independent detection;
# detec.h": the time is expressed as hour, i.e., 12:30 is 12.5h.

detections.func <- function(activity, distance){
  library(lubridate)
  sessions.unique = activity$sessions
  sessions.unique = levels(factor(sessions.unique))
  # Find all the records in each session and combine them all
  
  detections = data.frame() # used to store all detections
  # Find detections in the following code
  for (i in 1:length(sessions.unique)){
    subsess = subset(activity,sessions == sessions.unique[i]) 
    rowno = c(1:nrow(subsess))
    subsess = data.frame(subsess, rowno)
    subsess = subsess[order(subsess$rowno,subsess$capture_time),]
    # Calculate the intervals between consecutive photoes
    timelap = vector(mode = "integer", length = length(subsess$capture_time))
    if (nrow(subsess)==1){
      timelap[1] = distance + 1 
      # Some sessions have only 1 photo.
      # The interval of the first photo needs to be > distance; so we can use it
      # as a mark of detection.
    }
    else {
      timelap[1] = distance + 1 
      for (t in 2:nrow(subsess)){
        timelap[t] = difftime(subsess$capture_time[t],
                              subsess$capture_time[t-1], units = "mins")
      }
    }
    subsess = data.frame(subsess, timelap)
    subsess = subsess[order(subsess$rowno, decreasing = TRUE),] 
    # Reorder by decreasing = TRUE; then we can use intervals > distance as
    # marks of detections
    stp = 0 # Counter
    for (q in 1:nrow(subsess)){
      if (subsess$timelap[q] <= distance){
        stp = stp + 1
      } 
      else {
        dif = difftime(subsess$capture_time[(q-stp)],subsess$capture_time[q],
                       units = "secs")
        detec = as.POSIXct(subsess$capture_time[q]) + dif/2
        detec.rec = subsess[q,]
        detec.rec$capture_time = detec
        detec.time = format(detec, "%H:%M:%S") # extract time only
        tmp1 = hms(detec.time)
        detec.h = hour(tmp1) + minute(tmp1)/60 + second(tmp1)/3600
        # convert time to hour
        detec.rec = cbind(detec.rec, detec.time,detec.h)
        detections = rbind(detections, detec.rec)
        stp =0
        
      } 
    }
  }
  detections = detections[order(detections$sessions,detections$capture_time,
                                decreasing = FALSE),]
  row.names(detections) = c(1:nrow(detections))
  detections = subset(detections, select = -c(rowno))
  return(detections)
}