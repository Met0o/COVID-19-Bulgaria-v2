library(taskscheduleR)
                     
taskscheduler_create(taskname = "COVID-19-Automation", rscript = "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/Updates.R", 
                     schedule = "DAILY", starttime = "11:00", startdate = format(Sys.Date(), "%m/%d/%Y"))

# taskscheduler_delete(taskname = "COVID")
 