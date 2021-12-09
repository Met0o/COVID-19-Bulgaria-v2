library(taskscheduleR)
                     
taskscheduler_create(taskname = "COVID-19-Automation", rscript = "C://R/COVID/Updates.R", 
                     schedule = "DAILY", starttime = "12:00", startdate = format(Sys.Date(), "%m/%d/%Y"))

# taskscheduler_delete(taskname = "COVID")
 