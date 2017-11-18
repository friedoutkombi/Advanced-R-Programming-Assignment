# The purpose of this file is to outline the functions used in Part 2 
# Longitudinal Data Class and Methods of the Functional & Object Oriented
# Programming Assignment for the Coursera - Advanced R Programming course.

# Descr: The purpose of this part is to create a new class for representing
# longitudinal data, which is data that is collected over time on a given 
# subject/person. This data may be collected at multiple visits, in multiple 
# locations. You will need to write a series of generics and methods for 
# interacting with this kind of data.

#### Please note the methods coded are not exhaustive and are only there to
#### meet the demands of the input questions outlined.

#-----------------------LOAD LIBRARIES---------------------------#
library(readr)
library(dplyr)
library(tidyr)
#------------------------DATA VARIABLES--------------------------#

#The variables in the dataset are
## id: the subject identification number
## visit: the visit number which can be 0, 1, or 2
## room: the room in which the monitor was placed
## value: the level of pollution in micrograms per cubic meter
## timepoint: the time point of the monitor value for a given visit/room

#-----------------------WRITE FUNCTIONS--------------------------#

# 1. Write a function make_LD: a function that converts a data frame into a “LongitudinalData” object.
make_LD<-function(DF){
 df<- nest(DF, -id)
  output<-structure(df,class="LongitudinalData")
  return(invisible(output))
}

# 2. subject: a generic function for extracting subject-specific information
subject <- function (df, id) {
  UseMethod("subject")
}

# 3. visit: a generic function for extracting visit-specific information
visit <- function ( subject, visit) {
  UseMethod("visit")
}

# 4. room: a generic function for extracting room-specific information
room <- function( visit, room) {
  UseMethod("room")
}
#--------------------METHODS: LONGITUDINALDATA-------------------#
# Define methods for 1. LongitudinalData Objects.
# Only those required for assignment.

#This method prints out the amount of subjects in the dataset.
print.LongitudinalData <- function(x) {
  cat("Longitudinal dataset with", length(x$id), "subjects")
  return (invisible(x))
}

# This method returns a subject instance when an id is given.
subject.LongitudinalData<-function(df, id) {
  index <- which(df$id == id)
  if (length(index) == 0)
    return(NULL)
  structure(list(id = id, data = df$data[[index]]), class = "Subject")
}

#------------------------METHODS: SUBJECT------------------------#
# Write methods for 2. Subject objects.

# This method prints out the id of the subject instance.
print.Subject <-function(x) {
  cat("Subject ID:",x$id)
  invisible(x)
}

# Method returns summary of subject instance.
summary.Subject<-function(instance) {
  output <- instance$data %>% 
    group_by(visit, room) %>%
    summarise(value = mean(value)) %>% 
    spread(room, value) %>% 
    as.data.frame
  structure(list(id = instance$id,
                 output = output), class = "Summary")
}

# Method takes subject instance and visit number, returns visit instance.
Visit.Subject <- function(subject, visitnum) {
  data <- subject$data %>% 
    filter(visit == visitnum) %>% 
    select(-visit)
  structure(list(id = subject$id,
                 visitnum = visitnum,
                 data = data), class = "Visit")
}

#--------------------------METHODS: VISIT------------------------#
# Write methods for 3. Visit objects.

# Method takes visit instance and room name, returns room instance.
Room.Visit <- function(visit, roomname) {
  data <- visit$data %>% 
    filter(room == roomname) %>% 
    select(-room)
  structure(list(id = visit$id,
                 visitnum = visit$visitnum,
                 room = roomname,
                 data = data), class = "Room")
}
#--------------------------METHODS: ROOM-------------------------#
# Write methods for 4. Room objects.

# Method returns summary for the room instance.
summary.Room <- function(object) {
  output <- summary(object$data$value)
  structure(list(id = object$id,
                 output = output), class = "Summary")
}

# Method returns details for room instance.
print.Room <- function(x) {
  cat("ID:", x$id, "\n")
  cat("Visit:", x$visitnum, "\n")
  cat("Room:", x$room)
  invisible(x)
}


#---------------------------METHODS: SUMMARY---------------------#
# Write methods for Summary, note this is not linked to a generic
# function as above, this is the final print allowing us to view
# the output of the summary method functions. You will noticr the
# column calls are not named as such as theire are multiple summary calls.

print.Summary <- function(x) {
  cat("ID:", x[[1]], "\n")
  print(x[[2]])
  invisible(x)
}



#--------------------------END OF PROGRAM------------------------#
