
#------------------------------LOADING PACKAGES ----------------------------------#

#########################LOADING PACKAGES ########################################
library(sparklyr)
spark_install(version = "1.6.1")
library(plyr)
library(dplyr)
library(pryr)
library(tidyr)
library(stringr)
library(tidyverse)
library(leaflet)
library(DT)
library(ggplot2)
library(tidyr)
library(shiny)
library(shinyWidgets)
library(stringr)
library(DBI)
#---------------CONNECTION SPARK ------------------------#

sc<- spark_connect(master = "local")

GTDB=read_csv('globalterrorismdb_0617dist.csv')

bb_spark=copy_to(sc,  GTDB ,  overwrite = TRUE)

#--------------Connection SQLITE ---------------------------#

con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")



