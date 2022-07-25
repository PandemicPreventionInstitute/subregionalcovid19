# Author: Kaitlyn Johnson
# script to test if we can just individually aggrate country level data within this repo
rm(list = ls())

source("LoadNorway.R")
source("LoadSouthAfrica.R")

GLOBALDATA<-c()
NORWAYDATA<-LoadNorway()
GLOBALDATA<- bind_rows(GLOBALDATA, NORWAYDATA)
SOUTHAFRICADATA<-LoadSouthAfrica()
GLOBALDATA<-bind_rows(GLOBALDATA, SOUTHAFRICADATA)