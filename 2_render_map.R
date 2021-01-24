##########==========##########==========##########==========##########==========

## SET-UP ======================================================================

## meta-information
## author: Josh Mendelsohn
## creation: 2021-01-24
## version: R 4.0.3
## description: renders maps of road trips and road trip goals

## environment set up
remove(list = objects())
options(scipen = 2, digits = 6)
library(lintr) #lint("2_render_map.R")
library(tidyverse)

## LAYOUT PLAN =================================================================

## poster dimensions (layout plan)

## poster size: 36.0in x 24.0in, all measurements in inches
## margins: 0.5 on all sides
## map x/y ratio: 1.623

## panel dimensions (start-end coordinates)
## standard routes: 8.115 (0.5-8.615) x 5 (0.5-5.5) 
## routes traveled: 29.052 (0.5 - 29.552) x 17.9 (5.6-23.5)
## progress bar: 29.9 (5.6-35.5) x 5 (0.5-5.5)
## map legend: 5.848 (29.652-35.5) x 17.9 (5.6 x 23.5)

## READ IN DATA ================================================================

travels  <- readRDS("B_Intermediates/travels.RData")
progress <- readRDS("B_Intermediates/progress.RData")

## CALCULATE PLOT REGION COORDINATES ===========================================

## dimensions

## MAIN MAP ====================================================================

## INSET MAP ===================================================================

## PROGRESS BAR ================================================================

## MAP STYLING =================================================================

## EXPORT TO PDF ===============================================================

##########==========##########==========##########==========##########==========
