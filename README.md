# CSSS569HW3CarrSinclair

Author: Sinclair Carr
Email: shcarr@uw.edu
Student ID: 2126987

This repository contains all the data and code used for `CSSS 569 Problem Set 3`. The purpose of this code is to create an interactive Shiny visualization to vet the results of the modeling process in the calculation of population attributable fractions (PAFs). The Shiny tool displays figures comparing results from two separate cycles of the Global Burden of Disease, Injuries, and Risk Factors study (GBD), focusing on the alcohol consumption risk factor. For exposure data, cycles from 2020 were compared to 2019. For relative risk (RR) estimates, the 2020 cycle was compared with the 2016 cycle. It is important to note that only the five risk-outcome (RO) pairs, i.e., risk functions between alcohol consumption and the disease or injury of interest, which cause the most disease burden were updated with the new meta-regression tool MR-BRT (Meta-Regression - Bayesian, Regularized, Trimmed). These RO pairs are tuberculosis, diabetes mellitus, ischemic heart disease, ischemic stroke, and intracerebral hemorrhage. Thus, the other RO pairs have fully overlapping risk curves (mean estimates and 95% uncertainty intervals) when comparing the two rounds. Since I will be updating the remaining RO pairs as part of my capstone project, I have integrated them into the toggle option of the shiny application, i.e. to use this interactive visualization for future vetting processes.

## data
Contains .RDS files of all data used in analysis (including modeled results and metadata).

## src
Sourcing scripts.

## get_data.R
Script used to compile all data used in the shiny application (must be ran on IHME cluster).

## build_shiny.R
Script used to build the shiny, using navarPage as the main user interface.

## How to use
Download repository and open the project csss_569_hw3.Rproj
Run src/build_shiny.R

## Credits
In collaboration with Justin Lo (lojustin@uw.edu)