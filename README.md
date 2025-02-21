# SOAR_V1
SOAR is a data-driven resource that helps Oregonians understand air quality issues and engage in finding solutions that improve air quality in their communities. SOAR offers a web-based portal that centralizes all air quality information. 


📌 Overview
The Ozone-DB project retrieves air quality and meteorological data from:

The EPA's AQS API
Envista

It collects data for specific parameters listed in:
📄 data/reference/analyteXcode.csv


please see the miro board using the following link:
https://miro.com/app/board/uXjVPL4Eie8=/

if you don't have access, let me know (Neda)


There are a few lines in this directory that allow users to request queries for both hourly and daily data from
the AQS and Envista APIs (endpoints), giving users more control over decision-making.
Path: src/r/utils/Forvalidation_AQS_Envista_API.R