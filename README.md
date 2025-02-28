SOAR_V1
SOAR is a data-driven resource that helps Oregonians understand air quality issues and engage in finding solutions to 
improve air quality in their communities. It offers a web-based portal that centralizes all air quality information.

This database can be integrated with a Power BI dashboard to generate various summary reports and visualizations, 
providing insights into air quality trends and related data.

************************************************************************************************************************S
📌 important Notes:

Create a Secure credentials.json File to store all aqs & envista credentials securely:
{
  "AQS": {
    "email": "your_aqs_email@example.com",
    "api_key": "your_aqs_api_key"
  },
  "Envista": {
    "username": "your_envista_username",
    "password": "your_envista_password"
  },
  "OregonAPI": {
    "baseurl": "https://aqiapi.oregon.gov/"
  }
}

🔹 Instructions for Users:
Replace placeholders (your_aqs_email@example.com, your_aqs_api_key, etc.) with actual credentials.
Keep baseurl for Envista empty ("") so users can define it dynamically.
Ensure all values are enclosed in double quotes ("").
Do NOT commit this file to GitHub – Add credentials.json to .gitignore to protect sensitive information.

-If your project already has a .gitignore file, open it.
-If there is no .gitignore file, create one in your project root directory.

Open .gitignore in a text editor (e.g., VS Code, RStudio, Notepad++) and add this line:
credentials.json
#This tells Git to ignore this file and never track it.


Pass SIGNIN explicitly in every function that needs credentials.
Securely load JSON using jsonlite.


************************************************************************************************************************

📌 Overview
The Ozone_DB R package allows users to pull hourly data () for the following parameters over a specified period:

Criteria Air Pollutants
PM₂.₅
FRM/FEM: pm2.5l_bam1022
Nephelometer: pm2.5 estimate
SensOR: pm2.5 est sensor

Ozone, CO, and NOX

Meteorological Data
Ambient temperature
Wind speed & direction
Solar radiation
Relative humidity

Metadata Handling
Metadata is retrieved via API calls to Envista and generated using the R tool build_envista_monitor_list.R 
(located in src/r/data_processing). It is cross-referenced with recorded information from:
📄 data/reference/siteXepaid_crosstable.csv

The database also includes:

Datetime variables
Spatial data: Latitude & longitude, regions, census classifier
Site information: City, county, address, full site name, short site name (3-character code), station tag
This structured dataset can be connected to Power BI, allowing users to analyze and visualize air quality metrics 
dynamically across different regions and time periods.

Daily Database (Daily DB)
Does NOT use AQI-related endpoints to pull daily data from Envista or AQS API calls.
Aggregates and calculates daily averages from the Hourly DB
Health categories and AQI levels are calculated using updated breakpoints
Standard procedures are followed when missing observations exceed 6 hours

Wildfire flagging:
Smoke levels (Light, Medium, Heavy) are derived from HMS data
If 24-hour avg PM₂.₅ ≥ 35 µg/m³ and the smoke level is Light, Medium, Heavy, or NA → the wildfire flag is applied
This processed data can be visualized in Power BI for better decision-making and reporting.

Hourly Database (Hourly DB)
A 3-hour moving average window is used to flag hours where:
Avg PM₂.₅ concentration > 15 µg/m³
By leveraging Power BI, users can explore hourly trends, anomalies, and historical comparisons in an interactive dashboard format.




Hourly data retrieval:
The scripts get_aqs_data.R and get_envista_data.R standardize and simplify API calls by aggregating 1-hour values
from Envista and EPA's AQS API.

These scripts work for all sites and specified time periods, but currently support only one parameter at a time.

The processed data can then be displayed on Power BI dashboards to facilitate analysis. The current version does NOT
include visualization. However, there are several R tools available in RATSorganizing that can connect with this database,
including those developed for ozone analysis.


📌 Features
✔ Standardized Data Output

Returns data in a uniform format
Manages data compilation and formatting from AQS & Envista, making it analysis-ready
✔ Envista & AQS Data Comparison

Enables direct comparison of AQS & Envista datasets:
in the hourly database, both extracted values from aqs and ENvista are saved under the 
"sample_measurement_aqs" & "sample_measurement_envista"

Helps verify data integrity and confirm correct data loading
✔ Unified API Calls

Standardizes parameters, sites, and time periods for AQS & Envista
Reduces confusion and errors due to API differences
✔ Enhanced Data Retrieval

Supports data sources like SensOR, available in Envista but not AQS
Enables access to historical data (pre-2018), which is missing in Envista
✔ Power BI Integration

Allows users to query, visualize, and analyze air quality data dynamically
Generates real-time and historical insights into air pollution trends
Customizable dashboards for different regions and pollutants

📌 Key Challenges in Merging AQS & Envista Data
These scripts tackle critical issues when merging two distinct air quality databases:

1️⃣ Column Name & Data Type Differences

AQS & Envista use different column names and data types (e.g., character vs. numeric values).
The Envista-to-AQS data mapping can be found in the "analyteXcode.csv" file located at:

📂 C:\Users\nkhosra\Documents\GitHub\AirDataTeam\SOAR_V1\data\reference


It’s important to note that ROS tables need to be maintained, especially when database changes occur, as this can 
significantly impact data consistency and integration. If any modifications are made to the database structure, 
we should ensure the mapping is updated accordingly.


The pollutant crosstable is called analyteXcode.csv and it provides the link between the R sript name for the pollutant,
the envista name for pollutant, and the aqs name for the pollutant.
The crosstable has the header:
name = this is the name/handle that will be used in the R script for the pollutant	
EnvistaName = this is the name in the envista database that is used for getting data with the api	
Alias = this is another name in the envista database that is used for getting data with the api	- not currently working
AqsName	= the is the aqs parameter code (eg. 88502 for pm25est; a complete list of parameter codes that are used for the AqsName can be found on the AQS website - https://www.epa.gov/aqs/aqs-code-list)
method_code_filter = method code for aqs (eg. the neph is 771)

The user may need to fill out this table with the pollutant they need, providing data for each column in the header.

For example:

Name: The name used in the R script
EnvistaName: The name Envista uses for the pollutant
AqsName: The name AQS uses for the pollutant
Additionally, the user can call the parameter in main.R if it is already included in this table.


2️⃣ Time Conversions

AQS timestamps mark the beginning of the hour
Envista timestamps mark the end of the hour
The scripts align timestamps for consistency.
The timestamps in the hourly database align with AQS standards, meaning they are recorded at the beginning of each hour.
As a result, the hourly database starts at 00:00 and ends at 23:00 each day (needs code reviewers attention for further validation).

3️⃣ Metadata Differences in API Calls

AQS & Envista use different metadata structures in API queries
The scripts bridge these differences to standardize API calls.

4️⃣ Parameter Naming & Qualifiers

AQS & Envista use different parameter names/codes
Different metadata qualifiers (IDs or character codes) are needed for API calls
The scripts create a crosswalk between AQS & Envista parameters for consistency
These challenges are automatically handled in Power BI, allowing users to focus on insights rather than data processing.

📌 Power BI Dashboard Features
By connecting the database to Power BI, users can:

Query AQI levels dynamically across different time periods and locations
 AQS and Envista datasets
Generate visual summaries of pollutant trends, wildfire impacts, and air quality levels
Monitor real-time data from API sources
📌 Data Collection
The scripts pull data for specific parameters listed in:
📄 data/reference/analyteXcode.csv

📌 Miro Board Access
📌 See the Miro board using the following link:
🔗 Miro Board

📌 If you don’t have access, please let me know (Neda).

A screenshot of the board section prepared for code review is available in:
📄 docs/R Toolchain Overview.docx

📌 Code Query Requests
Users can request hourly & daily AQI data from both AQS & Envista APIs using:
📂 Path: src/r/utils/Forvalidation_AQS_Envista_API.R





