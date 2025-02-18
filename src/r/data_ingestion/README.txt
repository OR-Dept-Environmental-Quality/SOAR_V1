📌 Overview
The scripts get_aqs_data.R and get_envista_data.R standardize and simplify API calls by using API endpoints to
aggregate 1-hour values from both Envista and EPA's AQS API. These scripts work for all sites and specified time periods,
but currently support only one parameter at a time.

📌 Features
✔ Standardized Data Output
Returns data in a uniform format, handling data compilation and formatting from two different source of data AQS & Envista,
and making it ready for analysis and 
figure generation without additional processing.

✔ Envista & AQS Data Comparison
Enables direct comparison of Envista and AQS datasets to verify data integrity and ensure correct data loading.

✔ Unified API Calls
Standardizes parameters, sites, and time periods between Envista and AQS to simplify API queries.
Reduces user confusion and potential errors due to API difference.

✔ Enhanced Data Retrieval
This approach allows retrieving data sources like SensOR, which exist in Envista but not AQS, and enables access to
historical data prior to 2018, which is not available in Envista.


📌 Key Challenges in Merging AQS and Envista Data
These scripts address critical challenges in merging two distinct air quality databases:

1️⃣ Column Name & Data Type Differences
AQS and Envista use different column names and data types (e.g., character vs. numeric values).
2️⃣ Time Conversions
AQS timestamps represent the beginning of the hour, whereas Envista timestamps mark the end of the hour.
The scripts adjust timestamps to align data across both sources.
3️⃣ Metadata Differences in API Calls
Envista and AQS use different metadata structures to run API queries.
The scripts bridge these differences to standardize API calls.
4️⃣ Parameter Naming & Qualifiers
-AQS and Envista use different parameter names and codes for API calls.
-Different metadata qualifiers (e.g., IDs or character codes) are required for both systems.
-The scripts create a crosswalk between AQS and Envista parameters for consistency.


📌 Data Collection
The scripts collect data for specific parameters listed in:
📄 data/reference/analyteXcode.csv

📌 Load Links (load_links.R)
The script load_links.R creates a cross-table using all CSV files maintained and updated in:
📄 data/reference/

📂 Reference Data Files
File Name	Purpose
i. analyteXcode.csv	Cross-links AQS parameter codes and Envista parameter names to standardize API calls.
ii. envista_api_qualifier_code.csv	Merges qualifiers to facilitate quick data cleaning of the output, categorizing them
under the simple_qualifier column.
iii. siteXpaid_crosstable.csv	Unifies and standardizes metadata for running API calls in both AQS & Envista.



📌 Higher-Level Considerations
Should AQS and Envista Databases Be Combined?
Both get_aqs_data.R and get_envista_data.R return unraveled/unpacked output from hourly API endpoints.
The scripts return data in a standardized format but do not save raw data.
Further discussion is needed to determine if a single merged database is ideal or if another data management approach should be used.