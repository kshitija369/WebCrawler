There are two R Scripts and one Zillow_IDs.txt file for Zillow Project:
1. Zillow_IDs.txt
2. Util.R
3. Zillow_web_crawler.R

Zillow_ID’s.txt:
- This file contains the Zillow ID’s for each webpage to be scraped.

Util.R:
- In Util.R starts Selenium server
- It reads Zillow IDs from Zillow_IDs.txt and creates a dataframe of Zillow URL's
- It creates a zillow_data.csv file which contains all the url's from which data is to be extracted

Zillow_web_crawler.R:
- This file loops through url’s in zillow_data.csv and in each iteration we extract data from required portion of webpage.