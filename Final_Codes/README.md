# Code Contents

This folder contains the following files:

- **Amazon_RCode**: R code used for data cleaning from Kaggle. You can find the [original dataset](https://www.kaggle.com/datasets/arvindkhoda/boat-earbudsamazon).

- **Boat_RCodes**: R code for scraping and cleaning data from 17 categories on the official [boAt website](https://www.boat-lifestyle.com).

- **Python_URL_Scrapping.ipynb**: Python code used to obtain URLs for all products on the website, categorized accordingly.


## Web Scraping Approach

### Utilization of Selenium
The decision to employ Selenium for web scraping was motivated by the dynamic nature of the target website. Traditional scraping methods were insufficient due to the website's dependence on JavaScript for content rendering. Additionally, RSelenium was considered but found to be outdated and less suitable for the task.

### Dynamic Content Loading
During the scraping process, a notable observation was made regarding the website's dynamic content loading mechanism. Specifically, a loader was present at approximately 3.5 times the browser's height from the bottom of the page. This loader introduced a delay of 3-5 seconds to load additional products. Selenium provided the necessary functions to effectively handle these dynamic loading behaviors.

### Data Collection and Cleaning
After scraping the URLs for each category, the data was subsequently collected and cleaned using R codes. These R codes are available in this repository. The cleaned data is organized and made available for further analysis.

### Amazon Dataset Comparison
In addition to scraping and cleaning data from the target website, an Amazon dataset from Kaggle was also scraped and cleaned. This dataset will be used for comparisons and analysis, as per the project's requirements.
