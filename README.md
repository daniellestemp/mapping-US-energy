# mapping-US-energy
The repository includes relevant materials for geocoding data in R and mapping geocoded data in ArcGIS. This is part of an ongoing project for my GIS course at Pratt Institute, so this space will be updated as needed.

While companies like Microsoft and Amazon have made claims that energy prices won’t rise near data centers, other groups have published research stating otherwise. As part of a wider effort to understand these contradictions and the energy costs of data centers, I explored residential electricity costs in the United States from 2020-2025.

## Documentation & Workflow

### EIA Data Tools
Using US Energy Information Administration’s (EIA) web data tools, I queried data including average electricity price (cents per kilowatt hour (kWh)) by state from 2020-2025. 
I downloaded the dataset and performed initial data cleaning (such as cleaning up column names and ensuring state name formatting was consistent) in Google Sheets. 

### Geocoding & Calculated Columns in R
Next, I loaded the dataset into R (see link to file above) to geocode the data using libraries including tidyverse, sf, and tigris.
I created two calculated columns: total average price and price volatility. Volatility was measured by calculating the standard deviation of the average energy price for each state – the volatility values are in cents per kWh and represent how much a state’s electricity price typically deviated from its own average across 2020-2025. 

### Mapping in ArcGIS
I loaded the geocoded energy price dataset in ArcGIS and created maps of average energy price by state for each year 2020-2025. 
I then created separate maps for 1) overall average energy price by state 2020-2025 and 2) energy volatility by state. 
My final visual consists of price volatility as the main map, and average price maps for each year as an inset. 
