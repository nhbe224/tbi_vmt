# Welcome!
Hello! If you're reading this, you are at the repository for the paper ***Rebounds are half the game: estimating total and rebound effects of hybrid and always remote work on weekly VMT*** by Neeco Beltran and Greg Erhardt.

## Data
- **Travel Survey**
  - This project uses 2019, 2021, and 2023 data from the Metropolitan Council's Travel Behavior Inventory Household Travel Survey. For access to the data, please email Jonathan Ehrlich (Jonathan.Ehrlich@metc.state.mn.us).
- **Built-Environment Data**
  - The built environment variables used in this project are housing unit density per square mile of an individual's 2010 home block group and non-auto intersection density per square mile of an individual's 2010 home block group. These data come from the Environmental Protection Agency's Smart Location Database, [which can be accessed here](https://www.epa.gov/smartgrowth/smart-location-mapping).

## File/Code Structure
- **Step 1: Data Cleaning (datacleaning)**
   - `getvmt.R` code to read in raw survey data and links it together across years.
   - Also joined with the built-environment data (see above).
   - *Important: Users should change file paths in the code when appropriate to read in both travel survey and built-environment data.*
- **Step 2: Exploratory Analysis (exploratory_analysis)**
   - Contains code to read in cleaned data from step 1.
   - `exploratory_analysis.R` charts from data and generates summary statistics.
- **Step 3: Statistical Analysis (statistical_analysis)**
   - Contains code to perform ordered probit switching regression model.
   - `statistical_analysis.R` **contains the preferred specification and replicates what's currently in the paper.**
   - `archive.R` contains older/exploratory test work. 

