# Airline Performance Analysis 🛫📊

## Overview
This project analyzes flight data in the United States from 1987 to 2008 using R programming language, specifically utilizing `dplyr` for data manipulation and `ggplot` for data visualization.

## Project Goals
The main objectives of this analysis are to address the following questions:

1. Are there significantly more flights in any particular month? 📅
2. In which month do we observe the highest delays and flight cancellations? What are the factors influencing them? ⏰❌
3. What were the major airlines over the years, and how did they evolve? ✈️📈
4. Are there greater delays on longer flights? ✈️⏰
5. Does the hub and spoke travel model diminish over the years, giving way to the Point to Point system? 🔄
6. Can we detect cascading failures, where delays at one airport cause delays at others? 🤔🔗
7. Do newly established airlines opt for new fleets, or is it the domain of traditional carriers? 🆕✈️
8. What were the most popular airlines in the USA over the years? 🥇🇺🇸
9. What was the impact of the September 11, 2001 attacks on American carriers? 🚨🛫
10. Where do New Yorkers fly to? Does the list of top destinations change depending on the choice of airport serving the New York metropolitan area? 🗽✈️🌎

## Data Sources
The analysis is based on flight data from the United States Department of Transportation for the specified time period.

## Technologies Used
- **R**: Programming language used for data analysis.
- **dplyr**: R package for data manipulation.
- **ggplot**: R package for data visualization.
- **shiny**: R package for interactive web applications.

## Structure
The project includes the following files:
- `generate.R` and `generate2.R`: R scripts containing the code for data analysis and visualization.
- `airlines_most_flights.csv`: CSV file containing the data for Shiny App.
- `app.R`: Shiny App presenting most popular carriers over years.
- `prezentacja.pdf`: report from the project as a presentation in Polish.
- `README.md`: This file, providing an overview of the project, its goals, and the technologies used.

## How to Use
1. Clone the repository to your local machine.
2. Open `generate.R`, `generate2.R` and `app.R` in RStudio or any text editor supporting R scripts.
3. Run the script to perform the analysis and generate visualizations.
4. Modify the script or datasets as needed for further exploration.

## Contributors
This project was developed by [@Gaspar Sekula](https://github.com/GasparSekula) and [@Kacper Rodziewicz](https://github.com/kacperrodziewicz8814), as part of Data Science Engineering Bachelor Programme at Warsaw University of Technology.


