# 🛢️ Drill Baby Drill: Fossil Fuel Dashboard

This interactive **Shiny dashboard** explores global fossil fuel production, consumption, and climate policy. Built in R, the dashboard is inspired by the political slogan *"Drill, Baby, Drill"* and aims to critically examine the role of major producers—especially the United States—in shaping global energy trends and transition responsibilities.

https://drillbabydrill-datavisualization.shinyapps.io/DrillBabyDrill/


---

## 🔍 Project Overview

This dashboard integrates several datasets to offer a engaging and informative view of fossil fuel dynamics across countries. Users can explore key patterns and tensions between economic dependence on fossil fuels and global climate policy goals.

### ✨ Key Features

- **📈 Historical Trends**  
  Visualize oil and gas production for top global producers from 1980–2023.

- **🗺️ Production  Consumption Map**  
  Map of the top 50 oil and gas producers and consumers in 2023, highlighting trade status (exporter/importer) and imbalances.

- **💰 Fossil Fuel Rent**  
  Bar chart ranking countries by their fossil fuel rent to show economic dependence on extraction.

- **📘 Climate Policy Heatmap**  
  Explore fossil fuel mentions across different types of climate policy documents.

- **🔑 Keyword Network**  
  Network graph showing how fossil fuel terms co-occur in global climate documents.

- **🌎 Carbon Intensity vs GDP**  
  Scatterplot of carbon intensity per barrel of oil equivalent vs GDP per capita.

---

## 📦 Data Sources

- [U.S. Energy Information Administration (EIA)](https://www.eia.gov/)
- [Climate Change Laws of the World (CCLAWS)](https://climate-laws.org/)
- Fossil Fuel Rent Registry (aggregated from international sources)
- [Natural Earth shapefiles](https://www.naturalearthdata.com/) for mapping

---

## 🧑‍💻 Getting Started

###  Step 1: Clone the Repository


git clone https://github.com/your-username/fossil-fuel-dashboard.git
cd fossil-fuel-dashboard

###  Step 2: Prepare Data Files
Ensure the following files are placed in a local data/ folder:

EIA_Petroleum_by_country.xlsx

EIA_Gas_by_country.xlsx

EIA_Petroleum_consumption_country.xlsx

EIA_Gas_Consumption_country.xlsx

FFRegistry_Rent_GDP.xlsx

FFRegistry_Carbon Intensity.xlsx

CCLAWS_Text_Analysis.csv

### Step 3: Install Required R Packages
Open R or RStudio and run:


install.packages(c(
  "shiny", "ggplot2", "readxl", "dplyr", "readr", "tidyr", "leaflet", 
  "forcats", "ggrepel", "plotly", "collapsibleTree", "stringr", "scales", 
  "visNetwork", "tidytext", "igraph", "ggraph", "rnaturalearth", 
  "rnaturalearthdata", "sf", "rlang", "gridExtra"
))

### Step 4: Run the Dashboard

shiny::runApp()
Once launched, navigate to http://127.0.0.1:xxxx in your browser (R will print the exact port) to interact with the dashboard.

🎓 About
This project was developed for the Data Visualization course at Columbia Business School. It connects themes from energy economics, climate policy, and global trade into a visual narrative that empowers exploration and debate.

📬 Contact
If you have any questions, feedback, or would like to collaborate:

📨 Email: your.email@columbia.edu

🔗 LinkedIn: linkedin.com/in/yourprofile

🐙 GitHub Issues: Submit an Issue

📄 License
This project is licensed under the MIT License. You are free to use, modify, and distribute it with attribution.

Let me know if you'd like help generating a screenshot or customizing the license section!
