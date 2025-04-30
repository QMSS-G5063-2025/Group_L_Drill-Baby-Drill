# --- Load Libraries ---
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(forcats)
library(ggrepel)
library(plotly)
library(collapsibleTree)
library(stringr)
library(scales)
library(visNetwork)
library(tidytext)
library(igraph)
library(ggraph)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

##"C:/Users/asus/Desktop/MSAFA/Data Visualization/Group_L_Drill-Baby-Drill/2nd app/app/CCLAWS_Text_Analysis.csv"
## "C:\Users\asus\Downloads\CCLAWS_Text_Analysis.csv"

# --- Load Data ---
# Production data
EIA_petroleum <- read_excel("data/EIA_Petroleum_by_country.xlsx")
EIA_gas <- read_excel("data/EIA_Gas_by_country.xlsx")

# Consumption data
EIA_petroleum_c <- read_excel("data/EIA_Petroleum_consumption_country.xlsx")
EIA_gas_c <- read_excel("data/EIA_Gas_Consumption_country.xlsx")

# Fossil fuel rent and GDP
Rent <- read_excel("data/FFRegistry_Rent_GDP.xlsx")

# Carbon intensity data
carbon_data <- read_excel("data/FFRegistry_Carbon Intensity.xlsx")


## Preprocessing for production data (for Chart 1)
petroleum_clean <- EIA_petroleum %>%
  filter(`...3` == "Crude oil, NGPL, and other liquids (Mb/d)") %>%
  select(-`...3`) %>%
  rename(Code = 1, Country = 2) %>%
  mutate(across(where(is.numeric), ~ .x * 365))

gas_clean <- EIA_gas %>%
  mutate(across(where(is.numeric), ~ .x * 0.167))

# Convert all column names to character first (to prevent type conflicts)
colnames(petroleum_clean) <- as.character(colnames(petroleum_clean))
colnames(gas_clean) <- as.character(colnames(gas_clean))

# Convert year columns to numeric if possible (ensures consistency)
petroleum_clean <- petroleum_clean %>%
  mutate(across(matches("^[0-9]{4}$"), as.numeric))

gas_clean <- gas_clean %>%
  mutate(across(matches("^[0-9]{4}$"), as.numeric))

# Pivot longer
petroleum_long <- petroleum_clean %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "Year",
    values_to = "Oil_Production"
  ) %>%
  mutate(Year = as.numeric(Year))

gas_long <- gas_clean %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "Year",
    values_to = "Gas_Production"
  ) %>%
  mutate(Year = as.numeric(Year))

# Merge and clean
oil_gas_long <- full_join(petroleum_long, gas_long, by = c("Country", "Year")) %>%
  mutate(across(c(Oil_Production, Gas_Production), as.numeric),
         Total_Production = coalesce(Oil_Production, 0) + coalesce(Gas_Production, 0)) %>%
  filter(Year >= 1980 & Year <= 2023,
         !Country %in% c("World", "OECD", "Non-OECD"))

# Adjust for Russia/Former USSR
ussr_russia <- oil_gas_long %>%
  filter(Country == "Former U.S.S.R.", Year %in% 1980:1991) %>%
  mutate(Country = "Russia")

oil_gas_long <- oil_gas_long %>%
  filter(!(Country == "Russia" & Year %in% 1980:1991)) %>%
  bind_rows(ussr_russia) %>%
  filter(!(Country == "Russia" & Year == 1992 & (is.na(Total_Production) | Total_Production == 0)),
         !(Country == "Former U.S.S.R." & (Year > 1992 | is.na(Total_Production) | Total_Production == 0)))


## Preprocessing for production + consumption map data (for Chart 2)
petroleum_c_long <- EIA_petroleum_c %>%
  rename(Code = 1, Country = 2) %>%
  mutate(across(where(is.numeric), ~ . * 365)) %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "Year", values_to = "Oil_Consumption") %>%
  mutate(Year = as.numeric(Year))

gas_c_long <- EIA_gas_c %>%
  rename(Code = 1, Country = 2) %>%
  mutate(across(where(is.numeric), ~ . * 0.167)) %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "Year", values_to = "Gas_Consumption") %>%
  mutate(Year = as.numeric(Year))

oil_gas_c_long <- full_join(petroleum_c_long, gas_c_long, by = c("Country", "Year")) %>%
  mutate(
    Oil_Consumption = as.numeric(Oil_Consumption),
    Gas_Consumption = as.numeric(Gas_Consumption),
    Total_Consumption = coalesce(Oil_Consumption, 0) + coalesce(Gas_Consumption, 0)
  ) %>%
  filter(!Country %in% c("World", "OECD", "Non-OECD"))

prod_con <- full_join(oil_gas_long, oil_gas_c_long, by = c("Country", "Year")) %>%
  select(-Code.x, -Code.y)

prod_con_2023 <- prod_con %>%
  filter(Year == 2023) %>%
  select(Country, Oil_Production, Gas_Production, Total_Production,
         Oil_Consumption, Gas_Consumption, Total_Consumption)

world <- ne_countries(scale = "medium", returnclass = "sf")

map_data_merged <- world %>%
  left_join(prod_con_2023, by = c("name_long" = "Country"))

top50_producers <- map_data_merged %>% arrange(desc(Total_Production)) %>% slice_head(n = 50)
top50_consumers <- map_data_merged %>% arrange(desc(Total_Consumption)) %>% slice_head(n = 50)

map_top50 <- bind_rows(top50_producers, top50_consumers) %>%
  distinct(name_long, .keep_all = TRUE) %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )

map_top50 <- map_top50 %>%
  mutate(
    trade_status = case_when(
      Total_Production > Total_Consumption ~ "Net Exporter",
      Total_Production < Total_Consumption ~ "Net Importer",
      TRUE ~ "Balanced"
    ),
    popup_info = paste0(
      "<b>", name_long, "</b><br>",
      "🛢️ Production: ", round(Total_Production, 2), " MBOE<br>",
      "🔥 Consumption: ", round(Total_Consumption, 2), " MBOE<br>",
      "🔁 Status: <b>", trade_status, "</b>"
    )
  )


top10_labels <- map_top50 %>%
  arrange(desc(Total_Production)) %>%
  slice_head(n = 10) %>%
  mutate(
    lon_label = lon + ifelse(lon > 0, 5, -5),
    lat_label = lat + ifelse(lat > 0, 2, -2),
    lon_label = case_when(
      name_long == "United Arab Emirates" ~ lon + 5,
      name_long == "Iran" ~ lon + 3,
      name_long == "Saudi Arabia" ~ lon - 5,
      TRUE ~ lon_label
    ),
    lat_label = case_when(
      name_long == "United Arab Emirates" ~ lat + 2,
      name_long == "Iran" ~ lat - 2,
      name_long == "Saudi Arabia" ~ lat + 2,
      TRUE ~ lat_label
    )
  )


## Preprocessing for Fossil Fuel Rent data (for Chart 3)
top_20_rent <- Rent %>%
  group_by(Country) %>%
  summarise(Rent = sum(`Rent (USD)`, na.rm = TRUE)) %>%
  arrange(desc(Rent)) %>%
  slice_head(n = 20) %>%
  mutate(color = ifelse(Country == "United States", "#800", "#346674"))


## Preprocessing for climate policy data (for Chart 4&5)

# Normalize column names
colnames(climate_policies) <- make.names(colnames(climate_policies))

# Detect fossil fuel mentions
climate_policies <- climate_policies %>%
  mutate(
    fossil_fuel_mention = if_else(
      str_detect(Family.Summary, regex("fossil fuel|oil|gas|coal|petroleum|natural gas", ignore_case = TRUE)),
      1, 0
    )
  )

# Version 1: Tree structure
country_mentions <- climate_policies %>%
  group_by(Geographies) %>%
  summarise(total_mentions = sum(fossil_fuel_mention, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(commitment_level = case_when(
    total_mentions == 0 ~ "No Commitment",
    total_mentions <= 2 ~ "Low Commitment",
    total_mentions <= 5 ~ "Medium Commitment",
    total_mentions > 5 ~ "High Commitment"
  ))

climate_policies <- climate_policies %>%
  mutate(doc_strength = case_when(
    Document.Type %in% c("Regulation", "Order", "Decree", "Law") ~ 1,
    Document.Type %in% c("Framework", "Policy Report", "Discussion", "Summary") ~ -1,
    TRUE ~ 0
  ))

country_color_score <- climate_policies %>%
  filter(fossil_fuel_mention == 1) %>%
  group_by(Geographies) %>%
  summarise(avg_strength = mean(doc_strength, na.rm = TRUE)) %>%
  mutate(color_value = (avg_strength + 1) / 2,
         color = rgb(1 - color_value, color_value, 0))  # Red to green

network_data <- country_mentions %>%
  left_join(country_color_score, by = "Geographies") %>%
  filter(!is.na(commitment_level))

low_medium_high <- network_data %>%
  filter(commitment_level != "No Commitment") %>%
  mutate(root = "Fossil Fuel Policy") %>%
  select(root, commitment_level, country = Geographies, color)

no_commitment <- network_data %>%
  filter(commitment_level == "No Commitment") %>%
  mutate(root = "Fossil Fuel Policy") %>%
  summarise(
    root = first(root),
    commitment_level = "No Commitment",
    country = paste(n(), "countries with no commitment"),
    color = NA
  )

tree_data <- bind_rows(low_medium_high, no_commitment) %>%
  mutate(fill_color = ifelse(is.na(color), NA, color))

# Version 2: Heatmap
summary_mentions <- climate_policies %>%
  group_by(Geographies, Document.Type) %>%
  summarise(
    total_policies = n(),
    fossil_fuel_policies = sum(fossil_fuel_mention, na.rm = TRUE),
    fossil_fuel_share = fossil_fuel_policies / total_policies
  ) %>%
  ungroup()

country_totals <- summary_mentions %>%
  group_by(Geographies) %>%
  summarise(total_fossil_share = sum(fossil_fuel_share, na.rm = TRUE)) %>%
  arrange(desc(total_fossil_share))

document_totals <- summary_mentions %>%
  group_by(Document.Type) %>%
  summarise(total_fossil_share = sum(fossil_fuel_share, na.rm = TRUE)) %>%
  arrange(desc(total_fossil_share))

summary_mentions <- summary_mentions %>%
  mutate(
    Geographies = fct_reorder(Geographies, country_totals$total_fossil_share[match(Geographies, country_totals$Geographies)]),
    Document.Type = fct_reorder(Document.Type, document_totals$total_fossil_share[match(Document.Type, document_totals$Document.Type)])
  )

fossil_bigrams <- climate_policies %>%
  unnest_tokens(bigram, Family.Summary, token = "ngrams", n = 2) %>%
  filter(str_detect(bigram, "(?i)fossil|fuel|coal|gas|oil|petroleum")) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word)

bigram_counts <- fossil_bigrams %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n >= 5)

# Create igraph object
word_graph <- graph_from_data_frame(bigram_counts, directed = FALSE)

# Add node attributes
word_counts <- fossil_bigrams %>%
  pivot_longer(cols = c(word1, word2), values_to = "word") %>%
  count(word, sort = TRUE)

V(word_graph)$word_count <- word_counts %>%
  filter(word %in% V(word_graph)$name) %>%
  arrange(match(word, V(word_graph)$name)) %>%
  pull(n)

V(word_graph)$centrality <- degree(word_graph)

# Prepare nodes and edges for visNetwork
nodes <- data.frame(
  id = V(word_graph)$name,
  label = V(word_graph)$name,
  value = V(word_graph)$word_count,
  group = ifelse(V(word_graph)$centrality > median(V(word_graph)$centrality), "High", "Low"),
  title = paste0("<p><b>", V(word_graph)$name, "</b><br>Mentions: ", 
                 V(word_graph)$word_count, "<br>Degree: ", 
                 V(word_graph)$centrality, "</p>")
)

edges <- bigram_counts %>%
  rename(from = word1, to = word2, value = n)


## Preprocessing for Carbon Intensity vs GDP Data (for Chart 6)

# Filter and aggregate carbon intensity
carbon_filtered <- carbon_data %>%
  filter(GWP == "GWP100", Gas %in% c("CO2", "Methane")) %>%
  group_by(Country) %>%
  summarise(carbon_intensity = sum(`kg CO2e / boe`, na.rm = TRUE)) %>%
  ungroup()

# Process GDP per capita from rent data
rent_gdp <- Rent %>%
  filter(Fuel %in% c("Oil", "Gas")) %>%
  group_by(Country) %>%
  summarise(gdp_capita = sum(`GDP per capita`, na.rm = TRUE)) %>%
  ungroup()

# Harmonize country names
country_mapping <- tibble::tribble(
  ~carbon_country,              ~standard_country,
  "United States of America",   "United States",
  "Russia",                     "Russian Federation"
)

carbon_filtered <- carbon_filtered %>%
  left_join(country_mapping, by = c("Country" = "carbon_country")) %>%
  mutate(Country = if_else(is.na(standard_country), Country, standard_country)) %>%
  select(-standard_country)

# Merge data
carbon_merged_data <- carbon_filtered %>%
  inner_join(rent_gdp, by = "Country") %>%
  mutate(is_us = if_else(Country == "United States", "US", "Other"))





# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: url('oil_banner.jpg') no-repeat center center fixed;
        background-size: cover;
      }
    "))
  ),
  titlePanel("📊 Drill Baby Drill: Fossil Fuel Dashboard"),
  tabsetPanel(
    tabPanel("📘 Introduction",
             fluidPage(
               div(
                 style = "position: relative; background-color: rgba(255,255,255,0.9); padding: 30px; border-radius: 10px; font-family: Calibri;",
                 div(
                   style = "text-align: center; margin-bottom: 20px;",
                   img(src = "trump.jpg", width = "60%")
                 ),
                 h2("Introduction"),
                 tags$hr(style = "margin-top: 5px; border-top: 1px solid #aaa;"),
                 
                 tags$p(tags$b("To meet the Paris Agreement’s ambitious targets of limiting global warming to well below 2°C, and ideally to 1.5°C above pre-industrial levels,"), 
                        " it is imperative that countries drastically reduce both the consumption and production of fossil fuels."),
                 
                 tags$p("Yet, in the absence of coordinated global action, fossil fuel production tends to persist as long as there is demand, perpetuating a cycle of overproduction driven by market incentives that rarely align with climate goals."),
                 
                 tags$p(tags$b("The 2021 Glasgow Climate Pact marked a historic step by explicitly calling on nations to phase down coal use and to end new fossil fuel financing,"), 
                        " underscoring the urgent need to start limiting fossil fuel production at the source."),
                 
                 tags$p("Deciding which countries should reduce their oil and gas extraction more rapidly than others is a complex issue influenced by environmental, economic, and socio-political considerations."),
                 
                 tags$p(tags$b("The United States stands at the center of this challenge, being both the world’s largest fossil fuel producer and consumer.")),
                 
                 tags$p("The phrase ", tags$em('"drill, baby, drill"'), " was first popularized during the 2008 Republican National Convention by Michael Steele, advocating for increased domestic oil and gas production."),
                 
                 tags$p("In his 2025 campaign and inauguration speeches, President Donald Trump revived this slogan, signaling a renewed commitment to expanding fossil fuel extraction as a cornerstone of his energy policy. ",
                        tags$a(href = "https://thehill.com/policy/energy-environment/4243522-trump-drill-baby-drill-energy-plan/", target = "_blank", "The Hill reports"),
                        " that Trump pledged to end Biden’s climate policies and ramp up drilling activities if re-elected."),
                 
                 tags$p("Especially against this political backdrop, the U.S. must consider its global responsibility."),
                 
                 tags$p("This analysis delves into three distinct datasets capturing global fossil fuel dynamics to evaluate the country’s role and responsibilities within the international climate framework."),
                 
                 tags$p("For comprehensive and up-to-date data on U.S. oil and natural gas production, refer to the ",
                        tags$a(href = "https://www.eia.gov/petroleum/production/", target = "_blank", "U.S. Energy Information Administration (EIA) Monthly Crude Oil and Natural Gas Production"),
                        " page.")
               )
             )),
    
    tabPanel("📈 Historical Trends",
             fluidPage(
               div(
                 style = "position: relative; background-color: rgba(255,255,255,0.9); padding: 30px; border-radius: 10px; font-family: Calibri;",
                 tags$p(tags$b("In this first part, our aim is to establish the significance of the United States' oil and gas production in the world.")),
                 tags$p("We start by looking at U.S. production over time. We then move on to compare its production today relative to the rest of the world, and assess the country’s economic and policy-based positioning within the broader fossil fuel landscape."),
                 h4(tags$b("Historical Trends")),
                 tags$p(tags$b("This first chart shows the evolution of the top 10 oil and gas producers from 1980 to 2023"), ", measured in million barrels of oil equivalent."),
                 tags$p(tags$b("The United States, highlighted in red, maintained relatively stable production levels between 1980 and 2005."), " However, after 2005, U.S. production began to rise sharply, particularly following the shale oil and gas boom around 2010."),
                 tags$p(tags$b("By 2023, the United States far outpaced all other countries,"), " reaching nearly 60,000 million barrels of oil equivalent, significantly widening the gap with other major producers."),
                 tags$p(tags$b("Russia, shown in grey, had higher production than the U.S. in the late 1980s"), " but experienced a steep decline after the collapse of the Soviet Union around 1990. While it partially recovered, it never regained its previous dominance."),
                 tags$p(tags$b("Other countries such as Saudi Arabia, Iran, China, Canada, and Norway"), " show more gradual increases or relatively flat production over the period."),
                 tags$p(tags$b("This dramatic increase underscores the United States’ central role in global fossil fuel supply today and raises the question of whether it should also bear a greater responsibility in curbing production.")),
                 div(style = "text-align: center;", plotlyOutput("prod_plot")),
                 tags$p("U.S. highlighted in red, others in grey (Unit: Million Barrels of Oil Equivalent)", style = "text-align: right; font-style: italic; color: #555;"),
                 tags$p(paste("Others:", paste(c("Saudi Arabia", "Canada", "China", "United Arab Emirates", "Norway", "Iran", "Mexico"), collapse = " > ")), style = "text-align: right; font-style: italic; color: #555;")
               )
             )),
    tabPanel("🗺️ Production vs Consumption Map",
             fluidPage(
               div(
                 style = "position: relative; background-color: rgba(255,255,255,0.9); padding: 30px; border-radius: 10px; font-family: Calibri;",
                 h4(tags$b("Top 50 oil and gas consumption around the world in 2023")),
                 tags$p(tags$b("This map visualizes global oil and gas production and consumption."), " The United States clearly dominates both production and consumption, with a very large filled circle indicating its position as the world’s largest producer, alongside a prominent hollow circle showing its massive domestic energy demand."),
                 tags$p(tags$b("Other significant producers"), ", such as Canada, Norway, Algeria, Iran, the United Arab Emirates, and China, are marked by large filled circles, although their consumption patterns vary."),
                 tags$p(tags$b("China, notably, has a substantial hollow circle but a much smaller filled circle"), ", indicating that it consumes far more oil and gas than it produces domestically."),
                 tags$p(tags$b("Europe shows a concentration of hollow circles"), " with relatively few large filled ones, highlighting its dependence on imports."),
                 tags$p(tags$b("For the United States, being both a top consumer and a top producer"), " highlights the country’s unique leverage—and thus responsibility—in shaping global supply and demand. Its high consumption further complicates the case for leadership unless paired with meaningful internal transition efforts."),
                 leafletOutput("prod_map")
               )
             )),
    tabPanel("💰 Fossil Fuel Rent",
             fluidPage(
               div(
                 style = "position: relative; background-color: rgba(255,255,255,0.9); padding: 30px; border-radius: 10px; font-family: Calibri;",
                 tags$p(tags$b("This bar chart shows the top 20 countries ranked by their fossil fuel rent"), ", measured in U.S. dollars. Fossil fuel rent represents the economic value a country derives from extracting fossil fuels relative to the size of its economy."),
                 tags$p(tags$b("Iran, China, and Saudi Arabia top the list"), ", each with significantly higher fossil fuel rents compared to other countries."),
                 tags$p(tags$b("Iraq, Russia, Kuwait, and the United Arab Emirates also report high fossil fuel rents"), ", reflecting their heavy reliance on oil and gas extraction as a source of national income."),
                 tags$p(tags$b("Brazil appears just ahead of the United States"), ", which is highlighted in a different color (red) for emphasis."),
                 tags$p(tags$b("Despite being the world's largest oil and gas producer in absolute terms"), ", the United States ranks only mid-table here, indicating that fossil fuel rents make up a relatively smaller share of its broader economy."),
                 tags$p(tags$b("This distinction is important"), ": countries that are structurally dependent on fossil fuel revenues face much higher transition risks, whereas the U.S., with its diversified economy, may be better positioned to absorb the economic impacts of a managed phaseout."),
                 plotlyOutput("rent_plot")
               )
             )),
    tabPanel("📘 Climate Policy Heatmap",
             fluidPage(
               div(
                 style = "position: relative; background-color: rgba(255,255,255,0.9); padding: 30px; border-radius: 10px; font-family: Calibri;",
                 tags$p(tags$b("This heatmap illustrates the share of fossil fuel-related content in climate policy documents"), ", with each row representing a country or region and each column a different document type."),
                 tags$p(tags$b("The intensity of red shading indicates how much a given document focuses on fossil fuels"), "—darker reds signify a higher share, while white or pale shades indicate little to no mention."),
                 tags$p(tags$b("The concentration of red blocks on the right side"), " suggests that certain types of documents—likely newer or more targeted ones—are more focused on fossil fuels."),
                 tags$p(tags$b("Countries with dense vertical streaks of red"), " are engaging more consistently with fossil fuel issues across their policy documents, whereas countries with sparse or faint coloration show limited attention to the topic."),
                 tags$p(tags$b("For the United States, the degree of engagement in policy texts"), " suggests at least a rhetorical acknowledgment of the fossil fuel issue, although this does not necessarily translate into concrete production curbs."),
                 plotlyOutput("policy_heatmap")
               )
             )),
    tabPanel("🔑 Keyword Network",
             fluidPage(
               div(
                 style = "position: relative; background-color: rgba(255,255,255,0.9); padding: 30px; border-radius: 10px; font-family: Calibri;",
                 tags$p(tags$b("This network graph visualizes how fossil fuel-related terms co-occur within climate policy texts"), ", based on bigram analysis."),
                 tags$p(tags$b("Each node represents a keyword"), " (like ‘gas,’ ‘oil,’ or ‘coal’), and edges link words that frequently appear together in bigrams (e.g., \"gas supply\")."),
                 tags$p(tags$b("The size of each node reflects how often that word appears in the dataset"), " (its mention count), while its color indicates its degree centrality—that is, how many direct connections it has to other words."),
                 tags$p(tags$b("The term ‘gas’ is the most prominent node in the network"), ", with 2,375 mentions and a degree of 43, meaning it is directly connected to 43 other words."),
                 tags$p(tags$b("This suggests that gas is not only frequently mentioned but also widely associated with a diverse range of terms in climate policies.")),
                 tags$p(tags$b("The overall layout (likely a force-directed layout) clusters related terms"), ", revealing thematic groupings—such as technical terms around gas infrastructure or policy framing around fossil energy."),
                 tags$p(tags$b("This structure helps identify which fossil fuel terms are most central and how they interact with policy language.")),
                 visNetworkOutput("word_network")
               )
             )),
    tabPanel("🌎 Carbon Intensity vs GDP",
             fluidPage(
               div(
                 style = "position: relative; background-color: rgba(255,255,255,0.9); padding: 30px; border-radius: 10px; font-family: Calibri;",
                 tags$p(tags$b("This scatterplot compares countries based on their carbon intensity"), " (measured in kilograms of CO₂ equivalent per barrel of oil equivalent) and GDP per capita, focusing only on emissions from the oil and gas sectors under the GWP100 framework."),
                 tags$p(tags$b("The chart reveals a wide variation in carbon intensity among fossil fuel-producing countries."), " Nations like Iraq, Algeria, and Iran exhibit extremely high carbon intensity despite relatively low GDP per capita, highlighting inefficient or highly polluting extraction practices."),
                 tags$p(tags$b("In contrast, Norway and Saudi Arabia have some of the lowest carbon intensities"), ", suggesting cleaner production methods."),
                 tags$p(tags$b("The United States, marked in red"), ", stands out with the highest GDP per capita among peers but a moderate level of carbon intensity, positioning it as an outlier in terms of economic wealth relative to emissions performance."),
                 tags$p(tags$b("This visualization helps underscore the differing environmental footprints of fossil fuel rents across both developed and developing economies.")),
                 plotOutput("carbon_plot")
               )
             ))
  )
)

# --- Server ---
server <- function(input, output, session) {
  output$intro_ui <- renderUI({
    div(
      style = paste0(
        "position: relative; overflow: hidden; padding: 40px; min-height: 100vh;",
        "background: url('www/oil_banner.jpg') no-repeat center center fixed;",
        "background-size: cover;"
      ),
      div(
        style = "position: relative; background-color: rgba(255,255,255,0.5); padding: 30px; border-radius: 10px;",
        h2("Introduction"),
        tags$p(strong("To meet the Paris Agreement’s ambitious targets of limiting global warming to well below 2°C, and ideally to 1.5°C above pre-industrial levels, it is imperative that countries drastically reduce both the consumption and production of fossil fuels.")),
        tags$p("Yet, in the absence of coordinated global action, fossil fuel production tends to persist as long as there is demand, perpetuating a cycle of overproduction driven by market incentives that rarely align with climate goals."),
        tags$p("The 2021 Glasgow Climate Pact marked a historic step by explicitly calling on nations to phase down coal use and to end new fossil fuel financing, underscoring the urgent need to start limiting fossil fuel production at the source. This raises a critical and contentious question: which countries should take the lead in initiating production cuts?"),
        tags$p("Deciding which countries should reduce their oil and gas extraction more rapidly than others is a complex issue influenced by environmental, economic, and socio-political considerations. As the world confronts the urgent challenge of climate change, it is essential to establish clear criteria for equitable and effective transitions away from fossil fuel production."),
        tags$p(strong("The United States, as the world’s largest fossil fuel producer and consumer, presents a particularly significant case,"), "especially against the backdrop of recent political rhetoric championing increased extraction with slogans like ", em('"drill, baby, drill."'), "To understand whether the U.S. is positioned to spearhead reductions in fossil fuel production, this analysis delves into three distinct datasets capturing global fossil fuel dynamics, evaluating the country’s role and responsibilities within the international climate framework.")
      )
    )
  })
  
  output$prod_plot <- renderPlotly({
    # Top 10 countries
    top10_countries <- oil_gas_long %>%
      group_by(Country) %>%
      summarise(Total_Prod = sum(Total_Production, na.rm = TRUE)) %>%
      arrange(desc(Total_Prod)) %>%
      slice_head(n = 10) %>%
      pull(Country)
    
    oil_gas_top10 <- oil_gas_long %>%
      filter(Country %in% top10_countries) %>%
      mutate(
        Year = as.numeric(Year),
        Highlight = ifelse(Country == "United States", "U.S.",
                           ifelse(Country == "Russia", "Russia", "Other")),
        tooltip_text = paste0("Country: ", Country,
                              "<br>Year: ", Year,
                              "<br>Production: ", round(Total_Production, 2))
      )
    
    # Labels for U.S. and Russia
    inline_labels <- oil_gas_top10 %>%
      filter(Year == 2023 & Country %in% c("United States", "Russia")) %>%
      mutate(label_color = ifelse(Country == "United States", "#800", "grey40"))
    
    # Create plot
    p <- ggplot(oil_gas_top10, aes(x = Year, y = Total_Production, group = Country)) +
      geom_line(aes(color = Highlight, alpha = Highlight, text = tooltip_text), size = 1) +
      geom_text(
        data = inline_labels,
        aes(label = Country),
        hjust = 0, nudge_x = 1.5,
        color = inline_labels$label_color,
        size = 3.5
      ) +
      scale_color_manual(values = c("U.S." = "#800", "Russia" = "grey60", "Other" = "grey70")) +
      scale_alpha_manual(values = c("U.S." = 1, "Russia" = 0.8, "Other" = 0.5)) +
      scale_x_continuous(limits = c(1980, 2030), breaks = seq(1980, 2025, by = 5), expand = c(0, 0)) +
      coord_cartesian(clip = "off") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.margin = margin(5.5, 140, 40, 5.5)
      ) +
      labs(
        title = "Top 10 Oil & Gas Producers (1980–2023)",
        x = "Year",
        y = "Total Production"
      )
    
  })
  
  
  # Placeholder for other plots
  # --- Server Section: output$prod_map ---
  output$prod_map <- renderLeaflet({
    leaflet(map_top50) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      
      # Net consumption (fill)
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(Total_Consumption) / 6,
        color = NA,
        fillColor = "#3A6C74",
        fillOpacity = 0.8,
        popup = ~popup_info
      ) %>%
      
      # Net production (border)
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(Total_Production) / 6,
        color = "black",
        fillOpacity = 0,
        weight = 3,
        popup = ~popup_info
      ) %>%
      
      # Top 10 label annotations
      addLabelOnlyMarkers(
        data = top10_labels,
        lng = ~lon_label, lat = ~lat_label,
        label = ~name_long,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "auto",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-weight" = "bold",
            "font-size" = "12px",
            "background" = "white",
            "padding" = "2px 4px",
            "border-radius" = "4px",
            "box-shadow" = "2px 2px 2px rgba(0,0,0,0.1)"
          )
        )
      )
  })
  
  
  output$rent_plot <- renderPlotly({
    p <- ggplot(top_20_rent, aes(x = reorder(Country, Rent), y = Rent,
                                 text = paste0("Country: ", Country, "<br>Rent: ", comma(Rent)))) +
      geom_bar(stat = "identity", aes(fill = color), width = 0.7) +
      scale_fill_identity() +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = "Top 20 Countries by Fossil Fuel Rent",
        x = NULL,
        y = "Fossil Fuel Rent (USD)"
      ) +
      theme_minimal(base_family = "Arial") +
      theme(
        plot.title = element_text(family = "Arial", face = "bold", size = 18, hjust = 0),
        axis.text.y = element_text(size = 11, margin = margin(r = 15)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.margin = margin(20, 50, 20, 20),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text")
  })
  output$policy_heatmap <- renderPlotly({
    base_heatmap <- ggplot(summary_mentions, aes(x = Document.Type, y = Geographies, fill = fossil_fuel_share)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#800", na.value = "grey90") +
      labs(
        x = "Document Type",
        y = "Country / Region",
        title = "Heatmap of Fossil Fuel Focus in Climate Policies",
        subtitle = "Darker = Higher Share of Fossil Fuel Mentions",
        fill = "Fossil Fuel Share"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14)
      )
    
    ggplotly(base_heatmap)
  })
  
  output$policy_tree <- renderCollapsibleTree({
    collapsibleTree(
      tree_data,
      hierarchy = c("root", "commitment_level", "country"),
      fill = "fill_color",
      collapsed = FALSE,
      linkLength = 150,
      fontSize = 15
    )
  })
  
  
  output$word_network <- renderVisNetwork({
    visNetwork(nodes, edges, width = "100%", height = "800px") %>%
      visNodes(font = list(size = 20)) %>%
      visEdges(smooth = FALSE, color = list(color = "gray", highlight = "#3A6C74")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123)
  })
  
  output$carbon_plot <- renderPlot({
    ggplot(carbon_merged_data, aes(x = gdp_capita, y = carbon_intensity)) +
      geom_point(aes(color = is_us), size = 3.5, alpha = 0.8) +
      geom_text_repel(aes(label = Country), size = 4, max.overlaps = 30) +
      scale_color_manual(values = c("US" = "#800", "Other" = "gray")) +
      labs(
        x = "GDP per capita (USD)",
        y = "Carbon Intensity (kg CO₂e / boe)",
        title = "Carbon Intensity vs Fossil Fuel Rent",
        subtitle = "Only CO2 and Methane gases under GWP100, Oil and Gas sectors",
        color = NULL
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "none"
      )
  })
  
}

# --- Run App ---
shinyApp(ui = ui, server = server)
