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

# --- Load Data ---
EIA_petroleum      <- read_excel("C:/Users/asus/Downloads/EIA_Petroleum_by_country.xlsx")
EIA_gas            <- read_excel("C:/Users/asus/Downloads/EIA_Gas_by_country.xlsx")
EIA_petroleum_c    <- read_excel("C:/Users/asus/Downloads/EIA_Petroleum_consumption_country.xlsx")
EIA_gas_c          <- read_excel("C:/Users/asus/Downloads/EIA_Gas_consumption_country.xlsx")
Rent               <- read_excel("C:/Users/asus/Downloads/FFRegistry_Rent_GDP.xlsx")
carbon_data        <- read_excel("C:/Users/asus/Downloads/FFRegistry_Carbon Intensity.xlsx")
climate_policies   <- read.csv("C:/Users/asus/Downloads/CCLAWS_Text_Analysis.csv")

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
  titlePanel("📊 Drill Baby Drill: Fossil Fuel Dashboard"),
  tabsetPanel(
    tabPanel("1. Historical Trends", plotOutput("prod_plot")),
    tabPanel("2. Production vs Consumption Map", leafletOutput("prod_map")),
    tabPanel("3. Fossil Fuel Rent", plotlyOutput("rent_plot")),
    tabPanel("4. Climate Policy Heatmap", plotlyOutput("policy_heatmap")),
    tabPanel("5. Keyword Network", visNetworkOutput("word_network")),
    tabPanel("6. Carbon Intensity vs GDP", plotOutput("carbon_plot"))
  )
)

# --- Server ---
server <- function(input, output, session) {
  output$prod_plot <- renderPlot({
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
        Highlight = ifelse(Country == "United States", "U.S.", "Other")
      )
    
    labels_2023 <- oil_gas_top10 %>%
      filter(Year == 2023 & !is.na(Total_Production))
    
    ggplot(oil_gas_top10, aes(x = Year, y = Total_Production, group = Country)) +
      geom_line(aes(color = Highlight, alpha = Highlight), size = 1) +
      scale_color_manual(values = c("U.S." = "#800", "Other" = "grey70")) +
      scale_alpha_manual(values = c("U.S." = 1, "Other" = 0.5)) +
      scale_x_continuous(limits = c(1980, 2025), breaks = seq(1980, 2023, by = 5), expand = c(0, 0)) +
      geom_text_repel(data = labels_2023, aes(label = Country, color = Highlight), hjust = 0, nudge_x = 1.5,
                      direction = "y", segment.color = NA, size = 3.5) +
      coord_cartesian(clip = "off") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", plot.margin = margin(5.5, 40, 5.5, 5.5)) +
      labs(
        title = "Top 10 Oil & Gas Producers (1980–2023)",
        subtitle = "U.S. highlighted in red, others in grey",
        x = "Year", y = "Total Production (Million Barrels of Oil Equivalent)"
      )
  })
  
  # Placeholder for other plots
  # --- Server Section: output$prod_map ---
  output$prod_map <- renderLeaflet({
    leaflet(map_top50) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(Total_Consumption) / 6,
        color = NA,
        fillColor = "#3A6C74",
        fillOpacity = 0.8
      ) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(Total_Production) / 6,
        color = "black",
        fillOpacity = 0,
        weight = 3
      ) %>%
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
