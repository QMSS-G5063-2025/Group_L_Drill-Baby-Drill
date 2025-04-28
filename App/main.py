# --- Install & Import packages ---
import sys
import subprocess

def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

required_packages = ['pandas', 'numpy', 'plotly', 'streamlit', 'openpyxl']
for package in required_packages:
    try:
        __import__(package)
    except ImportError:
        install(package)

import pandas as pd
import numpy as np
import plotly.express as px
import streamlit as st
import os

# --- Page Configuration ---
st.set_page_config(page_title="Global Oil Indicators Dashboard", page_icon="🌎", layout="wide")

# --- Hero Banner ---
CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
IMAGE_PATH = os.path.join(CURRENT_DIR, "image", "oil_banner.jpg")  # <-- 自己準備油田圖片放這
if os.path.exists(IMAGE_PATH):
    st.image(IMAGE_PATH, use_container_width=True)

st.markdown("""
<div style='text-align: right; font-size: 12px; color: gray;'>
Source: <a href='https://www.eia.gov/' target='_blank'>EIA Energy Information Administration</a>
</div>
""", unsafe_allow_html=True)

# --- Load & Clean function ---
def load_clean(file_path, value_name):
    df = pd.read_excel(file_path, header=None)
    df.columns = df.iloc[2]
    df = df.drop(index=[0, 1, 2]).reset_index(drop=True)
    country_col = df.columns[0]
    year_columns = [col for col in df.columns if pd.notna(col) and isinstance(col, (int, float)) and 1960 <= col <= 2023]
    df = df[[country_col] + year_columns]
    df = df.rename(columns={country_col: 'Country'})
    df = df.dropna(subset=['Country'])
    regions_to_exclude = ['OECD Americas', 'OECD Europe', 'OECD Asia Oceania', 'OECD',
                          'Non-OECD Europe and Eurasia', 'Non-OECD', 'Africa', 'Middle East',
                          'Asia Pacific', 'World']
    df = df[~df['Country'].isin(regions_to_exclude)]
    replace_dict = {'United States': 'United States of America', 'Venezuela2': 'Venezuela',
                    'Iran, Islamic Republic of3': 'Iran', 'Syria4': 'Syria', 'Libya5': 'Libya',
                    'Vietnam6': 'Vietnam', 'Brunei Darussalam7': 'Brunei', 'Sudan & South Sudan8': 'Sudan',
                    'Congo': 'Republic of the Congo'}
    df['Country'] = df['Country'].replace(replace_dict)
    df_long = df.melt(id_vars='Country', value_vars=year_columns, var_name='Year', value_name=value_name)
    df_long[value_name] = pd.to_numeric(df_long[value_name], errors='coerce').fillna(0)
    return df_long

# --- Load All Data ---
reserve = load_clean('data/reserve.xlsx', 'Reserve')
production = load_clean('data/production.xlsx', 'Production')
import_ = load_clean('data/import.xlsx', 'Import')
export = load_clean('data/export.xlsx', 'Export')
demand = load_clean('data/demand.xlsx', 'Demand')

# Merge all datasets
final_df = reserve.merge(production, on=['Country', 'Year'], how='outer') \
                  .merge(import_, on=['Country', 'Year'], how='outer') \
                  .merge(export, on=['Country', 'Year'], how='outer') \
                  .merge(demand, on=['Country', 'Year'], how='outer')

# --- Sidebar Navigation ---
st.sidebar.title("📌 Navigation")
section = st.sidebar.radio(
    "Choose Section:",
    [
        "1. Project Overview",
        "2. Global Indicator Map",
        "3. Country Time Series Analysis",
        "4. Top 10 Countries by Selected Indicator",
        "5. Indicator Distribution Across Countries"
    ]
)

# --- Section 1: Project Overview ---
if section == "1. Project Overview":
    st.title("🌎 Global Oil Indicators Project Overview")
    st.markdown("""
    This dashboard visualizes global oil-related metrics between 1960 and 2023:
    
    - Oil Reserves
    - Production
    - Import and Export Volumes
    - Domestic Demand

    **Main Functions:**  
    - Interactive world map for each indicator  
    - Time-series view for individual countries  
    - Year selection and color customization options
    """)
    st.dataframe(final_df.head())

# --- Section 2: Global Indicator Map ---
elif section == "2. Global Indicator Map":
    st.title("🗺️ Global Oil Indicator Map (1960-2023)")

    indicator_options = {
        'Reserve': 'Reserve (million barrels)',
        'Production': 'Production (million barrels)',
        'Import': 'Import (million barrels)',
        'Export': 'Export (million barrels)',
        'Demand': 'Demand (million barrels)'
    }
    indicator = st.selectbox('Select Indicator:', list(indicator_options.keys()), index=0)

    color_palettes = [
        'Viridis', 'Turbo', 'Cividis', 'Blues', 'Greens', 'Reds', 'Purples', 'Plasma',
        'Inferno', 'Magma', 'IceFire', 'RdYlGn', 'Spectral', 'RdBu', 'PuBuGn'
    ]
    color_palette = st.selectbox('Select Color Palette:', color_palettes, index=1)

    year = st.slider('Select Year:', 1960, 2023, 2012, step=1)

    filtered = final_df[final_df['Year'] == year]
    fig_map = px.choropleth(
        filtered,
        locations="Country",
        locationmode="country names",
        color=indicator,
        hover_name="Country",
        hover_data={
            'Reserve': ':.0f',
            'Production': ':.0f',
            'Import': ':.0f',
            'Export': ':.0f',
            'Demand': ':.0f'
        },
        color_continuous_scale=color_palette,
        labels={indicator: indicator_options[indicator]},
    )
    fig_map.update_layout(
        geo=dict(
            projection_type='equirectangular',
            showframe=False,
            showcoastlines=True,
            landcolor="lightgray"
        ),
        margin=dict(l=0, r=0, t=0, b=0)
    )
    st.plotly_chart(fig_map, use_container_width=True)

# --- Section 3: Country Time Series Analysis ---
elif section == "3. Country Time Series Analysis":
    st.title("📈 Country-Level Time Series Analysis")

    selected_country = st.selectbox('Select a Country:', sorted(final_df['Country'].dropna().unique()))
    indicator = st.selectbox('Select Indicator:', list(indicator_options.keys()), index=0, key="indicator_2")

    country_data = final_df[final_df['Country'] == selected_country]
    fig_line = px.line(
        country_data,
        x='Year',
        y=indicator,
        labels={'Year': 'Year', indicator: indicator_options[indicator]},
        title=f"{selected_country} — {indicator_options[indicator]}"
    )
    fig_line.update_traces(mode="lines+markers")
    fig_line.update_layout(margin=dict(l=20, r=20, t=20, b=20))
    st.plotly_chart(fig_line, use_container_width=True)

# --- Section 4: Top 10 Countries by Selected Indicator ---
elif section == "4. Top 10 Countries by Selected Indicator":
    st.title("🏆 Top 10 Countries by Selected Indicator")

    st.markdown("""
    This section ranks the top 10 countries based on the selected oil-related indicator.
    
    **Why it matters:**  
    Quickly identify major players in reserves, production, imports, exports, or demand.
    """)

    indicator = st.selectbox('Select Indicator:', list(indicator_options.keys()), index=0, key="indicator_3")
    year = st.slider('Select Year:', 1960, 2023, 2012, step=1, key="year_2")

    top10 = final_df[final_df['Year'] == year].sort_values(by=indicator, ascending=False).head(10)

    fig_top10 = px.bar(
        top10,
        x=indicator,
        y='Country',
        orientation='h',
        color='Country',
        title=f"Top 10 Countries by {indicator_options[indicator]} ({year})",
        labels={indicator: indicator_options[indicator]},
    )
    fig_top10.update_layout(
        yaxis=dict(autorange="reversed"),
        template='simple_white',
        margin=dict(l=20, r=20, t=40, b=20)
    )
    st.plotly_chart(fig_top10, use_container_width=True)

# --- Section 5: Indicator Distribution Across Countries ---
elif section == "5. Indicator Distribution Across Countries":
    st.title("📊 Indicator Distribution Across Countries")

    st.markdown("""
    This section shows the distribution of a selected oil-related indicator across all countries.
    
    **Why it matters:**  
    Understand global patterns, detect outliers, and explore how resources are distributed.
    """)

    indicator = st.selectbox('Select Indicator:', list(indicator_options.keys()), index=0, key="indicator_4")
    year = st.slider('Select Year:', 1960, 2023, 2012, step=1, key="year_3")

    distribution = final_df[final_df['Year'] == year][indicator]

    fig_dist = px.histogram(
        distribution,
        nbins=30,
        title=f"Distribution of {indicator_options[indicator]} ({year})",
        labels={indicator: indicator_options[indicator]},
    )
    fig_dist.update_layout(
        template='simple_white',
        margin=dict(l=20, r=20, t=40, b=20),
        xaxis_title=indicator_options[indicator],
        yaxis_title="Number of Countries"
    )
    st.plotly_chart(fig_dist, use_container_width=True)

# --- Footer ---
st.caption("Global Oil Indicators Dashboard · Powered by Streamlit · 2025")