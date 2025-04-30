# --- Import packages ---
import pandas as pd
import numpy as np
import plotly.express as px
import streamlit as st
import os

# --- Page Configuration ---
st.set_page_config(page_title="Oil & Gas Global Dashboard", page_icon="🌍", layout="wide")

# --- Hero Banner ---
CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
IMAGE_PATH = os.path.join(CURRENT_DIR, "image", "oil_banner.jpg")
if os.path.exists(IMAGE_PATH):
    st.image(IMAGE_PATH, use_container_width=True)

# --- Load Data for Global Oil Dashboard ---
@st.cache_data
def load_global_data():
    def load_clean(filepath, value_name):
        df = pd.read_excel(filepath, header=None)
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

    reserve = load_clean('data/reserve.xlsx', 'Reserve')
    production = load_clean('data/production.xlsx', 'Production')
    import_ = load_clean('data/import.xlsx', 'Import')
    export = load_clean('data/export.xlsx', 'Export')
    demand = load_clean('data/demand.xlsx', 'Demand')

    return reserve, production, import_, export, demand

reserve, production, import_, export, demand = load_global_data()

# Combine global dataset for easier handling
def combine_global_data(reserve, production, import_, export, demand):
    final_df = reserve.merge(production, on=['Country', 'Year'], how='outer') \
                     .merge(import_, on=['Country', 'Year'], how='outer') \
                     .merge(export, on=['Country', 'Year'], how='outer') \
                     .merge(demand, on=['Country', 'Year'], how='outer')
    return final_df

final_df = combine_global_data(reserve, production, import_, export, demand)

# --- Updated Filtering Example (fix for long-format access) ---
# Example: get Reserve for year 2012
# indicator = 'Reserve'; year = 2012
# filtered = final_df[(final_df['Year'] == year)][['Country', indicator]].dropna()

# --- Load Data for Drill Baby Drill Project ---
@st.cache_data
def load_drill_data():
    petroleum_prod = pd.read_excel('data/EIA_Petroleum_by_country.xlsx')
    gas_prod = pd.read_excel('data/EIA_Gas_by_country.xlsx')
    petroleum_consump = pd.read_excel('data/EIA_Petroleum_consumption_country.xlsx')
    gas_consump = pd.read_excel('data/EIA_Gas_Consumption_country.xlsx')
    return petroleum_prod, gas_prod, petroleum_consump, gas_consump

petroleum_prod, gas_prod, petroleum_consump, gas_consump = load_drill_data()

# --- Main Tabs Navigation ---
main_tab1, main_tab2 = st.tabs(["\U0001F30D Global Oil Dashboard", "\U0001F4D6 Drill Baby Drill Project"])

# --- Global Oil Dashboard Main Tab ---
with main_tab1:
    subtab1, subtab2, subtab3, subtab4, subtab5 = st.tabs([
        "Overview", 
        "Global Indicator Map", 
        "Country Time Series", 
        "Top 10 Countries", 
        "Indicator Distribution"
    ])

    with subtab1:
        st.header("Project Overview")
        st.markdown("""
        This dashboard visualizes global oil-related metrics between 1960 and 2023:
        - Oil Reserves
        - Production
        - Import and Export Volumes
        - Domestic Demand
        """)
        st.dataframe(final_df.head())

    with subtab2:
        st.header("Global Indicator Map")
        indicator_options = ['Reserve', 'Production', 'Import', 'Export', 'Demand']
        indicator = st.selectbox('Select Indicator:', indicator_options)
        year = st.slider('Select Year:', 1960, 2023, 2012, step=1)

        filtered = final_df[['Country', indicator + ' ' + str(year)]].dropna()
        fig_map = px.choropleth(
            filtered,
            locations="Country",
            locationmode="country names",
            color=filtered.columns[1],
            hover_name="Country",
            color_continuous_scale='Viridis',
            title=f"{indicator} in {year}"
        )
        st.plotly_chart(fig_map, use_container_width=True)

    with subtab3:
        st.header("Country Time Series Analysis")
        country = st.selectbox('Select Country:', final_df['Country'].dropna().unique())
        indicator = st.selectbox('Select Indicator:', ['Reserve', 'Production', 'Import', 'Export', 'Demand'], key="indicator_ts")

        country_data = final_df[final_df['Country'] == country]
        country_data = country_data.melt(id_vars='Country', var_name='Year_Indicator', value_name='Value')
        country_data[['Indicator', 'Year']] = country_data['Year_Indicator'].str.extract(r'(.+)\s(\d+)')
        country_data['Year'] = pd.to_numeric(country_data['Year'], errors='coerce')
        country_filtered = country_data[(country_data['Indicator'] == indicator)].dropna()

        fig_ts = px.line(country_filtered, x='Year', y='Value', title=f"{country} - {indicator} Over Time")
        st.plotly_chart(fig_ts, use_container_width=True)

    with subtab4:
        st.header("Top 10 Countries by Selected Indicator")
        indicator = st.selectbox('Select Indicator:', ['Reserve', 'Production', 'Import', 'Export', 'Demand'], key="indicator_top10")
        year = st.slider('Select Year:', 1960, 2023, 2012, step=1, key="year_top10")

        regions_to_exclude = ['Middle East', 'Africa', 'Asia Pacific', 'World', 'OECD', 'Non-OECD Europe and Eurasia', 'Non-OECD', 'OECD Americas', 'OECD Europe', 'OECD Asia Oceania']
        top10 = final_df[~final_df['Country'].isin(regions_to_exclude)][['Country', indicator + ' ' + str(year)]].dropna()
        top10 = top10.sort_values(by=indicator + ' ' + str(year), ascending=False).head(10)

        fig_top10 = px.bar(
            top10,
            x=indicator + ' ' + str(year),
            y='Country',
            orientation='h',
            title=f"Top 10 Countries by {indicator} in {year}",
            labels={indicator + ' ' + str(year): indicator}
        )
        fig_top10.update_layout(yaxis={'categoryorder':'total ascending'})
        st.plotly_chart(fig_top10, use_container_width=True)

    with subtab5:
        st.header("Indicator Distribution Across Countries")
        indicator = st.selectbox('Select Indicator:', ['Reserve', 'Production', 'Import', 'Export', 'Demand'], key="indicator_dist")
        year = st.slider('Select Year:', 1960, 2023, 2012, step=1, key="year_dist")

        dist_data = final_df[[indicator + ' ' + str(year)]].dropna()
        fig_dist = px.histogram(dist_data, x=indicator + ' ' + str(year), nbins=30, title=f"Distribution of {indicator} in {year}")
        st.plotly_chart(fig_dist, use_container_width=True)

# --- Drill Baby Drill Project Main Tab ---
with main_tab2:
    drilltab1, drilltab2, drilltab3, drilltab4, drilltab5 = st.tabs([
        "Introduction", 
        "U.S. Production History", 
        "U.S. vs World", 
        "Short-Term Forecasts", 
        "Long-Term Scenarios"
    ])

    with drilltab1:
        st.header("Introduction")
        st.markdown("""
        In this project, we explore the importance of U.S. oil and gas production in the global context.
        We analyze historical production trends, compare U.S. output with other countries, and project future scenarios.
        The goal is to provide a comprehensive overview of how critical energy production patterns shape the global economy.
        """)

    with drilltab2:
        st.header("U.S. Oil and Gas Production History")

        # --- Crude Oil Production Trend ---
        st.subheader("Crude Oil Production (1960-2023)")

        petroleum_prod_clean = petroleum_prod.rename(columns={petroleum_prod.columns[0]: 'Country'})
        us_petroleum = petroleum_prod_clean[petroleum_prod_clean['Country'] == 'United States of America']
        us_petroleum = us_petroleum.melt(id_vars='Country', var_name='Year', value_name='Production')
        us_petroleum['Year'] = pd.to_numeric(us_petroleum['Year'], errors='coerce')

        fig_crude = px.line(
            us_petroleum,
            x='Year',
            y='Production',
            title="U.S. Crude Oil Production Over Time",
            markers=True,
            labels={"Production": "Million Barrels"}
        )
        st.plotly_chart(fig_crude, use_container_width=True)

        # --- Natural Gas Production Trend ---
        st.subheader("Natural Gas Production (1960-2023)")

        gas_prod_clean = gas_prod.rename(columns={gas_prod.columns[0]: 'Country'})
        us_gas = gas_prod_clean[gas_prod_clean['Country'] == 'United States of America']
        us_gas = us_gas.melt(id_vars='Country', var_name='Year', value_name='Production')
        us_gas['Year'] = pd.to_numeric(us_gas['Year'], errors='coerce')

        fig_gas = px.line(
            us_gas,
            x='Year',
            y='Production',
            title="U.S. Natural Gas Production Over Time",
            markers=True,
            labels={"Production": "Billion Cubic Feet (Bcf)"}
        )
        st.plotly_chart(fig_gas, use_container_width=True)

    with drilltab3:
        st.header("U.S. vs World Oil and Gas Production")

        st.subheader("Global Crude Oil Production Map")

        oil_latest_year = petroleum_prod.columns[-1]  # Assume last column is latest year
        oil_data = petroleum_prod.rename(columns={petroleum_prod.columns[0]: 'Country'})
        oil_data = oil_data[['Country', oil_latest_year]].dropna()

        fig_oil_map = px.choropleth(
            oil_data,
            locations="Country",
            locationmode="country names",
            color=oil_latest_year,
            hover_name="Country",
            color_continuous_scale='Oranges',
            title=f"Crude Oil Production by Country ({oil_latest_year})"
        )
        st.plotly_chart(fig_oil_map, use_container_width=True)

        st.subheader("Global Natural Gas Production Map")

        gas_latest_year = gas_prod.columns[-1]
        gas_data = gas_prod.rename(columns={gas_prod.columns[0]: 'Country'})
        gas_data = gas_data[['Country', gas_latest_year]].dropna()

        fig_gas_map = px.choropleth(
            gas_data,
            locations="Country",
            locationmode="country names",
            color=gas_latest_year,
            hover_name="Country",
            color_continuous_scale='Blues',
            title=f"Natural Gas Production by Country ({gas_latest_year})"
        )
        st.plotly_chart(fig_gas_map, use_container_width=True)

    with drilltab4:
        st.header("Short-Term Forecasts")

        st.subheader("Crude Oil Production Forecast (2010-2023)")

        recent_us_petroleum = us_petroleum[us_petroleum['Year'] >= 2010]

        # Linear Regression
        from sklearn.linear_model import LinearRegression

        X = recent_us_petroleum[['Year']]
        y = recent_us_petroleum['Production']
        model = LinearRegression()
        model.fit(X, y)
        recent_us_petroleum['Prediction'] = model.predict(X)

        fig_forecast = px.scatter(recent_us_petroleum, x='Year', y='Production', labels={"Production": "Million Barrels"})
        fig_forecast.add_traces(px.line(recent_us_petroleum, x='Year', y='Prediction').data)
        fig_forecast.update_layout(title="Short-Term Crude Oil Production Forecast (Linear Regression)")
        st.plotly_chart(fig_forecast, use_container_width=True)

    with drilltab5:
        st.header("Long-Term Scenarios")

        st.subheader("Projected Crude Oil Production Scenarios (2023-2050)")

        future_years = np.arange(2023, 2051)
        baseline = model.predict(pd.DataFrame(future_years, columns=['Year']))
        optimistic = baseline * 1.2
        pessimistic = baseline * 0.8

        scenario_df = pd.DataFrame({
            'Year': future_years,
            'Baseline': baseline,
            'Optimistic': optimistic,
            'Pessimistic': pessimistic
        })

        fig_scenario = px.line(
            scenario_df,
            x='Year',
            y=['Baseline', 'Optimistic', 'Pessimistic'],
            labels={"value": "Projected Production", "variable": "Scenario"},
            title="Long-Term Crude Oil Production Scenarios"
        )
        st.plotly_chart(fig_scenario, use_container_width=True)

# --- Footer ---
st.caption("Oil & Gas Global Dashboard · Powered by Streamlit · 2025")