import streamlit as st
import pandas as pd
import plotly.express as px

@st.cache_data
def load_clean(file_path, value_name):
    df = pd.read_excel(file_path, header=None)
    df.columns = df.iloc[2]
    df = df.drop(index=[0, 1, 2]).reset_index(drop=True)
    country_col = df.columns[0]
    year_columns = [col for col in df.columns if pd.notna(col) and isinstance(col, (int, float))]
    df_long = df.melt(id_vars=[country_col], value_vars=year_columns, var_name="Year", value_name=value_name)
    df_long = df_long.rename(columns={country_col: "Country"})
    df_long['Year'] = df_long['Year'].astype(int)
    return df_long

def render_sec1():
    st.title("🗺️ Dynamic World Map: Oil Indicators Over Time")

    # Load all datasets
    reserve = load_clean('data/reserve.xlsx', 'Reserve')
    production = load_clean('data/production.xlsx', 'Production')
    import_ = load_clean('data/import.xlsx', 'Import')
    export = load_clean('data/export.xlsx', 'Export')
    demand = load_clean('data/demand.xlsx', 'Demand')

    # Merge datasets
    final_df = reserve.merge(production, on=['Country', 'Year'], how='outer')
    final_df = final_df.merge(import_, on=['Country', 'Year'], how='outer')
    final_df = final_df.merge(export, on=['Country', 'Year'], how='outer')
    final_df = final_df.merge(demand, on=['Country', 'Year'], how='outer')

    indicator_options = {
        'Reserve': 'Reserve (million barrels)',
        'Production': 'Production (million barrels)',
        'Import': 'Import (million barrels)',
        'Export': 'Export (million barrels)',
        'Demand': 'Demand (million barrels)'
    }

    indicator = st.selectbox("Select Indicator", list(indicator_options.keys()))
    palette = st.selectbox("Select Color Palette", ['Turbo', 'Viridis', 'Plasma', 'Inferno'])

    # Plot animated choropleth
    fig = px.choropleth(
        final_df,
        locations="Country",
        locationmode="country names",
        color=indicator,
        hover_name="Country",
        color_continuous_scale=palette,
        labels={indicator: indicator_options[indicator]},
        animation_frame="Year",   # 這行超重要！加上動畫年份
        range_color=(final_df[indicator].min(), final_df[indicator].max())
    )

    fig.update_layout(
        margin={"r":0,"t":0,"l":0,"b":0},
        geo=dict(
            projection_type="equirectangular",
            showframe=False,
            showcoastlines=True,
            landcolor="lightgray"
        )
    )

    st.plotly_chart(fig, use_container_width=True)