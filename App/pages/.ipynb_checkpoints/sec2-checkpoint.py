
import streamlit as st
import pandas as pd
import plotly.express as px

def render_sec2():
    st.title("📊 Top 10 Countries by Oil Indicators")

    df = pd.read_csv('data/final_df.csv')

    indicator_options = {
        'Reserve': 'Reserve (million barrels)',
        'Production': 'Production (million barrels)',
        'Import': 'Import (million barrels)',
        'Export': 'Export (million barrels)',
        'Demand': 'Demand (million barrels)'
    }

    indicator = st.selectbox("Select Indicator", list(indicator_options.keys()))
    year = st.slider("Select Year", int(df['Year'].min()), int(df['Year'].max()), 2012)

    filtered = df[df['Year'] == year]
    top10 = filtered.nlargest(10, indicator)

    fig = px.bar(
        top10.sort_values(by=indicator, ascending=True),
        x=indicator,
        y='Country',
        orientation='h',
        labels={indicator: indicator_options[indicator]}
    )

    st.plotly_chart(fig, use_container_width=True)
