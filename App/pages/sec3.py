
import streamlit as st
import pandas as pd
import plotly.express as px

def render_sec3():
    st.title("⚡ Oil Consumption vs Production")

    df = pd.read_csv('data/final_df.csv')

    year = st.slider("Select Year", int(df['Year'].min()), int(df['Year'].max()), 2012)

    filtered = df[df['Year'] == year]

    fig = px.scatter(
        filtered,
        x='Production',
        y='Demand',
        hover_name='Country',
        labels={'Production': 'Production (million barrels)', 'Demand': 'Demand (million barrels)'},
        title='Production vs Demand'
    )

    fig.update_layout(margin={"r":0,"t":30,"l":0,"b":0})
    st.plotly_chart(fig, use_container_width=True)
