import streamlit as st
import pandas as pd
import plotly.express as px

@st.cache_data
def load_indicator(file_path, value_name):
    df = pd.read_excel(file_path, header=None)
    df.columns = df.iloc[2]
    df = df.drop(index=[0,1,2]).reset_index(drop=True)
    country_col = df.columns[0]
    year_cols = [col for col in df.columns if pd.notna(col) and isinstance(col, (int, float))]

    df_melt = df.melt(id_vars=[country_col], value_vars=year_cols, var_name='Year', value_name=value_name)
    df_melt = df_melt.rename(columns={country_col: 'Country'})
    df_melt['Year'] = df_melt['Year'].astype(int)
    df_melt[value_name] = pd.to_numeric(df_melt[value_name], errors='coerce')
    df_melt = df_melt.dropna(subset=[value_name])
    df_melt = df_melt[~df_melt['Country'].str.contains('World|Total', case=False, na=False)]
    return df_melt

def render_sec2():
    st.title("📊 Dynamic Top 10 Countries by Oil Indicators")

    indicator_file_mapping = {
        'Reserve': 'reserve.xlsx',
        'Production': 'production.xlsx',
        'Import': 'import.xlsx',
        'Export': 'export.xlsx',
        'Demand': 'demand.xlsx'
    }

    indicator = st.selectbox("Select Indicator", list(indicator_file_mapping.keys()))
    file_path = 'data/' + indicator_file_mapping[indicator]

    # Load and clean data
    df_melt = load_indicator(file_path, f'{indicator}_Value')

    # Get top 10 countries per year
    df_top10_each_year = (
        df_melt.groupby('Year')
        .apply(lambda x: x.sort_values(f'{indicator}_Value', ascending=False).head(10))
        .reset_index(drop=True)
    )

    # Plot animated bar chart
    fig = px.bar(
        df_top10_each_year,
        x='Country',
        y=f'{indicator}_Value',
        color='Country',
        animation_frame='Year',
        title=f"Top 10 Countries' {indicator} by Year",
        labels={f'{indicator}_Value': f'{indicator} (Million Barrels)'}
    )

    fig.update_layout(
        xaxis_tickangle=-45,
        yaxis_title=f'{indicator} (Million Barrels)',
        title_x=0.5,
        transition={'duration': 500},
        yaxis_type="linear" if indicator != "Reserve" else "log"  # Reserve預設用log scale，其它正常
    )

    st.plotly_chart(fig, use_container_width=True)