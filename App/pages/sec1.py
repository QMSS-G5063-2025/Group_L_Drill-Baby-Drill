import streamlit as st
import plotly.express as px

def render_sec1(final_df):
    """
    Render World Map Distribution with indicator selection and year slider.
    """

    # --- Page Header ---
    st.markdown(
        """
        <h1 style="text-align: center;">🌍 Global Oil Indicators Map</h1>
        <p style="text-align: center;">Explore reserves, production, imports, exports, and demand across countries from 1960 to 2023.</p>
        """,
        unsafe_allow_html=True
    )

    # --- Sidebar controls ---
    st.sidebar.header("Map Settings")

    # Options
    indicator_options = {
        'Reserve': 'Reserve (million barrels)',
        'Production': 'Production (million barrels)',
        'Import': 'Import (million barrels)',
        'Export': 'Export (million barrels)',
        'Demand': 'Demand (million barrels)'
    }

    color_palettes = [
        'Viridis', 'Turbo', 'Cividis', 'Blues', 'Greens', 'Reds', 'Purples', 'Plasma',
        'Inferno', 'Magma', 'IceFire', 'RdYlGn', 'Spectral', 'RdBu', 'PuBuGn'
    ]

    # Sidebar selections
    selected_indicator = st.sidebar.radio("Select Indicator:", list(indicator_options.keys()), index=0)
    selected_year = st.sidebar.slider("Select Year:", 1960, 2023, 2012)
    selected_palette = st.sidebar.selectbox("Select Color Palette:", color_palettes, index=1)

    # --- Main Map ---
    filtered = final_df[final_df['Year'] == selected_year]

    fig = px.choropleth(
        filtered,
        locations="Country",
        locationmode="country names",
        color=selected_indicator,
        hover_name="Country",
        hover_data={
            'Reserve': ':.0f',
            'Production': ':.0f',
            'Import': ':.0f',
            'Export': ':.0f',
            'Demand': ':.0f'
        },
        color_continuous_scale=selected_palette,
        labels={selected_indicator: indicator_options[selected_indicator]},
        title=f"{indicator_options[selected_indicator]} in {selected_year}"
    )

    fig.update_layout(
        geo=dict(
            projection_type='equirectangular',
            showframe=False,
            showcoastlines=True,
            landcolor="lightgray"
        ),
        margin=dict(l=0, r=0, t=30, b=0)
    )

    # Display the map
    st.plotly_chart(fig, use_container_width=True)

    # --- Footer instructions ---
    st.markdown(
        """
        ---
        **How to use:**  
        - Use the sidebar to select an oil indicator and year.
        - Hover over countries to see detailed data.
        - Change color palette to visualize differently.
        """
    )
