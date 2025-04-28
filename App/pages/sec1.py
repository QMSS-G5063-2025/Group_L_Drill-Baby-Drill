import streamlit as st
import plotly.express as px

def render_sec1(final_df):
    """
    Render the World Map Distribution page using Streamlit.
    Allows users to select oil indicators, years, and color palettes.
    """

    # Page title
    st.title("🗺️ World Map Distribution")

    # --- Sidebar controls ---
    st.sidebar.header("Map Settings")

    # Available indicators
    indicator_options = {
        'Reserve': 'Reserve (million barrels)',
        'Production': 'Production (million barrels)',
        'Import': 'Import (million barrels)',
        'Export': 'Export (million barrels)',
        'Demand': 'Demand (million barrels)'
    }

    # Available color palettes
    color_palettes = [
        'Viridis', 'Turbo', 'Cividis', 'Blues', 'Greens', 'Reds', 'Purples', 'Plasma',
        'Inferno', 'Magma', 'IceFire', 'RdYlGn', 'Spectral', 'RdBu', 'PuBuGn'
    ]

    # User selections
    selected_indicator = st.sidebar.selectbox("Select Indicator:", list(indicator_options.keys()), index=0)
    selected_year = st.sidebar.slider("Select Year:", min_value=1960, max_value=2023, value=2012, step=1)
    selected_palette = st.sidebar.selectbox("Select Color Palette:", color_palettes, index=1)

    # --- Main area: Map ---

    # Filter data
    filtered = final_df[final_df['Year'] == selected_year]

    # Create choropleth map
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
        margin=dict(l=0, r=0, t=20, b=0)
    )

    # Display the map
    st.plotly_chart(fig, use_container_width=True)

    # --- Footer instructions ---
    st.markdown("""
    ---
    **Instructions:**  
    - Use the sidebar to select different oil indicators, years, and color palettes.  
    - Hover over countries on the map to view detailed data.  
    - Adjust the color palette to highlight different ranges more clearly.
    """)
