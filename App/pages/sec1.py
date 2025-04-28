import pandas as pd
import plotly.express as px
from dash import dcc, html, Input, Output, State

def render_sec1(app, final_df):
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

    app.layout = html.Div([
        dcc.Graph(id='oil-map', style={'height': '100vh'}),
        html.Div([
            dcc.Tabs(id="tabs", value='tab-controls', children=[
                dcc.Tab(label='Controls', children=[
                    html.Div([
                        html.Label('Indicator:'),
                        dcc.Dropdown(
                            id='indicator-dropdown',
                            options=[{'label': v, 'value': k} for k, v in indicator_options.items()],
                            value='Reserve'
                        ),
                        html.Br(),
                        html.Label('Color Palette:'),
                        dcc.Dropdown(
                            id='color-dropdown',
                            options=[{'label': palette, 'value': palette} for palette in color_palettes],
                            value='Turbo'
                        ),
                        html.Br(),
                        html.Label('Year:'),
                        dcc.Slider(
                            id='year-slider',
                            min=1960,
                            max=2023,
                            step=1,
                            value=2012,
                            marks={str(year): str(year) for year in range(1960, 2024, 50)},
                            tooltip={"placement": "bottom", "always_visible": True}
                        )
                    ], style={'padding': '10px'})
                ]),
                dcc.Tab(label='Chart', children=[
                    html.Div([
                        html.H5(id='chart-title', style={'textAlign': 'center'}),
                        dcc.Graph(id='time-series', style={'height': '300px'})
                    ], style={'padding': '10px'})
                ]),
                dcc.Tab(label='About', children=[
                    html.Div([
                        html.H5("About This Dashboard"),
                        html.P("This app visualizes global crude oil indicators from 1960 to 2023."),
                        html.P("Click on a country to see its historical trend."),
                        html.P("You can customize the color palette.")
                    ], style={'padding': '10px'})
                ])
            ])
        ], id='floating-panel', style={
            'position': 'absolute',
            'bottom': '20px',
            'left': '20px',
            'width': '360px',
            'backgroundColor': 'white',
            'borderRadius': '10px',
            'boxShadow': '2px 2px 10px rgba(0,0,0,0.3)',
            'zIndex': '10',
            'cursor': 'default'
        }),
    ], style={'overflow': 'hidden'})

    @app.callback(
        Output('oil-map', 'figure'),
        Input('indicator-dropdown', 'value'),
        Input('color-dropdown', 'value'),
        Input('year-slider', 'value')
    )
    def update_map(selected_indicator, selected_palette, selected_year):
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
            title=None
        )

        fig.update_layout(
            geo=dict(
                projection_type='equirectangular',
                showframe=False,
                showcoastlines=True,
                landcolor="lightgray"
            ),
            margin=dict(l=0, r=0, t=0, b=0)
        )
        return fig

    @app.callback(
        Output('time-series', 'figure'),
        Output('chart-title', 'children'),
        Input('oil-map', 'clickData'),
        State('indicator-dropdown', 'value'),
        prevent_initial_call=True
    )
    def update_chart(clickData, selected_indicator):
        if clickData is None:
            raise dash.exceptions.PreventUpdate

        country_name = clickData['points'][0]['location']
        country_data = final_df[final_df['Country'] == country_name]

        fig = px.line(
            country_data,
            x='Year',
            y=selected_indicator,
            labels={'Year': 'Year', selected_indicator: indicator_options[selected_indicator]},
            title=None
        )

        fig.update_traces(mode="lines+markers")
        fig.update_layout(
            margin=dict(l=20, r=20, t=20, b=20),
            height=300
        )

        chart_title = f"{country_name} — {indicator_options[selected_indicator]}"
        return fig, chart_title
