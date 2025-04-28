# ===========================================
# Section 0: Install and Import Packages
# ===========================================
import sys
import subprocess

def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

required_packages = ['pandas', 'numpy', 'plotly', 'dash', 'openpyxl', 'streamlit']
for package in required_packages:
    try:
        __import__(package)
    except ImportError:
        install(package)

import pandas as pd
import numpy as np
import streamlit as st

# Import only section 1 page
from sec1 import render_sec1

# ===========================================
# Section 1: Data Preprocessing
# ===========================================

# Function: Load and clean a single dataset
def load_clean(file_path, value_name):
    """
    Load and clean a dataset from an Excel file.
    Standardize column names, remove aggregates, reshape to long format.
    """
    df = pd.read_excel(file_path, header=None)

    # Set third row as header
    df.columns = df.iloc[2]
    df = df.drop(index=[0, 1, 2]).reset_index(drop=True)

    country_col = df.columns[0]
    year_columns = [
        col for col in df.columns
        if pd.notna(col) and isinstance(col, (int, float)) and 1960 <= col <= 2023
    ]

    df_data = df[[country_col] + year_columns]
    df_data = df_data.rename(columns={country_col: 'Country'})
    df_data = df_data.dropna(subset=['Country'])

    regions_to_exclude = [
        'OECD Americas', 'OECD Europe', 'OECD Asia Oceania', 'OECD',
        'Non-OECD Europe and Eurasia', 'Non-OECD', 'Africa', 'Middle East',
        'Asia Pacific', 'World'
    ]
    df_data = df_data[~df_data['Country'].isin(regions_to_exclude)]

    replace_dict = {
        'United States': 'United States of America',
        'Venezuela2': 'Venezuela',
        'Iran, Islamic Republic of3': 'Iran',
        'Syria4': 'Syria',
        'Libya5': 'Libya',
        'Vietnam6': 'Vietnam',
        'Brunei Darussalam7': 'Brunei',
        'Sudan & South Sudan8': 'Sudan',
        'Congo': 'Republic of the Congo'
    }
    df_data['Country'] = df_data['Country'].replace(replace_dict)

    df_long = df_data.melt(
        id_vars='Country',
        value_vars=year_columns,
        var_name='Year',
        value_name=value_name
    )
    df_long[value_name] = pd.to_numeric(df_long[value_name], errors='coerce').fillna(0)
    return df_long

# Load all datasets needed for Section 1
reserve = load_clean('data/reserve.xlsx', 'Reserve')
production = load_clean('data/production.xlsx', 'Production')
import_ = load_clean('data/import.xlsx', 'Import')
export = load_clean('data/export.xlsx', 'Export')
demand = load_clean('data/demand.xlsx', 'Demand')

# Merge into a single final DataFrame
final_df = reserve.copy()
final_df = final_df.merge(production, on=['Country', 'Year'], how='left')
final_df = final_df.merge(import_, on=['Country', 'Year'], how='left')
final_df = final_df.merge(export, on=['Country', 'Year'], how='left')
final_df = final_df.merge(demand, on=['Country', 'Year'], how='left')

# ===========================================
# Section 2: Website Structure (Streamlit App)
# ===========================================

# Set Streamlit page settings
st.set_page_config(layout="wide")

# --- Navigation control for testing only Section 1 ---
st.title("🗺️ World Map Distribution")  # Directly set page title
render_sec1(final_df)  # Only render section 1