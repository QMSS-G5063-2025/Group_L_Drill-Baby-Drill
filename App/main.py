
import streamlit as st
from home import render_home
from sec1 import render_sec1
from sec2 import render_sec2
from sec3 import render_sec3

st.set_page_config(layout="wide")

if "current_section" not in st.session_state:
    st.session_state.current_section = "Home"

with st.sidebar:
    st.title("🌍 Oil Dashboard Navigation")
    sections = {
        "Home": "🏠",
        "World Map Distribution": "🗺️",
        "Top 10 Bar Charts": "📊",
        "Consumption vs Production": "⚡"
    }
    for section, icon in sections.items():
        if st.button(f"{icon} {section}"):
            st.session_state.current_section = section

if st.session_state.current_section == "Home":
    render_home()
elif st.session_state.current_section == "World Map Distribution":
    render_sec1()
elif st.session_state.current_section == "Top 10 Bar Charts":
    render_sec2()
elif st.session_state.current_section == "Consumption vs Production":
    render_sec3()
