
import streamlit as st

def render_home():
    st.title("🛢️ Global Oil Dashboard")

    col1, col2 = st.columns([3, 1])
    with col1:
        st.image("https://www.visualcapitalist.com/wp-content/uploads/2020/01/oil-reserves-by-country-2020.html")
        st.header("Understanding Global Oil Dynamics")
        st.markdown("""
Explore how crude oil reserves, production, imports, exports, and consumption evolve across countries over time.
This dashboard enables interactive exploration of trends and insights within the oil sector globally.
        """)

    with col2:
        st.subheader("Developer")
        st.write("- Your Name
- 2025 Project")

    st.divider()
    st.header("Sections")
    cols = st.columns(3)
    sections = ["World Map Distribution", "Top 10 Bar Charts", "Consumption vs Production"]
    for col, section in zip(cols, sections):
        with col:
            if st.button(f"Go to {section}"):
                st.session_state.current_section = section
