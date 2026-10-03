# social-web-analytics
COMP3020 Assessment 2 - Group Project

# COMP3020 Social Web Analytics - Group Project

## Project Topic

**Generative AI Discussions on Bluesky: Text, Engagement, Clustering and Social Network Analysis**

This project investigates public discussion about generative AI on Bluesky using R.

The analysis will address the four required COMP3020 project components:

- Text/content analysis and visualisation
- Hypothesis testing
- Clustering
- Network analysis

## Provisional Research Questions

**RQ1 - Text/Content Analysis**  
What words and themes are most prominent in Bluesky discussions about generative AI?

**RQ2 - Clustering**  
Can generative-AI posts be grouped into meaningful clusters based on similarities in their textual content?

**RQ3 - Hypothesis Testing**  
Is there a significant difference or association in engagement across different AI-related topics or post characteristics? 

**RQ4 - Network Analysis**  
Which users are the most structurally important within the Bluesky interaction network surrounding generative-AI discussions? 

> Note: RQ3 may be refined after the collected dataset and available variables are inspected.

---

# Project Folder Structure

```text
social-web-analytics/
│
├── data/
│   ├── raw/
│   └── processed/
│
├── R/
│
├── figures/
│
├── report/
│
├── .gitignore
├── README.md
└── social-web-analytics.Rproj


## Project Folder Structure

### `data/`
Stores all project datasets.

- `raw/` - Original Bluesky data as collected.
- `processed/` - Cleaned and prepared data used for analysis.

### `R/`
Stores all R scripts used for the project.

Planned files:
- `01_data_collection.R` - Collect Bluesky data.
- `02_data_cleaning.R` - Clean and prepare the data.
- `03_text_analysis.R` - Text/content analysis and visualisations.
- `04_hypothesis_testing.R` - Statistical hypothesis testing.
- `05_clustering.R` - Cluster similar posts.
- `06_network_analysis.R` - Build and analyse the Bluesky interaction network.

### `figures/`
Stores graphs and visualisations created in R.

### `report/`
Stores the R Markdown report and final PDF report.

### `.gitignore`
Prevents unnecessary or sensitive local files from being uploaded to GitHub.

### `social-web-analytics.Rproj`
RStudio project file used to open and work on the project.

Members:
1. Herman
2. Arend
3. Tim
