# Cosine Similarity Product Recommender

A user-based collaborative filtering recommender built with R Shiny. It uses cosine similarity to identify users with similar purchase patterns and recommends products that similar users own but the target user hasn't purchased yet.

## How It Works

1. **Build a user-product matrix** from purchase data (rows = users, columns = products, values = 1/0)
2. **Compute cosine similarity** between user vectors to find users with similar buying behaviour
3. **Generate recommendations** for a target user by scoring un-owned products based on how many similar users purchased them, weighted by similarity

## Dashboard Tabs

| Tab | Description |
|-----|-------------|
| **User-Product Matrix** | Color-coded table showing who purchased what |
| **User Similarity** | Interactive heatmap of cosine similarity between all users |
| **Similarity Scores** | Sortable pairwise similarity table |
| **Recommendations** | Select a target user to see recommended products, most similar users, and a visual comparison grid |
| **Product Popularity** | Bar chart of product adoption across all users |

## Getting Started

### Prerequisites

```r
install.packages(c("shiny", "bslib", "proxy", "plotly", "DT", "tidyverse"))
```

### Run the App

```bash
cd cosine_similarity_recommender
Rscript -e 'shiny::runApp("app.R")'
```

Or from RStudio, open `app.R` and click **Run App**.

### Load Data

- Click **Load Sample Data** in the sidebar to use the included sample dataset
- Or upload your own CSV with columns: `user`, `product`, `value`

### CSV Format

```csv
user,product,value
Alice,Netflix,1
Alice,Spotify,1
Alice,Disney Plus,0
Bob,Netflix,1
Bob,Spotify,0
Bob,Disney Plus,1
```

- `user` — user identifier
- `product` — product name
- `value` — 1 for purchased/subscribed, 0 for not

## Sample Dataset

`sample_user_products.csv` includes 10 users and 15 subscription products with three distinct user profiles:

| Profile | Users | Typical Products |
|---------|-------|-----------------|
| Entertainment / Readers | Alice, Bob, Hank, Irene | Netflix, Spotify, Disney+, Kindle Unlimited, Audible |
| Productivity / Creative | Charlie, Diana, Grace, Jake | Adobe CC, Microsoft 365, Notion, Figma, Dropbox |
| Gamers | Eve, Frank | Nintendo Switch Online, PlayStation Plus, Xbox Game Pass |

## Tech Stack

- **R Shiny** — web framework
- **bslib** — Bootstrap 5 theming
- **proxy** — cosine similarity computation
- **plotly** — interactive charts
- **DT** — interactive data tables
- **tidyverse** — data wrangling
