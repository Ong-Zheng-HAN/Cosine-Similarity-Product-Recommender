library(shiny)
library(bslib)
library(proxy)
library(plotly)
library(DT)
library(tidyverse)

# setwd("~/claude_workspace/cosine_similarity_recommender")

# ---- Helper functions ----

# Build user-product matrix from long-format data (user, product, value)
build_user_product_matrix <- function(df) {
  df %>%
    pivot_wider(
      names_from = product,
      values_from = value
    ) %>% 
    column_to_rownames(var = "user") %>% 
    as.matrix()
}

# Cosine similarity between row vectors
cosine_similarity <-function(mat) {
  simil(mat,
        method = "cosine") %>%
    as.matrix()
}

# Generate recommendations for a target user
recommend_products <- function(up_mat, sim_mat, target_user, top_n_users = 5) {
  # Products the target user already has
  owned <- names(which(up_mat[target_user, ] > 0))
  not_owned <- names(which(up_mat[target_user, ] == 0))

  if (length(not_owned) == 0) {
    return(data.frame(Product = character(), Score = numeric(),
                      Similar_Users_Who_Own = character(),
                      stringsAsFactors = FALSE))
  }

  # Get top similar users (exclude self)
  sims <- sim_mat[target_user, ]
  sims <- sims[names(sims) != target_user]
  sims <- sort(sims, decreasing = TRUE)
  top_users <- names(head(sims[sims > 0], top_n_users))

  if (length(top_users) == 0) {
    return(data.frame(Product = not_owned, Score = 0,
                      Similar_Users_Who_Own = "",
                      stringsAsFactors = FALSE))
  }

  # Score each un-owned product: weighted sum of similar users' ownership
  scores <- sapply(not_owned, function(prod) {
    sum(sim_mat[target_user, top_users] * up_mat[top_users, prod])
  })

  # Which similar users own each product
  who_owns <- sapply(not_owned, function(prod) {
    owners <- top_users[up_mat[top_users, prod] > 0]
    paste(owners, collapse = ", ")
  })

  df <- data.frame(
    Product = not_owned,
    Score = round(scores, 4),
    Similar_Users_Who_Own = who_owns,
    stringsAsFactors = FALSE
  )
  df[order(-df$Score), ]
}

# ---- UI ----

ui <- page_sidebar(
  title = "User-Based Collaborative Filtering Recommender",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 380,
    h5("Upload Data"),
    fileInput("csv_upload", NULL, accept = ".csv",
              placeholder = "user, product, value"),
    tags$small(class = "text-muted",
               "CSV columns: user, product, value",
               tags$br(),
               "(value = 1 for purchased, 0 for not)"),
    hr(),
    actionButton("load_sample", "Load Sample Data", class = "btn-outline-secondary w-100"),
    hr(),
    h5("Settings"),
    sliderInput("top_n", "Top N Similar Users for Recs", min = 1, max = 20, value = 5),
    hr(),
    h5("Data Summary"),
    uiOutput("data_summary"),
    hr(),
    actionButton("clear_btn", "Clear Data", class = "btn-outline-danger w-100")
  ),

  navset_card_tab(
    nav_panel(
      "User-Product Matrix",
      card_header("Who Purchased What"),
      DTOutput("up_matrix_table")
    ),
    nav_panel(
      "User Similarity",
      card_header("Cosine Similarity Between Users"),
      plotlyOutput("heatmap", height = "550px")
    ),
    nav_panel(
      "Similarity Scores",
      card_header("Pairwise User Similarity"),
      DTOutput("sim_table")
    ),
    nav_panel(
      "Recommendations",
      card_header("Product Recommendations for Target User"),
      selectInput("target_user", "Select Target User:", choices = NULL),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Recommended Products (not yet purchased)"),
          DTOutput("rec_table")
        ),
        card(
          card_header("Most Similar Users"),
          DTOutput("similar_users_table")
        )
      ),
      card(
        card_header("Visual: Target User vs Similar Users"),
        plotlyOutput("comparison_plot", height = "400px")
      )
    ),
    nav_panel(
      "Product Popularity",
      card_header("Product Adoption Overview"),
      plotlyOutput("popularity_plot", height = "450px")
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {

  raw_data <- reactiveVal(NULL)

  # Upload CSV
  observeEvent(input$csv_upload, {
    req(input$csv_upload)
    df <- read.csv(input$csv_upload$datapath, stringsAsFactors = FALSE)
    names(df) <- tolower(trimws(names(df)))
    req(all(c("user", "product") %in% names(df)))
    if (!"value" %in% names(df)) df$value <- 1
    df$user <- trimws(as.character(df$user))
    df$product <- trimws(as.character(df$product))
    df$value <- as.numeric(df$value)
    df$value[is.na(df$value)] <- 0
    raw_data(df)
  })

  # Load sample data
  observeEvent(input$load_sample, {
    sample_path <- file.path(getwd(), "sample_user_products.csv")
    if (file.exists(sample_path)) {
      df <- read.csv(sample_path, stringsAsFactors = FALSE)
      names(df) <- tolower(trimws(names(df)))
      df$user <- trimws(as.character(df$user))
      df$product <- trimws(as.character(df$product))
      df$value <- as.numeric(df$value)
      raw_data(df)
    }
  })

  # Clear
  observeEvent(input$clear_btn, { raw_data(NULL) })

  # Reactive: computed matrices
  matrices <- reactive({
    df <- raw_data()
    req(df)
    req(nrow(df) > 0)
    up_mat <- build_user_product_matrix(df)
    sim_mat <- cosine_similarity(up_mat)
    list(up_mat = up_mat, sim_mat = sim_mat)
  })

  # Update target user dropdown
  observe({
    m <- matrices()
    updateSelectInput(session, "target_user", choices = rownames(m$up_mat))
  })

  # Data summary
  output$data_summary <- renderUI({
    df <- raw_data()
    if (is.null(df)) return(tags$p(class = "text-muted", "No data loaded."))
    n_users <- length(unique(df$user))
    n_products <- length(unique(df$product))
    n_purchases <- sum(df$value > 0)
    sparsity <- round(1 - n_purchases / (n_users * n_products), 3) * 100
    tags$ul(
      class = "list-unstyled",
      tags$li(tags$strong(n_users), " users"),
      tags$li(tags$strong(n_products), " products"),
      tags$li(tags$strong(n_purchases), " purchases"),
      tags$li(tags$strong(paste0(sparsity, "%")), " sparsity")
    )
  })

  # ---- User-Product Matrix ----
  output$up_matrix_table <- renderDT({
    m <- matrices()
    df <- as.data.frame(m$up_mat)
    df <- cbind(User = rownames(df), df)
    rownames(df) <- NULL
    datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE) |>
      formatStyle(names(df)[-1], backgroundColor = styleEqual(c(0, 1), c("#f8f9fa", "#2ecc71")),
                  color = styleEqual(c(0, 1), c("#adb5bd", "#ffffff")),
                  fontWeight = "bold", textAlign = "center")
  })

  # ---- User Similarity Heatmap ----
  output$heatmap <- renderPlotly({
    m <- matrices()
    sim <- m$sim_mat
    plot_ly(
      x = colnames(sim), y = rownames(sim), z = round(sim, 3),
      type = "heatmap", colorscale = "Blues",
      zmin = 0, zmax = 1,
      hovertemplate = "User X: %{x}<br>User Y: %{y}<br>Similarity: %{z}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "", autorange = "reversed"),
        margin = list(l = 100, b = 100)
      )
  })

  # ---- Pairwise similarity table ----
  output$sim_table <- renderDT({
    m <- matrices()
    sim <- m$sim_mat
    pairs <- expand.grid(User_A = rownames(sim), User_B = colnames(sim),
                         stringsAsFactors = FALSE)
    pairs$Similarity <- as.vector(sim)
    pairs <- pairs[pairs$User_A < pairs$User_B, ]
    pairs <- pairs[order(-pairs$Similarity), ]
    rownames(pairs) <- NULL
    pairs$Similarity <- round(pairs$Similarity, 4)
    datatable(pairs, options = list(pageLength = 20), rownames = FALSE)
  })

  # ---- Recommendations ----
  output$rec_table <- renderDT({
    m <- matrices()
    req(input$target_user)
    req(input$target_user %in% rownames(m$up_mat))
    recs <- recommend_products(m$up_mat, m$sim_mat, input$target_user, input$top_n)
    recs <- recs[recs$Score > 0, ]
    if (nrow(recs) == 0) {
      return(datatable(data.frame(Message = "No recommendations available"),
                       rownames = FALSE))
    }
    datatable(recs, options = list(pageLength = 20), rownames = FALSE) |>
      formatStyle("Score",
                  background = styleColorBar(range(recs$Score), "#3498db"),
                  backgroundSize = "98% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # Similar users table
  output$similar_users_table <- renderDT({
    m <- matrices()
    req(input$target_user)
    req(input$target_user %in% rownames(m$sim_mat))
    sims <- m$sim_mat[input$target_user, ]
    sims <- sims[names(sims) != input$target_user]
    sims <- sort(sims, decreasing = TRUE)
    df <- data.frame(
      User = names(sims),
      Similarity = round(as.numeric(sims), 4),
      Products_Owned = sapply(names(sims), function(u) sum(m$up_mat[u, ] > 0)),
      stringsAsFactors = FALSE
    )
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })

  # Comparison plot: target user vs similar users
  output$comparison_plot <- renderPlotly({
    m <- matrices()
    req(input$target_user)
    req(input$target_user %in% rownames(m$up_mat))
    target <- input$target_user

    sims <- m$sim_mat[target, ]
    sims <- sims[names(sims) != target]
    sims <- sort(sims, decreasing = TRUE)
    top_users <- names(head(sims[sims > 0], input$top_n))

    show_users <- c(target, top_users)
    sub_mat <- m$up_mat[show_users, , drop = FALSE]

    products <- colnames(sub_mat)
    plot_data <- do.call(rbind, lapply(show_users, function(u) {
      data.frame(User = u, Product = products, Owned = sub_mat[u, ],
                 stringsAsFactors = FALSE)
    }))
    plot_data$User <- factor(plot_data$User, levels = rev(show_users))
    plot_data$marker <- ifelse(plot_data$Owned > 0, "Purchased", "Not Purchased")

    plot_ly(plot_data, x = ~Product, y = ~User, color = ~marker,
            colors = c("Not Purchased" = "#ecf0f1", "Purchased" = "#2ecc71"),
            type = "heatmap", z = ~Owned,
            showscale = FALSE,
            hovertemplate = "User: %{y}<br>Product: %{x}<br>%{text}<extra></extra>",
            text = ~marker) |>
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = ""),
        margin = list(l = 100, b = 120),
        annotations = list(
          list(x = 0.5, y = 1.06, text = paste0("Target: ", target, " (top row) vs Similar Users"),
               showarrow = FALSE, xref = "paper", yref = "paper",
               font = list(size = 13, color = "#7f8c8d"))
        )
      )
  })

  # ---- Product Popularity ----
  output$popularity_plot <- renderPlotly({
    m <- matrices()
    counts <- colSums(m$up_mat > 0)
    df <- data.frame(Product = names(counts), Users = as.integer(counts),
                     stringsAsFactors = FALSE)
    df <- df[order(df$Users), ]
    df$Product <- factor(df$Product, levels = df$Product)

    plot_ly(df, x = ~Users, y = ~Product, type = "bar", orientation = "h",
            marker = list(color = "#3498db"),
            hovertemplate = "%{y}<br>%{x} users<extra></extra>") |>
      layout(
        xaxis = list(title = "Number of Users"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
}

shinyApp(ui, server)
