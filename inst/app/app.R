library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(reactable)
library(tidygraph)
library(visNetwork)
library(readxl)
library(grid)
library(gridExtra)
library(janitor)
library(shinyjs)
library(tibble)
library(shinycssloaders)

ui <- tagList(
  tags$head(
    tags$title("AssociationExplorer App"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "https://html2canvas.hertzen.com/dist/html2canvas.min.js"),
    tags$script(HTML('
    function captureAndDownload(elementId, filename) {
      html2canvas(document.getElementById(elementId)).then(canvas => {
        var link = document.createElement("a");
        document.body.appendChild(link);
        link.download = filename;
        link.href = canvas.toDataURL("image/png");
        link.click();
      });
    }
  ')),
    tags$style(HTML("
    .download-btn {
      background-color: transparent;
      border: 1px solid #bbbbbb;
      color: #555555;
      padding: 3px 8px;
      border-radius: 4px;
      cursor: pointer;
      font-size: 12px;
      margin-left: 10px;
    }
    .download-btn:hover {
      background-color: #f0f0f0;
    }
    .download-container {
      text-align: right;
      margin-top: 10px;
      margin-bottom: 10px;
    }
  "))
  ),
  fluidPage(
    shinyjs::useShinyjs(),
    class = "app-container",
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#0072B2",
      base_font = font_google("Roboto"),
      heading_font = font_google("Roboto Slab"),
      code_font = font_google("Fira Code")
    ),
    tags$div(class = "centered-padding-top"),
    titlePanel(div("Association Explorer", class = "app-title")),
    br(),
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      tabPanel(
        title = tags$strong("📁 Data"),
        value = "upload_tab",
        br(),
        br(),
        fileInput("data_file", "Upload your dataset (CSV or Excel)",
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".xlsx",
            ".xls"
          )
        ),
        tags$p(
          style = "font-size:0.85em; color: #666666;",
          "For CSV files: values must be comma-separated (,) and decimals must use a dot (.)."
        ),
        fileInput("desc_file", "(Optional) Upload variable descriptions (CSV or Excel)",
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".xlsx",
            ".xls"
          )
        ),
        tags$p(
          style = "font-size:0.85em; color: #666666;",
          "The descriptions file must contain exactly two columns named 'Variable' and 'Description'."
        ),
        br(),
        actionButton("process_data", "Process data", class = "btn btn-primary")
      ),
      tabPanel(
        title = tags$strong("🔍 Variables"),
        value = "variables_tab",
        br(),
        br(),
        # selector for variables to include
        uiOutput("variable_checkboxes_ui"),
        br(),
        # optional survey weight selector
        uiOutput("weight_var_ui"),
        tags$p(
          style = "font-size:0.85em; color: #666666;",
          "If a survey weight is selected, all association measures, plots and tables are computed using these weights."
        ),
        br(),
        uiOutput("go_to_network_ui"),
        br(),
        br(),
        br(),
        uiOutput("selected_vars_table_ui")
      ),
      tabPanel(
        title = tags$strong("🔗 Correlation Network"),
        value = "network_tab",
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              "threshold_num",
              "Range for Quantitative-Quantitative and Quantitative-Categorical Associations (R²)",
              min = 0, max = 1,
              value = c(0.5, 1), # default range
              step = 0.05
            ),
            sliderInput(
              "threshold_cat",
              "Range for Categorical-Categorical Associations (Cramer's V)",
              min = 0, max = 1,
              value = c(0.5, 1), # default range
              step = 0.05
            ),
            tags$i(tags$span(
              style = "color: #666666",
              "Only associations whose strength falls within the selected ranges will be displayed in the plot."
            )),
            br(),
            br(),
            fluidRow(
              column(12,
                align = "center",
                actionButton("go_to_pairs", "See pairs plots", class = "btn btn-primary")
              )
            )
          ),
          mainPanel(
            class = "panel-white",
            withSpinner(visNetworkOutput("network_vis", height = "600px", width = "100%"), type = 6, color = "#0072B2")
          )
        )
      ),
      tabPanel(
        title = tags$strong("📊 Pairs Plots"),
        value = "pairs_tab",
        fluidPage(
          class = "panel-white",
          withSpinner(uiOutput("pairs_plot"), type = 6, color = "#0072B2")
        )
      ),
      tabPanel(
        title = tags$strong("❓ Help"),
        value = "help_tab",
        div(
          class = "help-container",
          br(),
          br(),
          h3("How to use the Association Explorer app?"),
          br(),
          tags$ul(
            tags$li("Upload your dataset (CSV or Excel) in the 'Data' tab. For CSV files, the separator must be a comma (,) and decimal values must use a dot (.). Excel files (.xlsx) are supported without additional formatting requirements. Optionally, you can upload a variable description file. This file must contain exactly two columns named 'Variable' and 'Description', where 'Variable' matches the column names in your dataset."),
            tags$li("In the 'Variables' tab, select the variables you want to explore. If you upload a file containing variables' descriptions, a summary table below shows the selected variables along with their descriptions."),
            tags$li("(Optional) Select a survey weight variable in the 'Variables' tab. When provided, all association measures, plots and tables are computed using these weights."),
            tags$li("Click 'Visualize all associations' to access the correlation network."),
            tags$li("Adjust the threshold ranges to filter associations by strength. Only variables whose associations fall within the selected ranges will appear in the network and in the pairs plots."),
            tags$li("In the correlation network plot, thicker and shorter edges indicate stronger associations. Moreover, for quantitative-quantitative pairs only, red edges indicate negative associations, while blue edges indicate positive ones."),
            tags$li("Click 'See pairs plots' to display bivariate visualizations for retained associations.")
          )
        )
      )
    ),
    br(),
    tags$hr(),
    tags$footer(
      class = "app-footer",
      "v3.6.",
      tags$a(href = "https://github.com/AntoineSoetewey/AssociationExplorer2", "Code", target = "_blank")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  var_descriptions <- reactiveVal(NULL)

  observeEvent(input$process_data, {
    req(input$data_file)

    # Read the uploaded data
    data_path <- input$data_file$datapath
    if (grepl("\\.csv$", data_path, ignore.case = TRUE)) {
      data_df <- read.csv(data_path, stringsAsFactors = TRUE)
    } else if (grepl("\\.(xlsx|xls)$", data_path, ignore.case = TRUE)) {
      data_df <- read_excel(data_path)
    } else {
      stop("Unsupported file format for data file.")
    }

    # Remove variables with all equal values (e.g., variance zero)
    original_names <- names(data_df)
    data_df <- data_df[, sapply(data_df, function(x) length(unique(x[!is.na(x)])) > 1), drop = FALSE]

    # Store filtered data
    data(data_df)

    # Show a warning if variables were removed
    removed_vars <- setdiff(original_names, names(data_df))
    if (length(removed_vars) > 0) {
      showNotification(
        paste(
          "The following variables were removed because they contain only one unique value:",
          paste(removed_vars, collapse = ", ")
        ),
        type = "warning"
      )
    }

    # Initialize descriptions with variable names as default descriptions
    default_descriptions <- data.frame(
      variable = names(data_df),
      description = names(data_df),
      stringsAsFactors = FALSE
    )

    # Read the uploaded descriptions if a file is provided
    if (!is.null(input$desc_file)) {
      # 1) read the uploaded file
      desc_path <- input$desc_file$datapath
      if (grepl("\\.csv$", desc_path, ignore.case = TRUE)) {
        user_desc <- read.csv(desc_path, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        user_desc <- read_excel(desc_path)
      }

      # Trim whitespace from column names
      colnames(user_desc) <- trimws(colnames(user_desc))

      # Validate the description file
      validation_passed <- TRUE

      if (length(colnames(user_desc)) != 2) {
        showNotification(
          "The description file must contain exactly two columns named 'Variable' and 'Description'.",
          type = "error",
          duration = NULL
        )
        validation_passed <- FALSE
      } else if (!all(c("Variable", "Description") %in% colnames(user_desc))) {
        showNotification(
          paste(
            "The description file must contain exactly two columns named 'Variable' and 'Description'.",
            "Found columns:", paste(sQuote(colnames(user_desc)), collapse = ", ")
          ),
          type = "error",
          duration = NULL
        )
        validation_passed <- FALSE
      }

      if (validation_passed) {
        # If validation passes, continue with processing
        user_desc <- user_desc |>
          janitor::clean_names() |>
          select(variable, description)

        merged_desc <- default_descriptions |>
          left_join(user_desc, by = "variable") |>
          mutate(
            description = ifelse(
              is.na(description.y) | description.y == "",
              variable,
              description.y
            )
          ) |>
          select(variable, description)
        var_descriptions(merged_desc)
      } else {
        var_descriptions(default_descriptions)
      }
    } else {
      var_descriptions(default_descriptions)
    }

    # Redirect to the Variables tab after processing the data
    updateTabsetPanel(session, "main_tabs", selected = "variables_tab")
  })

  # Optional survey weight selection (numeric variables only)
  output$weight_var_ui <- renderUI({
    req(data())
    numeric_vars <- names(data())[sapply(data(), is.numeric)]

    selectInput(
      inputId = "weight_var",
      label = "(Optional) Select survey weight variable:",
      choices = c("None" = "", numeric_vars),
      selected = "",
      width = "100%"
    )
  })

  output$variable_checkboxes_ui <- renderUI({
    req(data())

    all_vars <- names(data())
    wvar <- input$weight_var
    if (!is.null(wvar) && nzchar(wvar)) {
      all_vars <- setdiff(all_vars, wvar)
    }

    selectizeInput(
      inputId = "selected_vars",
      label = "Select variables to include:",
      choices = all_vars,
      selected = all_vars,
      multiple = TRUE,
      width = "100%",
      options = list(
        maxItems = NULL,
        plugins = list("remove_button"),
        placeholder = "Choose variables...",
        openOnFocus = TRUE
      )
    )
  })

  valid_selected_vars <- reactive({
    req(input$selected_vars)
    input$selected_vars
  })

  output$go_to_network_ui <- renderUI({
    req(input$selected_vars)
    actionButton("go_to_network", "Visualize all associations", class = "btn btn-primary")
  })

  output$selected_vars_table_ui <- renderUI({
    req(input$selected_vars)
    # hide the table unless the user has uploaded a custom descriptions file
    req(input$desc_file)
    reactableOutput("selected_vars_table")
  })

  output$selected_vars_table <- renderReactable({
    req(var_descriptions())
    req(valid_selected_vars())

    df <- tibble(variable = valid_selected_vars()) |>
      left_join(var_descriptions(), by = "variable")

    cols <- list(
      variable = colDef(name = "Variable", minWidth = 150),
      description = colDef(name = "Description", html = TRUE, minWidth = 400)
    )

    make_table(df, cols)
  })

  observeEvent(input$go_to_network, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "network_tab")
  })

  observeEvent(input$go_to_pairs, {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "pairs_tab")
  })

  cor_matrix_reactive <- reactive({
    req(data())
    selected_vars <- valid_selected_vars()
    selected_data <- data()[, selected_vars, drop = FALSE]

    # extract weights (if any) aligned with the full dataset
    w <- NULL
    wvar <- input$weight_var
    if (!is.null(wvar) && nzchar(wvar) && wvar %in% names(data())) {
      w <- data()[[wvar]]
    }

    calculate_correlations(selected_data, input$threshold_num, input$threshold_cat, weights = w)
  })

  cor_matrix_vals <- reactive({
    cor_matrix_reactive()
  })

  filtered_data_for_pairs <- reactive({
    mat <- cor_matrix_vals()$cor_matrix
    nodes_to_keep <- rowSums(abs(mat) > 0) > 1
    filtered_matrix <- mat[nodes_to_keep, nodes_to_keep, drop = FALSE]

    vars <- colnames(filtered_matrix)
    df <- data()[, vars, drop = FALSE]

    wvar <- input$weight_var
    if (!is.null(wvar) && nzchar(wvar) && wvar %in% names(data())) {
      df$.weight <- data()[[wvar]]
    }

    df
  })

  significant_pairs <- reactive({
    mat <- cor_matrix_vals()$cor_matrix
    nodes_to_keep <- rowSums(abs(mat) > 0) > 1
    filtered_matrix <- mat[nodes_to_keep, nodes_to_keep]
    pairs <- which(filtered_matrix != 0 & upper.tri(filtered_matrix), arr.ind = TRUE)
    if (nrow(pairs) == 0) {
      return(NULL)
    }
    data.frame(
      var1 = rownames(filtered_matrix)[pairs[, 1]],
      var2 = colnames(filtered_matrix)[pairs[, 2]],
      stringsAsFactors = FALSE
    )
  })

  output$network_vis <- renderVisNetwork({
    cor_result <- cor_matrix_reactive()
    cor_matrix <- cor_result$cor_matrix
    cor_type_matrix <- cor_result$cor_type_matrix

    nodes_to_keep <- rowSums(abs(cor_matrix) > 0) > 1
    mat <- cor_matrix[nodes_to_keep, nodes_to_keep]
    type_mat <- cor_type_matrix[nodes_to_keep, nodes_to_keep]

    validate(
      need(
        ncol(mat) > 0,
        "No associations above the thresholds. Please adjust the thresholds or select different variables."
      )
    )

    # 1) Prepare nodes with descriptions instead of names
    nodes <- data.frame(id = colnames(mat), stringsAsFactors = FALSE) |>
      left_join(var_descriptions(), by = c("id" = "variable")) |>
      mutate(
        label = id, # keep the variable code as label
        title = description, # on hover, the description will be shown
        size = 15 # default size
      ) |>
      select(id, label, title, size)

    # 2) Prepare edges with appropriate correlation type
    edgelist <- which(mat != 0 & upper.tri(mat), arr.ind = TRUE)
    edges <- data.frame(
      from = rownames(mat)[edgelist[, 1]],
      to = colnames(mat)[edgelist[, 2]],
      width = 1 + 4 * (abs(mat[edgelist]) - min(abs(mat[edgelist]))) / (max(abs(mat[edgelist])) - min(abs(mat[edgelist]))),
      color = ifelse(mat[edgelist] > 0, "steelblue", "darkred"),
      title = paste0(type_mat[edgelist], " = ", round(mat[edgelist], 2)),
      stringsAsFactors = FALSE
    )

    # Adjust edge lengths based on association strengths
    strengths <- abs(mat[edgelist])
    min_len <- 100 # min length (strong association)
    max_len <- 500 # max length (weak association)
    edges$length <- (1 - strengths) * (max_len - min_len) + min_len

    # 3) Build the plot
    visNetwork(nodes, edges, width = "100%", height = "900px") |>
      visNodes(
        color = list(
          background = "lightgray",
          border = "lightgray",
          highlight = list(border = "darkgray", background = "darkgray")
        )
      ) |>
      visEdges(smooth = FALSE) |>
      visPhysics(
        enabled = TRUE,
        stabilization = TRUE,
        solver = "forceAtlas2Based"
      ) |>
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = FALSE,
        manipulation = FALSE
      ) |>
      visInteraction(
        zoomView = TRUE,
        dragView = FALSE,
        navigationButtons = FALSE
      ) |>
      visLayout(randomSeed = 123) |>
      visExport(
        type = "png",
        name = "correlation_network",
        label = "Download plot",
        style = "background-color:transparent; color:#555555;
           border:1px solid #bbbbbb; font-size:12px;
           padding:3px 8px; border-radius:4px;
           cursor:pointer;"
      )
  })

  output$pairs_plot <- renderUI({
    req(input$main_tabs == "pairs_tab")
    pairs <- significant_pairs()
    if (is.null(pairs) || nrow(pairs) == 0) {
      return(tags$p(
        "No variable pairs exceed the threshold to display bivariate plots. Please adjust the thresholds or select different variables.",
        style = "color: gray;"
      ))
    }
    df <- filtered_data_for_pairs()
    tabs <- lapply(seq_len(nrow(pairs)), function(i) {
      v1 <- pairs$var1[i]
      v2 <- pairs$var2[i]

      # Get the descriptions
      desc1 <- var_descriptions()$description[var_descriptions()$variable == v1]
      desc2 <- var_descriptions()$description[var_descriptions()$variable == v2]

      plotname <- paste0("plot_", i)
      is_num1 <- is.numeric(df[[v1]])
      is_num2 <- is.numeric(df[[v2]])

      # Create a clean subset without NAs for these variables
      plot_data <- df %>%
        filter(!is.na(.data[[v1]]), !is.na(.data[[v2]]))

      # Numeric vs numeric case
      if (is_num1 && is_num2) {
        # Calculate correlation only with complete cases
        current_cor <- if (nrow(plot_data) > 0) {
          cor(plot_data[[v1]], plot_data[[v2]], use = "complete.obs")
        } else {
          NA
        }

        output[[plotname]] <- renderPlot({
          if (nrow(plot_data) > 0) {
            p <- ggplot(plot_data, aes(x = .data[[v1]], y = .data[[v2]])) +
              geom_jitter(
                alpha = 0.6,
                color = "steelblue",
                width = 0.5,
                height = 0.5
              )

            if (".weight" %in% names(plot_data)) {
              p <- p +
                geom_smooth(
                  method = "lm",
                  se = FALSE,
                  aes(weight = .weight),
                  color = "darkred",
                  linewidth = 1
                )
            } else {
              p <- p +
                geom_smooth(
                  method = "lm",
                  se = FALSE,
                  color = "darkred",
                  linewidth = 1
                )
            }

            p +
              labs(
                x = desc1,
                y = desc2
              ) +
              scale_x_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
              scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
              theme_minimal(base_size = 14)
          } else {
            plot.new()
            text(0.5, 0.5, "No valid data available",
              cex = 1.5, adj = 0.5
            )
          }
        })
        nav_panel(
          paste0(v1, " vs ", v2),
          div(
            id = paste0("plot_container_", i),
            plotOutput(plotname, height = "600px")
          ),
          div(
            class = "download-container",
            actionButton(
              inputId = paste0("download_plot_", i),
              label = "Download plot",
              class = "btn download-btn",
              onclick = paste0("captureAndDownload('plot_container_", i, "', '", v1, "_vs_", v2, ".png')")
            )
          )
        )
      }

      # Categorical vs categorical case (table)
      else if (!is_num1 && !is_num2) {
        output[[plotname]] <- renderUI({
          if (nrow(plot_data) > 0) {
            build_contingency_table(plot_data, v1, v2)
          } else {
            div("No valid data available", style = "padding: 20px; text-align: center;")
          }
        })
        nav_panel(
          paste0(v1, " vs ", v2),
          div(
            id = paste0("table_container_", i),
            uiOutput(plotname)
          ),
          div(
            class = "download-container",
            actionButton(
              inputId = paste0("download_table_", i),
              label = "Download table",
              class = "btn download-btn",
              onclick = paste0("captureAndDownload('table_container_", i, "', '", v1, "_vs_", v2, ".png')")
            )
          )
        )
      }

      # Mixed case (numeric vs categorical)
      else {
        if (is_num1) {
          num_var <- v1
          cat_var <- v2
          desc_num <- desc1
          desc_cat <- desc2
        } else {
          num_var <- v2
          cat_var <- v1
          desc_num <- desc2
          desc_cat <- desc1
        }

        output[[plotname]] <- renderPlot({
          if (nrow(plot_data) > 0) {
            if (".weight" %in% names(plot_data)) {
              df_sum <- plot_data |>
                filter(!is.na(.data$.weight)) |>
                group_by(.data[[cat_var]]) |>
                summarise(
                  mean_val = weighted.mean(.data[[num_var]], w = .data$.weight, na.rm = TRUE),
                  .groups = "drop"
                ) |>
                arrange(mean_val) |>
                mutate({{ cat_var }} := factor(.data[[cat_var]], levels = .data[[cat_var]]))
            } else {
              df_sum <- plot_data |>
                group_by(.data[[cat_var]]) |>
                summarise(
                  mean_val = mean(.data[[num_var]], na.rm = TRUE),
                  .groups = "drop"
                ) |>
                arrange(mean_val) |>
                mutate({{ cat_var }} := factor(.data[[cat_var]], levels = .data[[cat_var]]))
            }

            ggplot(df_sum, aes(x = .data[[cat_var]], y = mean_val)) +
              geom_col(fill = "steelblue", width = 0.6) +
              geom_text(
                aes(label = format(round(mean_val, 2),
                  big.mark = ",", decimal.mark = "."
                )),
                hjust = 1.1, color = "white", size = 4
              ) +
              labs(
                x = desc_cat,
                y = paste0('Mean of "', desc_num, '"')
              ) +
              scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".")) +
              theme_minimal(base_size = 14) +
              coord_flip()
          } else {
            plot.new()
            text(0.5, 0.5, "No valid data available",
              cex = 1.5, adj = 0.5
            )
          }
        })
        nav_panel(
          paste0(v1, " vs ", v2),
          div(
            id = paste0("plot_container_", i),
            plotOutput(plotname, height = "600px")
          ),
          div(
            class = "download-container",
            actionButton(
              inputId = paste0("download_plot_", i),
              label = "Download plot",
              class = "btn download-btn",
              onclick = paste0("captureAndDownload('plot_container_", i, "', '", v1, "_vs_", v2, ".png')")
            )
          )
        )
      }
    })
    tagList(navset_card_tab(id = "bivariate_tabs", !!!tabs))
  })

  weighted_cor <- function(x, y, w) {
    # Remove rows with missing x, y or w
    ok <- !is.na(x) & !is.na(y) & !is.na(w)
    x <- x[ok]
    y <- y[ok]
    w <- w[ok]

    if (length(x) < 2 || sum(w) <= 0) {
      return(NA_real_)
    }

    w_norm <- w / sum(w)
    mx <- sum(w_norm * x)
    my <- sum(w_norm * y)
    cov_xy <- sum(w_norm * (x - mx) * (y - my))
    vx <- sum(w_norm * (x - mx)^2)
    vy <- sum(w_norm * (y - my)^2)

    if (vx <= 0 || vy <= 0) {
      return(NA_real_)
    }
    cov_xy / sqrt(vx * vy)
  }

  # Function to calculate correlation matrix based on variable types
  calculate_correlations <- function(data, threshold_num, threshold_cat, weights = NULL) {
    vars <- names(data)
    n <- length(vars)
    cor_matrix <- matrix(0, n, n, dimnames = list(vars, vars))
    cor_type_matrix <- matrix("", n, n, dimnames = list(vars, vars)) # Matrix to store correlation type
    combs <- combn(vars, 2, simplify = FALSE)

    # unpack ranges
    num_min <- threshold_num[1]
    num_max <- threshold_num[2]
    cat_min <- threshold_cat[1]
    cat_max <- threshold_cat[2]

    for (pair in combs) {
      v1 <- pair[1]
      v2 <- pair[2]
      is_num1 <- is.numeric(data[[v1]])
      is_num2 <- is.numeric(data[[v2]])
      cor_val <- 0
      cor_type <- ""

      # Get complete cases for these two variables
      complete_cases <- complete.cases(data[[v1]], data[[v2]])
      x <- data[[v1]][complete_cases]
      y <- data[[v2]][complete_cases]

      # Subset weights for these complete cases (if provided)
      w <- NULL
      if (!is.null(weights)) {
        w <- weights[complete_cases]
      }

      # Numeric vs numeric case
      if (is_num1 && is_num2) {
        if (length(x) > 0 && length(y) > 0) {
          if (!is.null(w)) {
            r <- weighted_cor(x, y, w)
          } else {
            r <- cor(x, y, use = "complete.obs")
          }
          r2 <- r^2
          if (!is.na(r2) && r2 >= num_min && r2 <= num_max) {
            cor_val <- r # keep sign
            cor_type <- "Pearson's r"
          }
        }

        # Categorical vs categorical case
      } else if (!is_num1 && !is_num2) {
        if (length(x) > 0 && length(y) > 0) {
          if (!is.null(w)) {
            # Weighted contingency table
            df_tmp <- data.frame(x = x, y = y, w = w)
            df_tmp <- df_tmp[!is.na(df_tmp$w), ]
            if (nrow(df_tmp) > 0) {
              tbl <- xtabs(w ~ x + y, data = df_tmp)
            } else {
              tbl <- table(x, y)
            }
          } else {
            tbl <- table(x, y)
          }

          if (nrow(tbl) > 1 && ncol(tbl) > 1) { # Need at least 2 categories in each
            chi <- tryCatch(
              chisq.test(tbl, simulate.p.value = TRUE),
              error = function(e) NULL
            )
            if (!is.null(chi)) {
              n_obs <- sum(tbl)
              df_min <- min(nrow(tbl) - 1, ncol(tbl) - 1)
              if (df_min > 0) {
                v_cramer <- sqrt(chi$statistic / (n_obs * df_min))
                if (!is.na(v_cramer) && v_cramer >= cat_min && v_cramer <= cat_max) {
                  cor_val <- v_cramer
                  cor_type <- "Cramer's V"
                }
              }
            }
          }
        }

        # Mixed case (numeric vs categorical)
      } else {
        if (is_num1) {
          num_var <- x
          cat_var <- y
          w_pair <- w
        } else {
          num_var <- y
          cat_var <- x
          w_pair <- w
        }

        complete_non_na <- !is.na(num_var) & !is.na(cat_var)
        num_var <- num_var[complete_non_na]
        cat_var <- cat_var[complete_non_na]
        if (!is.null(w_pair)) {
          w_pair <- w_pair[complete_non_na]
        }

        if (length(num_var) > 0 && length(cat_var) > 0) {
          if (!is.null(w_pair)) {
            # Weighted correlation ratio (eta)
            df_tmp <- data.frame(
              num = num_var,
              cat = cat_var,
              w = w_pair
            )

            df_tmp <- df_tmp[!is.na(df_tmp$w), ]
            if (nrow(df_tmp) > 0) {
              # overall weighted mean
              overall_mean <- with(df_tmp, sum(w * num) / sum(w))

              # group weights and means
              group_w <- tapply(df_tmp$w, df_tmp$cat, sum)
              group_num <- tapply(df_tmp$w * df_tmp$num, df_tmp$cat, sum)
              means_by_group <- group_num / group_w

              bss <- sum(group_w * (means_by_group - overall_mean)^2, na.rm = TRUE)
              tss <- sum(df_tmp$w * (df_tmp$num - overall_mean)^2, na.rm = TRUE)
            } else {
              bss <- NA_real_
              tss <- NA_real_
            }
          } else {
            # Unweighted eta
            means_by_group <- tapply(num_var, cat_var, mean, na.rm = TRUE)
            overall_mean <- mean(num_var, na.rm = TRUE)
            n_groups <- tapply(num_var, cat_var, length)
            bss <- sum(n_groups * (means_by_group - overall_mean)^2, na.rm = TRUE)
            tss <- sum((num_var - overall_mean)^2, na.rm = TRUE)
          }

          if (!is.na(tss) && tss > 0) {
            eta <- sqrt(bss / tss)
            eta2 <- eta^2
            if (!is.na(eta2) && eta2 >= num_min && eta2 <= num_max) {
              cor_val <- eta
              cor_type <- "Eta"
            }
          }
        }
      }

      cor_matrix[v1, v2] <- cor_matrix[v2, v1] <- cor_val
      cor_type_matrix[v1, v2] <- cor_type_matrix[v2, v1] <- cor_type
    }

    diag(cor_matrix) <- 1
    list(cor_matrix = cor_matrix, cor_type_matrix = cor_type_matrix)
  }

  make_table <- function(df, columns_defs) {
    reactable(
      df,
      columns = columns_defs,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50),
      theme = reactableTheme(headerStyle = list(fontWeight = "bold"))
    )
  }

  # Fonction to build contigency table
  build_contingency_table <- function(df, v1, v2) {
    desc1 <- var_descriptions()$description[var_descriptions()$variable == v1]
    desc2 <- var_descriptions()$description[var_descriptions()$variable == v2]

    # Ensure df has the correct columns
    if (!v1 %in% colnames(df) || !v2 %in% colnames(df)) {
      return(div("Invalid variable names for contingency table", style = "color:red"))
    }

    # Create a version of the data with NAs removed for the contingency table
    df_clean <- df[complete.cases(df[, c(v1, v2)]), ]

    if (nrow(df_clean) == 0) {
      return(div("No valid data available", style = "padding: 20px; text-align: center;"))
    }

    # weighted table if .weight exists
    if (".weight" %in% names(df_clean)) {
      df_clean <- df_clean[!is.na(df_clean$.weight), ]
      if (nrow(df_clean) == 0) {
        return(div("No valid data available", style = "padding: 20px; text-align: center;"))
      }
      form <- as.formula(paste(".weight ~", v1, "+", v2))
      tbl <- xtabs(form, data = df_clean)
    } else {
      tbl <- table(df_clean[[v1]], df_clean[[v2]])
    }

    if (length(tbl) == 0) {
      return(div("No valid data available", style = "padding: 20px; text-align: center;"))
    }

    tbl_with_margins <- addmargins(tbl)

    df_table <- as.data.frame.matrix(tbl_with_margins)
    df_table <- tibble::rownames_to_column(df_table, var = desc1)

    inner_vals <- tbl
    min_val <- min(inner_vals)
    max_val <- max(inner_vals)
    pal <- colorRampPalette(c("#e1f5fe", "#0288d1"))(100)

    column_defs <- lapply(seq_along(df_table), function(j) {
      colname <- names(df_table)[j]
      if (colname == desc1) {
        colDef(name = desc1, minWidth = 150)
      } else {
        colDef(
          name = colname,
          align = "center",
          cell = function(value, index) {
            is_total <- df_table[[desc1]][index] == "Sum" || colname == "Sum"
            val <- as.numeric(value)

            # rounding: 0 = nearest integer
            val_rounded <- round(val, 0)

            label <- format(
              val_rounded,
              big.mark = ",",
              decimal.mark = ".",
              scientific = FALSE
            )

            if (is_total) {
              # totals without background
              return(label)
            }
            idx <- if (max_val > min_val) {
              max(1, min(100, floor(99 * (val - min_val) / (max_val - min_val)) + 1))
            } else {
              50
            }
            div(style = paste0("background-color:", pal[idx], "; padding:4px;"), label)
          }
        )
      }
    })
    names(column_defs) <- names(df_table)

    tagList(
      div(class = "reactable-title", desc2),
      make_table(df_table, column_defs)
    )
  }
}

shinyApp(ui, server)
