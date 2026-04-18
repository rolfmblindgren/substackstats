library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(jsonlite)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)

read_uploaded_posts <- function(files_df) {
  if (is.null(files_df) || nrow(files_df) == 0) return(NULL)

  exts <- tolower(tools::file_ext(files_df$name))
  idx_csv <- which(exts == "csv")
  idx_json <- which(exts == "json")

  if (length(idx_csv) > 0 && length(idx_json) > 0) {
    df_csv <- read_posts_file(files_df$datapath[[idx_csv[[1]]]])
    df_json <- read_posts_file(files_df$datapath[[idx_json[[1]]]]) %>%
      select(id, audience, slug, cover_image, type)

    return(df_csv %>%
      left_join(df_json, by = "id", suffix = c("", ".json")) %>%
      mutate(
        audience_clean = na_if(na_if(audience, "Ukjent"), "0"),
        audience = coalesce(audience_clean, `audience.json`, "Ukjent"),
        slug = coalesce(slug, `slug.json`),
        cover_image = coalesce(cover_image, `cover_image.json`),
        type = coalesce(type, `type.json`)
      ) %>%
      select(-audience_clean, -ends_with(".json")))
  }

  read_posts_file(files_df$datapath[[1]])
}

safe_div <- function(x, y) {
  ifelse(is.na(y) | y == 0, NA_real_, x / y)
}

extract_stats_df <- function(df) {
  if (!"stats" %in% names(df)) return(df)

  stats_tbl <- purrr::map_dfr(df$stats, function(x) {
    if (is.null(x)) return(tibble())
    as_tibble(x)
  })

  df %>%
    select(-stats) %>%
    bind_cols(stats_tbl)
}

normalize_posts <- function(df) {
  df <- extract_stats_df(df)

  wanted <- c(
    "id", "title", "audience", "post_date", "email_sent_at", "is_published",
    "views", "opens", "opened", "open_rate", "clicks", "clicked", "sent",
    "delivered", "signups", "subscribes", "subscriptions_within_1_day",
    "signups_within_1_day", "shares", "estimated_value", "click_through_rate",
    "engagement_rate", "likes", "comments", "restacks", "reaction_count",
    "cover_image", "slug", "type"
  )

  for (nm in wanted) {
    if (!nm %in% names(df)) df[[nm]] <- NA
  }

  df %>%
    mutate(
      post_date = suppressWarnings(ymd_hms(post_date, quiet = TRUE)),
      email_sent_at = suppressWarnings(ymd_hms(email_sent_at, quiet = TRUE)),
      audience = case_when(
        audience %in% c("only_paid", "paid") ~ "Kun betalende",
        audience %in% c("everyone", "free") ~ "Alle",
        TRUE ~ coalesce(as.character(audience), "Ukjent")
      ),
      title = coalesce(title, "Uten tittel"),
      views = as.numeric(views),
      opens = as.numeric(opens),
      opened = as.numeric(opened),
      sent = as.numeric(sent),
      delivered = as.numeric(delivered),
      clicks = as.numeric(clicks),
      clicked = as.numeric(clicked),
      signups = as.numeric(signups),
      subscribes = as.numeric(subscribes),
      subscriptions_within_1_day = as.numeric(subscriptions_within_1_day),
      signups_within_1_day = as.numeric(signups_within_1_day),
      shares = as.numeric(shares),
      estimated_value = as.numeric(estimated_value),
      engagement_rate = as.numeric(engagement_rate),
      click_through_rate = as.numeric(click_through_rate),
      open_rate = as.numeric(open_rate),
      likes = as.numeric(likes),
      comments = as.numeric(comments),
      restacks = as.numeric(restacks),
      reaction_count = as.numeric(reaction_count),
      paid_conv_per_view = safe_div(coalesce(subscribes, 0), views),
      signup_per_view = safe_div(coalesce(signups, 0), views),
      paid_plus_signup_per_view = safe_div(coalesce(subscribes, 0) + coalesce(signups, 0), views),
      total_reactions = coalesce(likes, 0) + coalesce(comments, 0) + coalesce(restacks, 0) + coalesce(reaction_count, 0),
      published_day = as.Date(post_date),
      weekday = if_else(is.na(post_date), NA_character_, weekdays(post_date)),
      month = if_else(is.na(post_date), NA_character_, format(post_date, "%Y-%m"))
    ) %>%
    arrange(desc(post_date))
}

read_posts_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "json") {
    raw <- jsonlite::fromJSON(path, flatten = TRUE)
    if (is.data.frame(raw)) return(normalize_posts(as_tibble(raw)))
    if (is.list(raw)) return(normalize_posts(bind_rows(raw)))
  }

  if (ext == "csv") {
    raw <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    return(normalize_posts(as_tibble(raw)))
  }

  stop("Filtypen støttes ikke. Bruk JSON eller CSV.")
}

metric_card <- function(title, value, subtitle = NULL) {
  div(
    style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:12px;padding:16px;height:100%;",
    tags$div(style = "font-size:0.95rem;color:#555;", title),
    tags$div(style = "font-size:1.8rem;font-weight:700;margin-top:6px;", value),
    if (!is.null(subtitle)) tags$div(style = "font-size:0.9rem;color:#777;margin-top:6px;", subtitle)
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-size: 16px; }
      .container-fluid { max-width: 1500px; }
      .shiny-notification { position: fixed; top: 12px; right: 12px; }
    "))
  ),

  titlePanel("Substack-statistikk"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput(
        "files",
        "Velg posts.json og/eller user_posts.csv",
        accept = c(".json", ".csv"),
        multiple = TRUE
      ),
      numericInput("paid_current", "Aktive betalende nå (valgfritt)", value = NA, min = 0, step = 1),
      checkboxInput("published_only", "Vis bare publiserte poster", TRUE),
      uiOutput("audience_ui"),
      dateRangeInput("daterange", "Datointervall"),
      textInput("title_search", "Filtrer på tittel", placeholder = "f.eks. WISC, ADHD, HR"),
      helpText("Tips: user_posts.csv har ofte mest tall. posts.json har ofte «Tilgang» (Alle / Kun betalende). Hvis du velger begge, slår appen dem sammen.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Oversikt",
          br(),
          fluidRow(
            column(3, uiOutput("card_posts")),
            column(3, uiOutput("card_views")),
            column(3, uiOutput("card_open_rate")),
            column(3, uiOutput("card_paid"))
          ),
          br(),
          fluidRow(
            column(6, plotlyOutput("views_time_plot", height = 350)),
            column(6, plotlyOutput("conv_scatter", height = 350))
          ),
          br(),
          fluidRow(
            column(6, plotlyOutput("weekday_plot", height = 320)),
            column(6, plotlyOutput("audience_plot", height = 320))
          )
        ),

        tabPanel(
          "Poster",
          br(),
          DTOutput("posts_table")
        ),

        tabPanel(
          "Sammenlikning",
          br(),
          fluidRow(
            column(6, plotlyOutput("top_views_plot", height = 400)),
            column(6, plotlyOutput("top_engagement_plot", height = 400))
          ),
          br(),
          fluidRow(
            column(6, plotlyOutput("top_paid_plot", height = 400)),
            column(6, plotlyOutput("ctr_plot", height = 400))
          )
        ),

        tabPanel(
          "Oppsummering",
          br(),
          verbatimTextOutput("summary_text")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  posts_raw <- reactive({
    req(input$files)
    df <- read_uploaded_posts(input$files)
    shiny::validate(shiny::need(!is.null(df), "Velg minst én fil (posts.json og/eller roffe_posts.csv)."))
    df
  })

  observeEvent(posts_raw(), {
    df <- posts_raw()
    dates <- df$published_day
    dates <- dates[!is.na(dates)]
    if (length(dates) == 0) return(NULL)
    updateDateRangeInput(
      session,
      "daterange",
      start = min(dates),
      end = max(dates),
      min = min(dates),
      max = max(dates)
    )
  }, ignoreInit = TRUE)

  output$audience_ui <- renderUI({
    req(posts_raw())
    choices <- sort(unique(na.omit(posts_raw()$audience)))
    choices <- choices[choices != "0"]
    if (length(choices) == 0 || all(choices == "Ukjent")) {
      return(helpText("Tilgang mangler i datasettet. Bruk Auto (slå sammen) med både posts.json + roffe_posts.csv, eller velg posts.json."))
    }
    checkboxGroupInput("audience", "Tilgang", choices = choices, selected = choices)
  })

  posts <- reactive({
    df <- posts_raw()

    if (isTRUE(input$published_only) && "is_published" %in% names(df)) {
      df <- df %>% filter(is.na(is_published) | is_published)
    }

    if (!is.null(input$audience) && length(input$audience) > 0) {
      df <- df %>% filter(audience %in% input$audience)
    }

    if (!is.null(input$daterange) && all(!is.na(input$daterange))) {
      df <- df %>% filter(is.na(published_day) | (published_day >= input$daterange[1] & published_day <= input$daterange[2]))
    }

    if (!is.null(input$title_search) && nzchar(trimws(input$title_search))) {
      pattern <- str_to_lower(trimws(input$title_search))
      df <- df %>% filter(str_detect(str_to_lower(title), fixed(pattern)))
    }

    df
  })

  output$card_posts <- renderUI({
    df <- posts()
    metric_card("Poster", nrow(df))
  })

  output$card_views <- renderUI({
    df <- posts()
    metric_card("Visninger", comma(sum(df$views, na.rm = TRUE)))
  })

  output$card_open_rate <- renderUI({
    df <- posts()
    avg_open <- mean(df$open_rate, na.rm = TRUE)
    metric_card("Snitt åpningsrate", percent(avg_open, accuracy = 0.1))
  })

  output$card_paid <- renderUI({
    df <- posts()
    conversions <- sum(df$subscribes, na.rm = TRUE)
    signups <- sum(df$signups, na.rm = TRUE)

    if (!is.null(input$paid_current) && is.finite(input$paid_current)) {
      return(metric_card(
        "Aktive betalende nå",
        comma(as.numeric(input$paid_current)),
        subtitle = paste0("Nye betalende (konverteringer): ", comma(conversions), " | Signups: ", comma(signups))
      ))
    }

    metric_card(
      "Nye betalende (konverteringer)",
      comma(conversions),
      subtitle = paste0("Summerer per post. Signups: ", comma(signups))
    )
  })

  output$views_time_plot <- renderPlotly({
    df <- posts() %>% filter(!is.na(post_date)) %>% arrange(post_date)
    shiny::validate(shiny::need(nrow(df) > 0, "Ingen poster med dato (post_date) etter filtrering."))
    p <- ggplot(df, aes(post_date, views, color = audience, text = title)) +
      geom_line(alpha = 0.5) +
      geom_point(size = 2) +
      labs(x = NULL, y = "Visninger", color = "Tilgang") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("text", "x", "y"))
  })

  output$conv_scatter <- renderPlotly({
    df <- posts() %>% filter(!is.na(views), views > 0)
    shiny::validate(shiny::need(nrow(df) > 0, "Ingen poster med visninger > 0 etter filtrering."))
    p <- ggplot(df, aes(views, paid_plus_signup_per_view, color = audience, size = total_reactions, text = title)) +
      geom_point(alpha = 0.8) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Visninger", y = "(Signups + betalende) per visning", color = "Tilgang", size = "Reaksjoner") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("text", "x", "y", "size"))
  })

  output$weekday_plot <- renderPlotly({
    df <- posts() %>% filter(!is.na(weekday)) %>%
      group_by(weekday) %>%
      summarise(mean_views = mean(views, na.rm = TRUE), .groups = "drop")
    shiny::validate(shiny::need(nrow(df) > 0, "Ingen poster med ukedag (weekday) etter filtrering."))
    p <- ggplot(df, aes(reorder(weekday, mean_views), mean_views)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Gjennomsnittlige visninger") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })

  output$audience_plot <- renderPlotly({
    df <- posts() %>%
      group_by(audience) %>%
      summarise(
        posts = n(),
        mean_views = mean(views, na.rm = TRUE),
        mean_open = mean(open_rate, na.rm = TRUE),
        mean_eng = mean(engagement_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(mean_views, mean_open, mean_eng), names_to = "metric", values_to = "value")

    shiny::validate(shiny::need(nrow(df) > 0, "Ingen data å vise etter filtrering."))
    p <- ggplot(df, aes(audience, value, fill = metric, text = paste(audience, metric, round(value, 3)))) +
      geom_col(position = "dodge") +
      facet_wrap(~metric, scales = "free_y") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  output$posts_table <- renderDT({
    df <- posts() %>%
      transmute(
        Dato = as.character(published_day),
        Tittel = title,
        Tilgang = audience,
        Visninger = views,
        `Åpningsrate` = percent(open_rate, accuracy = 0.1),
        `CTR` = percent(click_through_rate, accuracy = 0.1),
        `Engasjement` = percent(engagement_rate, accuracy = 0.1),
        Signups = signups,
        Betalende = subscribes,
        Delinger = shares,
        Kommentarer = comments,
        Likes = likes
      )

    datatable(df, rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE))
  })

  output$top_views_plot <- renderPlotly({
    df <- posts() %>% slice_max(order_by = views, n = 10) %>% arrange(views)
    shiny::validate(shiny::need(nrow(df) > 0, "Ingen poster etter filtrering."))
    p <- ggplot(df, aes(views, reorder(title, views), text = title)) +
      geom_col() +
      labs(x = "Visninger", y = NULL) +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("text", "x"))
  })

  output$top_engagement_plot <- renderPlotly({
    df <- posts() %>% filter(!is.na(engagement_rate)) %>% slice_max(order_by = engagement_rate, n = 10) %>% arrange(engagement_rate)
    shiny::validate(shiny::need(nrow(df) > 0, "Ingen poster med engasjementsrate etter filtrering."))
    p <- ggplot(df, aes(engagement_rate, reorder(title, engagement_rate), text = title)) +
      geom_col() +
      scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Engasjementsrate", y = NULL) +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("text", "x"))
  })

  output$top_paid_plot <- renderPlotly({
    df <- posts() %>% filter(coalesce(subscribes, 0) + coalesce(signups, 0) > 0) %>%
      mutate(total_conv = coalesce(subscribes, 0) + coalesce(signups, 0)) %>%
      slice_max(order_by = total_conv, n = 10) %>% arrange(total_conv)

    shiny::validate(shiny::need(nrow(df) > 0, "Ingen poster med signups/betalende > 0 etter filtrering."))
    p <- ggplot(df, aes(total_conv, reorder(title, total_conv), text = title, fill = audience)) +
      geom_col() +
      labs(x = "Signups + betalende", y = NULL, fill = "Tilgang") +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("text", "x"))
  })

  output$ctr_plot <- renderPlotly({
    df <- posts() %>% filter(!is.na(click_through_rate)) %>% slice_max(order_by = click_through_rate, n = 10) %>% arrange(click_through_rate)
    shiny::validate(shiny::need(nrow(df) > 0, "Ingen poster med klikkrate (CTR) etter filtrering."))
    p <- ggplot(df, aes(click_through_rate, reorder(title, click_through_rate), text = title)) +
      geom_col() +
      scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Klikkrate", y = NULL) +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = c("text", "x"))
  })

  output$summary_text <- renderText({
    df <- posts()
    if (nrow(df) == 0) return("Ingen poster matcher filteret.")

    best_views <- df %>% slice_max(order_by = views, n = 1)
    best_eng <- df %>% slice_max(order_by = engagement_rate, n = 1)
    best_paid <- df %>% mutate(total_conv = coalesce(signups, 0) + coalesce(subscribes, 0)) %>% slice_max(order_by = total_conv, n = 1)

    paid_current_line <- ""
    if (!is.null(input$paid_current) && is.finite(input$paid_current)) {
      paid_current_line <- paste0("Aktive betalende nå (manuelt): ", comma(as.numeric(input$paid_current)), "\n")
    }

    paste0(
      "Antall poster i utvalget: ", nrow(df), "\n\n",
      "Gjennomsnittlig åpningsrate: ", percent(mean(df$open_rate, na.rm = TRUE), accuracy = 0.1), "\n",
      "Gjennomsnittlig engasjementsrate: ", percent(mean(df$engagement_rate, na.rm = TRUE), accuracy = 0.1), "\n",
      "Totale visninger: ", comma(sum(df$views, na.rm = TRUE)), "\n",
      "Totale signups: ", comma(sum(df$signups, na.rm = TRUE)), "\n",
      "Nye betalende (konverteringer): ", comma(sum(df$subscribes, na.rm = TRUE)), "\n\n",
      paid_current_line,
      "Mest leste post: ", best_views$title[[1]], " (", comma(best_views$views[[1]]), " visninger)\n",
      "Sterkest engasjement: ", best_eng$title[[1]], " (", percent(best_eng$engagement_rate[[1]], accuracy = 0.1), ")\n",
      "Best konvertering i absolutte tall: ", best_paid$title[[1]], " (",
      coalesce(best_paid$signups[[1]], 0) + coalesce(best_paid$subscribes[[1]], 0), ")\n"
    )
  })
}

shinyApp(ui, server)

# Local Variables:
# mode: R
# End:
