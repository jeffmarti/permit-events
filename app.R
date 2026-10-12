# ==============================================================================
# app.R — WA Water Right Permit Events: Overdue vs Pending (Shiny)
# ==============================================================================
# Maps OPEN development-schedule events (Begin Construction, Complete
# Construction, Proof of Appropriation) on Active permits / Change-ROEs, and
# lets you slide a date to see which deadlines are overdue vs still pending.
#
# DATA is prepared BEFORE publication by pipeline/prepare_app_data.R — users
# never upload anything. The app reads only:
#   data/permit_events.rds   records + "data current as of" info
#   data/boundaries.rds      WRIA + county polygons
# PLSS townships/sections are drawn live from BLM's CadNSDI WMS service.
#
# FOLDER LAYOUT (RStudio project folder):
#   app.R                         <- this file            (deploy)
#   data/permit_events.rds                                (deploy)
#   data/boundaries.rds                                   (deploy)
#   pipeline/                     <- refresh scripts + working files (do NOT deploy)
#   .Renviron                     <- CARTO_API_KEY=...   (do NOT commit to GitHub)
#
# REQUIRED PACKAGES:
# install.packages(c("shiny","leaflet","dplyr","readr","sf","DT","plotly","htmltools"))
# ==============================================================================

library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(DT)
library(plotly)
library(htmltools)

# ==============================================================================
# SETTINGS
# ==============================================================================

APP_DATA_FILE <- "data/permit_events.rds"
BOUNDARY_FILE <- "data/boundaries.rds"

# Status colours (validated as a pair for colour-vision deficiency).
# Overdue = solid red dot; Pending = hollow blue ring (shape is a second cue).
COL_OVERDUE <- "#d03b3b"
COL_PENDING <- "#2a78d6"

# Boundary styling (matches the low-flow-wr app)
COL_WRIA   <- "#1a3a5c"
COL_COUNTY <- "#6a0dad"

# Keep the map on Washington: lowest zoom shows roughly the whole state, and
# panning is held to a box a little larger than the state.
MIN_ZOOM   <- 6
WA_VIEW    <- c(lng1 = -124.8, lat1 = 45.5, lng2 = -116.9, lat2 = 49.0)
WA_MAXBNDS <- c(lng1 = -127.5, lat1 = 44.3, lng2 = -114.5, lat2 = 50.3)

# CARTO basemap key — read from .Renviron (CARTO_API_KEY=...), never typed here.
# Without a key CARTO stamps an "API key required" watermark on its tiles.
CARTO_KEY <- Sys.getenv("CARTO_API_KEY")
message("CARTO_API_KEY ", if (nzchar(CARTO_KEY))
  paste0("found (", nchar(CARTO_KEY), " characters)") else
  "NOT found - Light basemap will show CARTO's watermark")
CARTO_ATTRIB <- paste0(
  "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors, ",
  "&copy; <a href='https://carto.com/attributions'>CARTO</a>"
)

BLM_PLSS_WMS <- paste0("https://gis.blm.gov/arcgis/services/Cadastral/",
                       "BLM_Natl_PLSS_CadNSDI/MapServer/WMSServer")

WRTS_RECORD_URL <- paste0(
  "https://appswr.ecology.wa.gov/waterrighttrackingsystem/WaterRights/",
  "WaterRightRecord.aspx?waRecId="
)

EVENT_ORDER <- c("Begin Construction", "Complete Construction",
                 "Proof of Appropriation")

# ==============================================================================
# LOAD PREPARED DATA (once, at startup)
# ==============================================================================

if (!file.exists(APP_DATA_FILE)) {
  stop("Missing ", APP_DATA_FILE, ". Run pipeline/prepare_app_data.R first.")
}
app_data  <- readRDS(APP_DATA_FILE)
RECORDS   <- app_data$records
DATA_INFO <- app_data$info

boundaries <- if (file.exists(BOUNDARY_FILE)) readRDS(BOUNDARY_FILE) else
  list(wria = NULL, county = NULL)
WRIA_SF   <- boundaries$wria
COUNTY_SF <- boundaries$county
if (is.null(WRIA_SF))   message("NOTE: no WRIA boundaries in ", BOUNDARY_FILE)
if (is.null(COUNTY_SF)) message("NOTE: no county boundaries in ", BOUNDARY_FILE)

# Choices for the filters (data are fixed, so compute once)
PHASE_CHOICES  <- sort(unique(na.omit(RECORDS$phase)))
wria_tbl       <- RECORDS |> distinct(WRIA, wria_label) |> filter(!is.na(WRIA)) |>
  arrange(as.integer(WRIA))
WRIA_CHOICES   <- setNames(wria_tbl$WRIA, wria_tbl$wria_label)
COUNTY_CHOICES <- sort(unique(na.omit(RECORDS$county)))

# Slider range: first due year -> one year past the later of today / last due date
SLIDER_MIN <- as.Date(format(min(RECORDS$due), "%Y-01-01"))
SLIDER_MAX <- as.Date(paste0(as.integer(format(max(max(RECORDS$due), Sys.Date()), "%Y")) + 1,
                             "-12-31"))

DATA_DATE_TXT <- format(DATA_INFO$data_date, "%m/%d/%Y")

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# "3.2 yrs" or "145 days"
fmt_span <- function(days) {
  days <- abs(as.numeric(days))
  if_else(days >= 365,
          paste0(formatC(days / 365.25, format = "f", digits = 1), " yrs"),
          paste0(days, if_else(days == 1, " day", " days")))
}

# Popup HTML for each record, relative to the as-of date
build_popups <- function(df, as_of) {
  days    <- as.numeric(as_of - df$due)
  overdue <- df$due < as_of
  status_html <- if_else(
    overdue,
    paste0("<span style='color:", COL_OVERDUE, ";font-weight:bold;'>&#9679; OVERDUE</span> by ",
           fmt_span(days)),
    paste0("<span style='color:", COL_PENDING, ";font-weight:bold;'>&#9675; PENDING</span> &mdash; due in ",
           fmt_span(days))
  )
  esc  <- function(x) if_else(is.na(x), "&mdash;", htmlEscape(x))
  qq_q <- trimws(gsub("\\s+", " ", paste(coalesce(df$qq, ""), coalesce(df$q, ""))))

  paste0(
    "<div style='font-family:Arial;font-size:12px;min-width:240px;'>",
    "<div style='font-size:14px;font-weight:bold;margin-bottom:2px;'>",
    "<a href='", WRTS_RECORD_URL, df$wr_doc_id, "' target='_blank'>",
    esc(df$doc_number), "</a></div>",
    "<div style='margin-bottom:6px;'>", esc(df$person), "</div>",
    "<div style='margin-bottom:6px;'>", status_html,
    " <span style='color:#52514e;'>(as of ", format(as_of, "%m/%d/%Y"), ")</span></div>",
    "<table style='border-collapse:collapse;'>",
    "<tr><td style='color:#52514e;padding-right:8px;'>Event</td><td>", esc(df$event), "</td></tr>",
    "<tr><td style='color:#52514e;'>Due date</td><td>", format(df$due, "%m/%d/%Y"), "</td></tr>",
    "<tr><td style='color:#52514e;'>Phase / Stage</td><td>", esc(df$phase), " / ", esc(df$stage), "</td></tr>",
    "<tr><td style='color:#52514e;'>Priority date</td><td>", esc(df$priority), "</td></tr>",
    "<tr><td style='color:#52514e;'>Purpose</td><td>", esc(df$purpose), "</td></tr>",
    "<tr><td style='color:#52514e;'>Qi</td><td>", esc(df$qi), " ", coalesce(df$uom, ""), "</td></tr>",
    "<tr><td style='color:#52514e;'>Qa</td><td>", esc(df$qa), "</td></tr>",
    "<tr><td style='color:#52514e;'>Source</td><td>", esc(df$source_name), "</td></tr>",
    "<tr><td style='color:#52514e;'>Location</td><td>", esc(qq_q), " ", esc(df$trs), "</td></tr>",
    "<tr><td style='color:#52514e;'>WRIA</td><td>", esc(df$wria_label), "</td></tr>",
    "<tr><td style='color:#52514e;'>County</td><td>", esc(df$county), "</td></tr>",
    "</table></div>"
  )
}

legend_html <- paste0(
  "<div style='background:white;padding:8px 10px;border-radius:4px;",
  "font-family:Arial;font-size:12px;box-shadow:0 1px 4px rgba(0,0,0,.3);'>",
  "<div style='font-weight:bold;margin-bottom:4px;'>Event status at slider date</div>",
  "<div><svg width='14' height='14'><circle cx='7' cy='7' r='5.5' fill='", COL_OVERDUE,
  "' stroke='white' stroke-width='1.5'/></svg> Overdue (due date has passed)</div>",
  "<div><svg width='14' height='14'><circle cx='7' cy='7' r='5' fill='white' stroke='",
  COL_PENDING, "' stroke-width='2.5'/></svg> Pending (not yet due)</div>",
  "</div>"
)

# ==============================================================================
# CSS — navy-to-blue banner, matching the other apps in the suite
# ==============================================================================

app_css <- "
  .app-banner {
    background: linear-gradient(90deg, #0b2545 0%, #13315c 45%, #1d6fb8 100%);
    color: white; padding: 14px 20px; margin: 0 -15px 15px -15px;
  }
  .app-banner h2 { margin: 0; font-size: 22px; font-weight: 600; }
  .app-banner p  { margin: 2px 0 0 0; font-size: 13px; opacity: .85; }
  .stat-row { display: flex; gap: 12px; margin-bottom: 10px; flex-wrap: wrap; }
  .stat-tile {
    flex: 1; min-width: 150px; background: #fcfcfb; border: 1px solid #e3e2de;
    border-radius: 6px; padding: 8px 12px;
  }
  .stat-tile .num { font-size: 26px; font-weight: 600; color: #0b0b0b; line-height: 1.1; }
  .stat-tile .lbl { font-size: 12px; color: #52514e; }
  .play-row { display: flex; gap: 6px; align-items: center; margin-top: -6px; margin-bottom: 10px; }
  .play-row .form-group { margin-bottom: 0; }
  .navbar { margin-bottom: 0; }
  .data-note { font-size: 12px; color: #52514e; margin-bottom: 4px; }
"

# ==============================================================================
# UI  (a function, so "today" on the slider is fresh for every visitor)
# ==============================================================================

ui <- function(request) {
  navbarPage(
    title = "Permit Events",
    header = tags$head(tags$style(HTML(app_css))),

    # ---- Tab 1: Map ---------------------------------------------------------
    tabPanel(
      "Map",
      div(class = "app-banner",
          h2("WA Water Right Permit Events — Overdue vs Pending"),
          p("Open construction and proof-of-appropriation deadlines on active permits and Change-ROEs",
            HTML("&nbsp;&middot;&nbsp;"), "Data current as of ", strong(DATA_DATE_TXT))),

      sidebarLayout(
        sidebarPanel(
          width = 3,

          h4("As-of date"),
          sliderInput("as_of", NULL, min = SLIDER_MIN, max = SLIDER_MAX,
                      value = min(max(Sys.Date(), SLIDER_MIN), SLIDER_MAX),
                      step = 1, timeFormat = "%m/%d/%Y", ticks = FALSE, width = "100%"),
          div(class = "play-row",
              actionButton("play", "Play", icon = icon("play"), class = "btn-sm"),
              actionButton("go_today", "Today", class = "btn-sm"),
              selectInput("play_step", NULL, width = "120px",
                          choices = c("1 month" = "1 month", "3 months" = "3 months",
                                      "1 year" = "1 year"),
                          selected = "3 months")),

          hr(),
          h4("Filters"),
          checkboxGroupInput("status_show", "Show",
                             choices = c("Overdue", "Pending"),
                             selected = c("Overdue", "Pending"), inline = TRUE),
          checkboxGroupInput("event_show", "Event",
                             choices = EVENT_ORDER, selected = EVENT_ORDER),
          selectizeInput("phase_sel", "Phase", choices = PHASE_CHOICES, multiple = TRUE,
                         options = list(placeholder = "All phases")),
          selectizeInput("wria_sel", "WRIA", choices = WRIA_CHOICES, multiple = TRUE,
                         options = list(placeholder = "All WRIAs (map zooms to selection)")),
          selectizeInput("county_sel", "County", choices = COUNTY_CHOICES, multiple = TRUE,
                         options = list(placeholder = "All counties")),
          checkboxInput("spread", "Spread stacked points (same section)", value = TRUE),

          hr(),
          h4("Map overlays"),
          checkboxInput("show_wria",   "WRIA boundaries",       value = TRUE),
          checkboxInput("show_county", "County boundaries",     value = FALSE),
          checkboxInput("show_plss",   "PLSS / TRS grid (BLM)", value = FALSE),
          helpText(style = "font-size:11px;margin-top:-6px;",
                   "BLM CadNSDI township, range & section grid.",
                   "Zoom to level 10+ for section lines to render."),

          hr(),
          h4("Data"),
          uiOutput("data_info")
        ),

        mainPanel(
          width = 9,
          uiOutput("stat_tiles"),
          leafletOutput("map", height = 560),
          br(),
          h4("Open milestones by year due (current filters)", style = "margin-top:4px;"),
          plotlyOutput("timeline", height = 260)
        )
      )
    ),

    # ---- Tab 2: Table -------------------------------------------------------
    tabPanel(
      "Table",
      br(),
      p("Records currently shown on the map, with their status at the slider date."),
      downloadButton("dl_csv", "Download CSV", class = "btn-sm"),
      br(), br(),
      DTOutput("table")
    ),

    # ---- Tab 3: About -------------------------------------------------------
    tabPanel(
      "About",
      fluidRow(column(
        width = 8, offset = 1,
        br(),
        h3("About this app"),
        p("Each point is an active water right permit (or Change-ROE) with an",
          strong("open"), "development-schedule event: Begin Construction,",
          "Complete Construction, or Proof of Appropriation. Permits move through",
          "these events in order, so each record has one open event at a time."),
        h4("Overdue vs pending"),
        p("Move the slider to any date. An event is", strong("overdue"),
          "if its due date is before the slider date, and", strong("pending"),
          "if its due date is on or after it. Press", em("Play"),
          "to step forward through time."),
        h4("Summary tiles"),
        p(strong("Total permit schedules"), "is the number of open milestones",
          "matching the current filters (one per permit).", strong("Percent overdue"),
          "is overdue milestones divided by that total, at the slider date."),
        h4("Bar chart"),
        p("Each bar counts the open milestones whose due date falls in that year.",
          "Milestones due before the slider date are red (overdue); those due on or",
          "after it are blue (pending), so only the slider's own year is split.",
          "Future bars are simply scheduled due dates — the chart makes no",
          "prediction about which of them will eventually be missed."),
        h4("Important: this is not a historical view"),
        p("The data contains only events that were open on the data date. Sliding",
          "backward shows when each", em("current"), "deadline lapsed — not what",
          "the permit looked like at that time. For example, a Proof of",
          "Appropriation due in 2030 appears as pending in 2020, even though in",
          "2020 the permit was probably still in its construction phase."),
        h4("Locations"),
        p("Points are placed at the PLSS section, quarter, or quarter-quarter",
          "centroid from the TRS description, so locations are approximate.",
          "Records sharing a location are spread slightly around a small ring",
          "so each can be clicked; uncheck", em("Spread stacked points"),
          "to show true centroids. Records that could not be geocoded are not shown."),
        h4("Map overlays"),
        p("WRIA boundaries: WA Department of Ecology. County boundaries: US Census",
          "Bureau TIGERweb. Both were downloaded when the data were prepared.",
          "PLSS grid: drawn live from the",
          tags$a("BLM National PLSS CadNSDI service",
                 href = "https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer",
                 target = "_blank"), "."),
        h4("Data source and currency"),
        p("Washington State Department of Ecology, Water Rights Tracking System",
          "(WRTS) record search exports, joined on WR Doc ID and geocoded against",
          "the BLM CadNSDI PLSS layer.", strong(paste0("Data current as of ", DATA_DATE_TXT, ".")),
          "The data are refreshed periodically by the app maintainer; events",
          "completed or newly opened after that date are not reflected."),
        br()
      ))
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Data panel in the sidebar
  # --------------------------------------------------------------------------

  output$data_info <- renderUI({
    i <- DATA_INFO
    div(class = "data-note",
        strong(paste("Data current as of", DATA_DATE_TXT)), br(),
        format(i$n_used, big.mark = ","), " records mapped", br(),
        if (!is.na(i$n_failed) && i$n_failed > 0)
          tagList(i$n_failed, " could not be geocoded (not shown)", br()),
        if (i$n_no_due > 0) tagList(i$n_no_due, " with no due date (not shown)", br()),
        "Source: WA Ecology WRTS")
  })

  # --------------------------------------------------------------------------
  # Play / Today buttons
  # --------------------------------------------------------------------------

  playing <- reactiveVal(FALSE)

  observeEvent(input$play, {
    playing(!playing())
    updateActionButton(session, "play",
                       label = if (playing()) "Pause" else "Play",
                       icon  = icon(if (playing()) "pause" else "play"))
  })

  observeEvent(input$go_today, {
    playing(FALSE)
    updateActionButton(session, "play", label = "Play", icon = icon("play"))
    updateSliderInput(session, "as_of", value = Sys.Date())
  })

  observe({
    req(playing())
    invalidateLater(900)
    isolate({
      req(input$as_of)
      nxt <- seq(as.Date(input$as_of), by = input$play_step, length.out = 2)[2]
      if (nxt > SLIDER_MAX) {
        playing(FALSE)
        updateActionButton(session, "play", label = "Play", icon = icon("play"))
      } else {
        updateSliderInput(session, "as_of", value = nxt)
      }
    })
  })

  # --------------------------------------------------------------------------
  # Filtering
  #   base_filtered(): attribute filters only (independent of the date)
  #   with_status():   + overdue/pending at the slider date
  #   shown():         + the Show overdue/pending checkboxes
  # --------------------------------------------------------------------------

  as_of <- reactive({
    req(input$as_of)
    as.Date(input$as_of)
  })

  base_filtered <- reactive({
    df <- RECORDS |> filter(event %in% input$event_show)
    if (length(input$phase_sel)  > 0) df <- df |> filter(phase  %in% input$phase_sel)
    if (length(input$wria_sel)   > 0) df <- df |> filter(WRIA   %in% input$wria_sel)
    if (length(input$county_sel) > 0) df <- df |> filter(county %in% input$county_sel)
    df
  })

  with_status <- reactive({
    base_filtered() |>
      mutate(status = if_else(due < as_of(), "Overdue", "Pending"))
  })

  shown <- reactive({
    with_status() |> filter(status %in% input$status_show)
  })

  # --------------------------------------------------------------------------
  # Stat tiles
  # --------------------------------------------------------------------------

  output$stat_tiles <- renderUI({
    # Tiles follow the attribute filters (event, phase, WRIA, county) but NOT
    # the Show overdue/pending checkboxes, so the totals stay meaningful.
    df <- with_status()
    n_total  <- nrow(df)
    n_over   <- sum(df$status == "Overdue")
    n_over90 <- sum(df$status == "Overdue" & as.numeric(as_of() - df$due) > 90)
    pct_over <- if (n_total > 0) sprintf("%.1f%%", 100 * n_over / n_total) else "—"
    n_next12 <- sum(df$status == "Pending" & as.numeric(df$due - as_of()) <= 365)

    tile <- function(num, lbl, swatch = NULL) {
      div(class = "stat-tile",
          div(class = "num", if (is.numeric(num)) format(num, big.mark = ",") else num),
          div(class = "lbl", if (!is.null(swatch)) HTML(swatch), lbl))
    }
    dot  <- paste0("<span style='color:", COL_OVERDUE, ";'>&#9679;</span> ")
    ring <- paste0("<span style='color:", COL_PENDING, ";'>&#9675;</span> ")

    div(class = "stat-row",
        tile(n_total,  "Total permit schedules (open milestones)"),
        tile(n_over,   paste("Overdue as of", format(as_of(), "%m/%d/%Y")), dot),
        tile(n_over90, "Overdue by more than 90 days", dot),
        tile(pct_over, "Of permit schedules overdue", dot),
        tile(n_next12, "Pending, due within 12 months", ring))
  })

  # --------------------------------------------------------------------------
  # Map — base, boundaries and PLSS drawn once; points redrawn via proxy.
  # Panes keep the stacking order fixed no matter what is redrawn:
  #   basemap (200) < PLSS grid (350) < boundaries (410) < points (420)
  # so boundary polygons never sit on top of points and block their popups.
  # --------------------------------------------------------------------------

  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = MIN_ZOOM,
                                          maxBoundsViscosity = 1.0)) |>
      addMapPane("plss",       zIndex = 350) |>
      addMapPane("boundaries", zIndex = 410) |>
      addMapPane("points",     zIndex = 420)

    # "Light" basemap: CARTO Positron with your key; falls back to the
    # unkeyed provider (watermarked) if CARTO_API_KEY isn't set.
    m <- if (nzchar(CARTO_KEY)) {
      m |> addTiles(
        urlTemplate = paste0("https://basemaps.cartocdn.com/rastertiles/light_all/",
                             "{z}/{x}/{y}.png?key=", CARTO_KEY),
        attribution = CARTO_ATTRIB, group = "Light")
    } else {
      m |> addProviderTiles(providers$CartoDB.Positron, group = "Light")
    }

    m <- m |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
      # PLSS grid (same settings as the low-flow-wr app), in its own pane
      addWMSTiles(
        baseUrl = BLM_PLSS_WMS,
        layers  = "1,2",
        options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                 opacity = 0.80, version = "1.3.0", pane = "plss"),
        attribution = "PLSS grid: BLM National CadNSDI",
        group = "PLSS grid"
      )

    if (!is.null(WRIA_SF)) {
      m <- m |> addPolygons(
        data = WRIA_SF, group = "WRIA boundaries",
        color = COL_WRIA, weight = 1.5, fillColor = COL_WRIA, fillOpacity = 0.04,
        label = ~wria_display,
        labelOptions = labelOptions(style = list("font-size" = "11px", "font-weight" = "bold"),
                                    sticky = TRUE),
        highlightOptions = highlightOptions(color = "#2e6da4", weight = 2.5,
                                            fillOpacity = 0.10, bringToFront = FALSE),
        options = pathOptions(pane = "boundaries")
      )
    }
    if (!is.null(COUNTY_SF)) {
      m <- m |> addPolygons(
        data = COUNTY_SF, group = "County boundaries",
        color = COL_COUNTY, weight = 1.5, fillColor = COL_COUNTY, fillOpacity = 0.03,
        label = ~paste(NAME, "County"),
        labelOptions = labelOptions(style = list("font-size" = "11px"), sticky = TRUE),
        highlightOptions = highlightOptions(color = COL_COUNTY, weight = 2.5,
                                            fillOpacity = 0.08, bringToFront = FALSE),
        options = pathOptions(pane = "boundaries")
      )
    }

    m |>
      addLayersControl(baseGroups = c("Light", "Topographic", "Imagery"),
                       position = "topleft",
                       options = layersControlOptions(collapsed = TRUE)) |>
      fitBounds(WA_VIEW[["lng1"]], WA_VIEW[["lat1"]], WA_VIEW[["lng2"]], WA_VIEW[["lat2"]]) |>
      setMaxBounds(WA_MAXBNDS[["lng1"]], WA_MAXBNDS[["lat1"]],
                   WA_MAXBNDS[["lng2"]], WA_MAXBNDS[["lat2"]]) |>
      hideGroup(c("County boundaries", "PLSS grid")) |>
      addControl(HTML(legend_html), position = "bottomright") |>
      addScaleBar(position = "bottomleft")
  })

  # The map widget must exist in the browser before the proxy can change it.
  # input$map_zoom first appears once the map has drawn, so use it as a
  # one-time "map is ready" signal (zooming later does NOT redraw points).
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$map_zoom, map_ready(TRUE), once = TRUE)

  # Overlay checkboxes -> show / hide groups
  observe({
    req(map_ready())
    proxy <- leafletProxy("map")
    toggle <- function(p, show, group) if (isTRUE(show)) showGroup(p, group) else hideGroup(p, group)
    proxy |>
      toggle(input$show_wria,   "WRIA boundaries") |>
      toggle(input$show_county, "County boundaries") |>
      toggle(input$show_plss,   "PLSS grid")
  })

  # WRIA filter -> outline the selected WRIA(s) and zoom to them;
  # clearing the filter zooms back out to the whole state.
  observeEvent(input$wria_sel, {
    req(map_ready())
    proxy <- leafletProxy("map") |> clearGroup("WRIA selected")
    sel <- input$wria_sel
    if (length(sel) > 0 && !is.null(WRIA_SF)) {
      sel_sf <- WRIA_SF[WRIA_SF$WRIA %in% sel, ]
      if (nrow(sel_sf) > 0) {
        bb <- st_bbox(sel_sf)
        proxy |>
          addPolylines(data = sel_sf, group = "WRIA selected", color = COL_WRIA,
                       weight = 3.5, opacity = 0.9,
                       options = pathOptions(pane = "boundaries", interactive = FALSE)) |>
          flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    } else {
      proxy |> flyToBounds(WA_VIEW[["lng1"]], WA_VIEW[["lat1"]],
                           WA_VIEW[["lng2"]], WA_VIEW[["lat2"]])
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Points
  observe({
    req(map_ready())
    df  <- shown()
    d   <- as_of()
    use_spread <- isTRUE(input$spread)

    proxy <- leafletProxy("map") |> clearGroup("points")
    if (nrow(df) == 0) return()

    # Draw pending first so overdue (solid) sits on top
    df <- df |> arrange(desc(status == "Pending")) |>
      mutate(
        x = if (use_spread) lon_sprd else lon,
        y = if (use_spread) lat_sprd else lat,
        stroke_col = if_else(status == "Overdue", "#ffffff", COL_PENDING),
        fill_col   = if_else(status == "Overdue", COL_OVERDUE, "#ffffff"),
        stroke_w   = if_else(status == "Overdue", 1.5, 2.5)
      )

    proxy |>
      addCircleMarkers(
        data = df, lng = ~x, lat = ~y, group = "points",
        radius = 6, color = ~stroke_col, weight = ~stroke_w, opacity = 1,
        fillColor = ~fill_col, fillOpacity = 0.95,
        popup = build_popups(df, d),
        label = ~paste0(doc_number, " — ", event, " due ", format(due, "%m/%d/%Y")),
        options = pathOptions(pane = "points")
      )
  })

  # --------------------------------------------------------------------------
  # Due-year bar chart
  #   One bar per year = open milestones whose DUE DATE falls in that year.
  #   Red (bottom) = due before the slider date (overdue); blue = due on/after
  #   it (pending). Only the slider's own year is split. When the slider moves,
  #   bars are recoloured through plotlyProxy (no full redraw, no flicker).
  # --------------------------------------------------------------------------

  # Counts per due year, split at date d
  bar_counts <- function(df, d, years) {
    yr <- as.integer(format(df$due, "%Y"))
    ov <- df$due < d
    list(
      over = as.integer(table(factor(yr[ov],  levels = years))),
      pend = as.integer(table(factor(yr[!ov], levels = years)))
    )
  }

  # Dotted as-of line plus labels that ride along with it
  asof_shapes <- function(d) {
    list(list(type = "line", x0 = d, x1 = d, yref = "paper", y0 = 0, y1 = 1,
              line = list(color = "#52514e", width = 1.5, dash = "dot")))
  }
  asof_labels <- function(d) {
    list(
      list(x = d, y = 1, xref = "x", yref = "paper", xanchor = "right", yanchor = "top",
           xshift = -6, showarrow = FALSE, text = "◀ Overdue",
           font = list(color = "#0b0b0b", size = 11)),
      list(x = d, y = 1, xref = "x", yref = "paper", xanchor = "left", yanchor = "top",
           xshift = 6, showarrow = FALSE, text = "Pending ▶",
           font = list(color = "#0b0b0b", size = 11))
    )
  }

  timeline_years <- reactiveVal(NULL)

  output$timeline <- renderPlotly({
    df <- base_filtered()
    req(nrow(df) > 0, input$as_of)
    d0 <- isolate(as_of())

    yrs <- seq(as.integer(format(min(df$due), "%Y")),
               as.integer(format(max(df$due), "%Y")))
    timeline_years(yrs)
    cnt   <- bar_counts(df, d0, yrs)
    x_mid <- as.Date(paste0(yrs, "-07-01"))         # bar centred mid-year
    bar_w <- 0.82 * 365.25 * 86400000               # bar width in ms (date axis)

    plot_ly(x = x_mid) |>
      add_bars(y = cnt$over, name = "Overdue", width = bar_w,
               marker = list(color = COL_OVERDUE),
               customdata = yrs,
               hovertemplate = "%{customdata}: %{y:,} overdue<extra></extra>") |>
      add_bars(y = cnt$pend, name = "Pending", width = bar_w,
               marker = list(color = COL_PENDING),
               customdata = yrs,
               hovertemplate = "%{customdata}: %{y:,} pending<extra></extra>") |>
      layout(
        barmode   = "stack",
        hovermode = "x unified",
        xaxis = list(title = "Year milestone is due", showgrid = FALSE,
                     color = "#52514e", type = "date"),
        yaxis = list(title = "Open milestones", gridcolor = "#ebeae6",
                     zeroline = FALSE, color = "#52514e", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.18, traceorder = "normal"),
        shapes = asof_shapes(d0),
        annotations = asof_labels(d0),
        margin = list(l = 55, r = 20, t = 10, b = 40),
        plot_bgcolor = "#fcfcfb", paper_bgcolor = "#ffffff"
      ) |>
      config(displayModeBar = FALSE)
  })

  observeEvent(as_of(), {
    yrs <- timeline_years()
    req(yrs)
    cnt <- bar_counts(isolate(base_filtered()), as_of(), yrs)
    plotlyProxy("timeline", session) |>
      plotlyProxyInvoke("restyle", list(y = list(as.list(cnt$over), as.list(cnt$pend))),
                        list(0, 1)) |>
      plotlyProxyInvoke("relayout", list(shapes = asof_shapes(as_of()),
                                         annotations = asof_labels(as_of())))
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # Table + download
  # --------------------------------------------------------------------------

  table_data <- reactive({
    shown() |>
      mutate(days = as.numeric(as_of() - due)) |>
      arrange(due) |>
      transmute(
        Status = status,
        `Days past due (negative = days until due)` = days,
        `Record Document Number` = doc_number,
        `WR Doc ID` = wr_doc_id,
        `Person or Organization` = person,
        Event = event,
        `Due Date` = format(due, "%m/%d/%Y"),
        Phase = phase, Stage = stage,
        `Priority Date` = priority,
        Purpose = purpose, Qi = qi, UOM = uom, Qa = qa,
        WRIA = wria_label, County = county,
        TRS = trs, Q = q, QQ = qq,
        Source = source_name,
        lon, lat
      )
  })

  output$table <- renderDT({
    datatable(
      table_data() |> select(-lon, -lat),
      rownames = FALSE, filter = "top",
      options = list(pageLength = 25, scrollX = TRUE)
    ) |>
      formatStyle("Status", color = styleEqual(c("Overdue", "Pending"),
                                               c(COL_OVERDUE, COL_PENDING)),
                  fontWeight = "bold")
  })

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("permit_events_asof_", format(as_of(), "%Y%m%d"),
             "_data_", format(DATA_INFO$data_date, "%Y%m%d"), ".csv")
    },
    content = function(file) write_csv(table_data(), file, na = "")
  )
}

# ==============================================================================
# RUN THE APP
# ==============================================================================

shinyApp(ui = ui, server = server)
