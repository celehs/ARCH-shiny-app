
library("colourpicker")
library("data.table")
library("DBI")
library("dplyr")
library("DT")
library("ggplot2")
# library("htmltools")
library("igraph")
# library("shiny")
# library("shinyBS")
library("shinycssloaders")
library("shinydashboard")
library("shinydashboardPlus")
library("shinyhelper")
library("shinyWidgets")
library("stringr")
library("plotly")
library("reactable")
# library("readr")
# library("reshape2")
library("rintrojs")
library("RPostgres")
library("visNetwork")

library(yaml)
data <- yaml.load_file("config.yaml")
print(data)
url_home <- data$url_home
url_phe <- data$url_phe
db <- data$db

sapply(dir("func", full.names = TRUE), source)
load("data/sysdata.rda")

# header ====
header <- shinydashboardPlus::dashboardHeader(
  # title = tags$p(img(src="www/parse-logo.png", width = "30px"), span("NLP & Codified Network")),
  title = tags$p(span("NLP & Codified Network")),
  leftUi = headerUI("btn", url_home),
  titleWidth = "350px", controlbarIcon = shiny::icon("cogs"))

# sidebar ====
sidebar <- dashboardSidebar(
  sidebarUI("side"),
  hr(),
  uiOutput("ui_filter"),
  id = "sideBar",
  collapsed = FALSE,
  width = "350px",
  minified = FALSE
)

# body ====
body <- shinydashboard::dashboardBody(
  windowSizeUI("win"),
  includeCSS("www/style.css"),
  rintrojs::introjsUI(),
  fluidRow(
    column(6,  div(id = "node-info",
                   uiOutput("clicked_node_title"),
                   htmlOutput("clicked_node_info"))),
    column(6, uiOutput("ui_details"))),
  br(),
  tabsetPanel(id = "tabs_nodeinfo",
              tabPanel(title = "Hierarchy of connected nodes",
                       br(),
                       div(uiOutput("ui_sun"),align="center")
              ),
              tabPanel(title = "Table of connected nodes",
                       uiOutput("clicked_node_table")
              ),
              tabPanel(title = "Network",
                       uiOutput("ui_network")),
  
              tabPanel(title = "Circular plot (codified)",
                       br(),
                       uiOutput("circularplot_codify")
              ),
              tabPanel(title = "Circular plot (nlp)",
                       br(),
                       uiOutput("circularplot_nlp") %>%
                         shinyhelper::helper(type = "markdown",
                                             # colour = "blue",
                                             title = "Note",
                                             content = "helper_circularplot",
                                             size = "m",
                                             style = "margin-right: 5px;")
              )
  )
)

# controlbar ====
controlbar <- shinydashboardPlus::dashboardControlbar(
  width = 450,
  id = "controlbar",
  skin = "light",
  shinydashboardPlus::controlbarMenu(
    id = "controlbarMenu",
    selected = "Network",
    # shinydashboardPlus::controlbarItem(
    #   # "Threshold",
    #   "Filter Nodes"#,
    #   # uiOutput("ui_filter")
    # ),
    shinydashboardPlus::controlbarItem(
      "Network",
      checkboxInput("hide_labels", "Hide the labels", value = FALSE),
      h4(tags$b("customize color")),
      div(uiOutput("ui_color"), style="height: 450px; overflow-y: scroll;"),
      h4(tags$b("customize shape")),
      uiOutput("ui_shape")
    )
  )
)



ui <- shinydashboardPlus::dashboardPage(
  header,
  sidebar,
  body,
  controlbar,
  title = NULL
)

server <- function(input, output, session){
  shinyhelper::observe_helpers(help_dir = "doc/")
  
  observeEvent(input$`btn-help`, {
    if (!(input$sideBar)) {
      shinydashboardPlus::updateSidebar("sideBar")
    }
    rintrojs::introjs(session,
                      options = list(
                        steps = steps[, -1],
                        showBullets = FALSE
                      )
    )
  })
  
  ## data  ===================
  dict.combine <- getData(NULL, "dict", db)
  dict.combine <- dict.combine[, !colnames(dict.combine) %in% c("term_s", "stype_s")]
  dict.combine$idforCCSlabel = dict.combine$id
  ccs_map = read.csv('data/CUI_CCS_UMLS2021AA_VID.csv') %>% filter(str_detect(CCS, '^[0-9]'))
  ccs_map$CCS_num = ccs_map$CCS
  ccs_map$CCS = paste0('CCS:', ccs_map$CCS)
  ccs_map$CCS_id = paste0('ProcedureCode:', 1:nrow(ccs_map))
  ccs_map_2 = read.csv('data/no_match_cui_ccs_final.csv')
  
  dict.combine.ccs = dict.combine[str_detect(dict.combine$id, '^CCS\\:'), ]
  dict.combine.ccs$label[dict.combine.ccs$id %in% ccs_map$CCS] =
    ccs_map$CCS_id[match(dict.combine.ccs$id[dict.combine.ccs$id %in% ccs_map$CCS], ccs_map$CCS)]
  dict.combine.ccs$term[dict.combine.ccs$id %in% ccs_map$CCS] =
    ccs_map$CUI_STR[match(dict.combine.ccs$id[dict.combine.ccs$id %in% ccs_map$CCS], ccs_map$CCS)]
  dict.combine.ccs$idforCCSlabel = dict.combine.ccs$id
  dict.combine.ccs$id[dict.combine.ccs$id %in% ccs_map$CCS] =
    ccs_map$CCS_id[match(dict.combine.ccs$id[dict.combine.ccs$id %in% ccs_map$CCS], ccs_map$CCS)]
  
  dict.combine.ccs.0 = dict.combine.ccs[str_detect(dict.combine.ccs$id, '^CCS'), ] %>%
    arrange(id)
  dict.combine.ccs.0$id = paste0('ProcedureCode:ab',1:nrow(dict.combine.ccs.0))
  dict.combine.ccs.0$term = ccs_map_2$new_term[match(dict.combine.ccs.0$id, ccs_map_2$id_new)]
  dict.combine = rbind(dict.combine[!str_detect(dict.combine$id, '^CCS\\:'), ],
                       dict.combine.ccs[!str_detect(dict.combine.ccs$id, '^CCS'), ],
                       dict.combine.ccs.0)
  dict.combine$group1 = str_replace(dict.combine$group1, '^CCS$', 'ProcedureCode')
  
  # junk = dict.combine.ccs[str_detect(dict.combine.ccs$id, '^CCS'), ]
  # junk = junk[, c(1,4)]
  # junk = cbind(junk, `id_new` = dict.combine.ccs.0$id)
  # write.csv(junk, file = 'no_match_cui_ccs.csv')
  
  
  
  ids <- unique(dict.combine$id)
  if(sum(!grepl("\\w", dict.combine$group, perl = TRUE)) > 0){
    dict.combine$group[!grepl("\\w", dict.combine$group, perl = TRUE)] <- "Unknown"
  }
  
  tb_input <- dict.combine[, c("id", "term")]
  
  node_num_cutoff = 500
  
  winsize <- windowSizeServer("win")
  
  headerServer("btn", steps[, -1], "doc/documentation.md", df_edges_cutted())
  
  directed = FALSE
  
  # center_node ====
  center_node <- sidebarServer("side", tb_input, tname = "dict", db, type = 2, 
                               # selected = c(2, 4, 8, 12),
                               selected = c(4),
                               # init_nodes = c("C0003873", "C0409637", "PheCode:714.1", "PheCode:714.2"),
                               init_nodes = c("PheCode:714.1"),
                               server = TRUE)
  
  name_input <- reactive({
    paste(gsub("[^\\w]", "_", center_node(), perl = TRUE), collapse = "_")
  })
  
  # ui filter ====
  observeEvent(center_node(), {
    top_n <- 500
    max_nodes <- min(length(center_node()) * top_n, nrow(df_edges_center()))
    thr_cos <- floor(sort(df_edges_center()$weight, decreasing = TRUE)[max_nodes]*100)/100
    max = ceiling(max(df_edges_center()$weight)*100)/100
    min = floor(min(df_edges_center()$weight)*100)/100
    
    df <- df_edges_center()[df_edges_center()$weight >= thr_cos[1], ]
    categories <- sort(unique(df$category))
    
    df_categories <- data.frame(cat1 = dict.combine$type[match(categories, dict.combine$category)],
                                cat2 = categories)
    
    print(df_categories)
    
    if(length(unique(df_categories$cat1)) > 1){
      categories <- lapply(split(df_categories$cat2, df_categories$cat1), as.list)
    }
    
    print(categories)
    
    # if(input$controlbarMenu == "Network"){
    #   updateControlbarMenu(session = session, id = "controlbarMenu", selected = "Filter Nodes")
    # }
    print("====min====max====value====")
    print(max)
    print(min)
    print(c(thr_cos, max))
    if(is.na(thr_cos)){
      thr_cos <- min
    }
    output$ui_filter <- renderUI({
      tagList(
        sliderInput(
          inputId = paste0(name_input(), "-filter_cos"),
          label = "Filter nodes by cosine similarity",
          min = min, max = max,
          value = c(thr_cos, max),
          step = 0.01,
          width = "100%"
        ),
        shinyWidgets::virtualSelectInput(
          inputId = paste0(name_input(), "-filter_category"),
          label = "Filter nodes by category:",
          choices = categories,
          selected = as.vector(unlist(categories)),
          multiple = TRUE,
          optionsCount = 20,
          width = "100%"
        )
      )
    })
  })
  
  categories <- reactive({
    req(input[[paste0(name_input(), "-filter_cos")]])
    thr_cos <- input[[paste0(name_input(), "-filter_cos")]]
    df <- df_edges_center()[df_edges_center()$weight >= thr_cos[1], ]
    cats <- sort(unique(df$category))
    types <- dict.combine$type[match(cats, dict.combine$category)]
    if(length(unique(types)) > 1){
      cats <- lapply(split(cats, types), as.list)
    }
    cats
  })
  
  observeEvent(categories(), {
    shinyWidgets::updateVirtualSelect(
      session = session,
      inputId = paste0(name_input(), "-filter_category"),
      label = "Filter nodes by category:",
      choices = categories(),
      selected = as.vector(unlist(categories()))
    )
  })
  
  df_edges_cutted <- reactive({
    req(input[[paste0(name_input(), "-filter_cos")]])
    req(input[[paste0(name_input(), "-filter_category")]])
    # print("df_edges_cutted")
    # print(nrow(df_edges_center()))
    df <- df_edges_center()[df_edges_center()$weight >= input[[paste0(name_input(), "-filter_cos")]][1], ]
    print(nrow(df))
    # print(nrow(df[df$category %in% input$filter_category, ]))
    df[df$category %in% input[[paste0(name_input(), "-filter_category")]], ]
  })
  
  df_edges_center <- reactive({
    print("df_edges")
    req(center_node())
    print(center_node())
    df <- Reduce(rbind, lapply(center_node(), getData, "df_edges", db))
    df$from = dict.combine$id[match(df$from, dict.combine$idforCCSlabel)]
    df$to = dict.combine$id[match(df$to, dict.combine$idforCCSlabel)]
    
    df$category <- dict.combine$category[match(df$to, dict.combine$id)]
    print(table(df$category))
    df
  })
  
  
  ## network  ====================================
  output$ui_network <- renderUI({
    req(winsize()[2])
    # req(input$filter_cos)
    # req(input$filter_category)
    if(length(center_node()) > 0 & (nrow(df_edges_cutted()) > 0)){
      print("ui_network")
      print(nrow(df_edges_cutted()))
      print(center_node())
      shinycssloaders::withSpinner(
        visNetworkOutput("network",
                         height =  paste0((winsize()[2]-100),"px")),
        type = 6
      )
    } else {
      h3("Try select some rows and click on submit")
    }
  })
  
  output$network <- renderVisNetwork({
    print("********************network**************")
    # myconfirmation = input$myconfirmation
    print(center_node())
    if(length(center_node()) > 0 | (isTruthy(df_edges_cutted()))){
      req(picked_colors())
      req(picked_shapes())
      df <- df_edges_cutted()
      plot_network(center_node(), df_edges_cutted(), 
                   dict.combine, attrs, picked_colors(), picked_shapes(),
                   hide_labels = FALSE, 
                   directed = directed,
                   node_num_cutoff = 500, 
                   layout = "layout_nicely")
    }
  })
  
  # node info ====
  
  # Clicked node text
  output$clicked_node_title <- renderUI({
    h3(dict.combine$term[match(center_node(), dict.combine$id)])
  })
  output$clicked_node_info <- renderUI({
    print("clicked_node_info")
    clickedNodeText(center_node(), dict.combine)
  })
  
  ## df plots  ===================================
  
  df_plots <- reactive({
    df_edges_cutted()
  })
  
  ## sunburst =======================================
  output$ui_sun <- renderUI({
    if(nrow(df_plots()) > 0){
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("sun",width="auto",
                             height="750px"), type = 6
      )} else {
        ""
      }
  })
  
  output$sun <- plotly::renderPlotly({
    print("sunburst")
    sunburstPlotly(center_node(), df_plots(),
                   # input$changeline, input$rotatelabel, input$scale_sungh,
                   dict.combine)
  })
  
  ## circular plot  =======================================
  
  output$circularplot_codify <- renderUI({
    if(nrow(df_plots()) > 0){
      shinycssloaders::withSpinner(
        plotOutput("circular_codify", width = "100%",
                   height = paste0(max(700, winsize()[2] - 450),"px")), type = 6
      )} else {
        ""
      }
  })
  output$circular_codify <- renderPlot({
    print("circular")
    node_now = center_node()
    circularBar_codify(df_plots(), dict.combine, ColorsCirc)
  })
  
  
  output$circularplot_nlp <- renderUI({
    if(nrow(df_plots()) > 0){
      shinycssloaders::withSpinner(
        plotOutput("circular_nlp", width = "100%",
                   height = paste0(max(700, winsize()[2] - 450),"px")), type = 6
      )} else {
        ""
      }
  })
  output$circular_nlp <- renderPlot({
    print("circular")
    node_now = center_node()
    circularBar_nlp(df_plots(), dict.combine, ColorsCirc)
  })
  
  # table of clicked node  ================================
  # tbClickedServer("tb1", df_plots(), paste0(winsize()[2] - 400,"px"), dict.combine)
  
  output$clicked_node_table <- renderUI({
    if(nrow(df_plots()) > 0){
      shinycssloaders::withSpinner(
        reactable::reactableOutput("tb_clicked_node", width = "100%",
                                   height = paste0(winsize()[2] - 400,"px")), type = 6
      )} else {
        ""
      }
  })
  
  df_clicked_node <- reactive({
    df <- df_plots()[, c("from", "to", "weight")]
    colnames(df) <- c("center_node", "connected_nodes", "cosine_similarity")
    df <- left_join(df,
                    dict.combine[, c("id", "term", "category")],
                    by = c("connected_nodes" = "id"))
    df$cosine_similarity <- round(df$cosine_similarity, 3)
    df <- df[order(df$cosine_similarity, decreasing = TRUE), c(1, 2, 5, 4, 3)]
    if("count" %in% colnames(dict.combine)){
      df$Count = dict.combine$count[match(df$connected_nodes, dict.combine$id)]
    } else {
      df$Count = NA
    }
    df
  })
  
  
  output$tb_clicked_node <- reactable::renderReactable(
    reactable::reactable({ df_clicked_node() },
                         details = function(index) {
                           row_id = df_clicked_node()$connected_nodes[index]
                           if(grepl("^C\\d+$", row_id, perl = TRUE)){
                             sy <- getData(row_id, "synonyms", db, field = "id")
                             if(nrow(sy) > 1){
                               htmltools::div(
                                 "Synonyms of ", row_id, ":",
                                 htmltools::tags$pre(paste(sort(sy$synonyms[sy$id == row_id]), collapse = "\n")),
                                 style = "margin-left: 300px; width: 700px"
                               )
                             }
                           }
                         },
                         groupBy = c("center_node"),
                         columns = list(
                           cosine_similarity = reactable::colDef(name = "cosine similarity"),
                           connected_nodes = reactable::colDef(
                             minWidth = 250,
                             name = "connected_nodes / term",
                             # Show species under character names
                             cell = function(value, index) {
                               term <- df_clicked_node()$term[index]
                               term <- if (!is.na(term)) term else "Unknown"
                               div(
                                 div(style = list(fontWeight = 600), value),
                                 div(style = list(fontSize = 12), term)
                               )
                             }
                           ),
                           term = reactable::colDef(show = FALSE),
                           Count = reactable::colDef(show = ifelse("count" %in% colnames(dict.combine), TRUE, FALSE))
                         ),
                         bordered = TRUE,
                         defaultExpanded = TRUE,
                         pagination = FALSE
    )
  )
  
  
  
  ## ui details  ===================================================
  output$ui_details <- renderUI({
    if(center_node() %in% phecode$Phecode){
      phe_id <- gsub(".+:", "", center_node(), perl = TRUE)
      href <- paste0(url_phe, phe_id)
      htmltools::div(tags$a(span(icon("hand-point-right"), "Phecode map to ICD"), 
             href = href, target = "_blank", style = "color: darkblue"),
             style = 'box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px; 
                      margin-top: 20px; padding:30px')
    } else {
      outdiv <- tagList()
      tbs <- getData(NULL, "details", db)
      if(!is.null(tbs)) {
        # tbs <- rbind(tbs, data.frame(tname = "synonyms", title = "Synonyms", note = "Synonyms"))
        print(tbs)
        sy <- "synonyms" %in% tbs$tname
        print(sy)
        
        apply(tbs, 1, function(x){
          tname = x[1]
          title = x[2]
          helps = ifelse(!is.null(x) & length(x) == 3, x[3], "")
          print(tname)
          df <- getData(center_node(), tname, db, field = "id")
          # print(head(df))
          if(nrow(df) > 0){
            outdiv <<- detailsTab(tname, df[df$id == center_node(),], title, outdiv, center_node(), output, sy, helps)
          }
        })
        if (length(outdiv) > 0){
          outdiv
        } }
      
    }
    
  })
  
  # color picker  =================================
  
  output$ui_color <- renderUI({
    n <- 1
    colors_group <- NULL
    print("ui_color")
    print(ColorsNet$group)
    print(unique(dict.combine$group))
    lapply(sort(unique(dict.combine$group)), function(x){
      if(!x %in% ColorsNet$group){
        c <- sample(setdiff(colors, c(ColorsNet$color.background, colors_group$color.background)), 1)
        colors_group <<- rbind(colors_group, data.frame("group"=x,
                                                        "color.background"=c))
      } else {
        colors_group <<- rbind(colors_group, ColorsNet[ColorsNet$group == x,])
      }
      column(6, colorpickerUI(x, colors_group$color.background[colors_group$group == x]))
    })
  })
  
  picked_colors <- reactive({
    updateControlbarMenu("controlbarMenu", selected = "Network")
    c <- sapply(sort(unique(dict.combine$group)), colorpickerServer)
    req(c[[1]])
    print("picked_colors")
    print(c)
    data.frame("group" = names(c), "color.background" = c)
  })
  
  # shape picker ====
  output$ui_shape <- renderUI({
    n <- 1
    shape_group <- NULL
    print("ui_shape")
    types <- sort(unique(dict.combine$type))
    print(types)
    print(shapes)
    lapply(1:length(types), function(x){
      shape_group <<- rbind(shape_group, data.frame("type"=types[x],
                                                    "shape"=shapes[x]))
      column(6, shapepickerUI(types[x], shapes[x], shapes, shapes_icons))
    })
  })
  
  picked_shapes <- reactive({
    updateControlbarMenu("controlbarMenu", selected = "Network")
    c <- sapply(sort(unique(dict.combine$type)), shapepickerServer)
    req(c[[1]])
    print("picked_shapes")
    print(c)
    data.frame("type" = names(c), "shape" = c)
  })
}


shinyApp(ui, server)



