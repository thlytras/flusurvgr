library(shiny)
load("fluData.RData")
load("data_translations.RData")
source("include.R")

#options(shiny.usecairo=FALSE)
if (require(Cairo)) {
  CairoFonts(
    regular="Roboto:style=Regular",
    bold="Roboto:style=Medium",
    italic="Roboto:style=RegularOblique",
    bolditalic="Roboto:style=MediumOblique"
  )
}

glb <- list(
  currTab = "0"
)


ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "mine.css")
  ),
  uiOutput("ui_title"),
  uiOutput("ui_appsubtitle"),
  br(),br(),
  sidebarLayout(
    sidebarPanel(
      uiOutput("ui_lang"),
      uiOutput("ui_yearSelect"),
      img(src='keelpno.png', width=199, height=157, 
          style="display: block; margin-left: auto; margin-right: auto;"),
      uiOutput("ui_lastUpd"),
      width=3
    ), mainPanel(
      uiOutput("ui_tabPanels")
    )
  )
))


server <- shinyServer(function(input, output, session) {

  # ***** BASIC UI ELEMENTS *****

  # *** Title & subtitle ***
  
  output$ui_title <- renderUI({
    titlePanel(tr["UI_TITLE", lang()])
  })

  output$ui_appsubtitle <- renderUI({
    span(tr["UI_APPSUBTITLE", lang()])
  })
  
  output$ui_lastUpd <- renderUI({
    div(tr["UI_LASTUPD", lang()], ": ",
      format(lastUpd, "%d/%m/%Y"), style="text-align:center; line-height:140%;")
  })

  # *** Language selector ***
  
  output$ui_lang <- renderUI({
    radioButtons("lang", tr["UI_LANG", lang()], 
                 c("Ελληνικά" = "GR", "English" = "EN"), selected=lang())
  })
  
  lang <- reactive({
    if (is.null(input$lang)) return("GR")
    input$lang
  })
  
  # *** Year selector ***
  
  output$ui_yearSelect <- renderUI({
    yearSelections <- rev(rownames(avInfo))
    names(yearSelections) <- rev(paste(rownames(avInfo), as.integer(rownames(avInfo))%%100+1, sep="-"))
    names(yearSelections)[1] <- paste(names(yearSelections)[1], tr["UI_YCURRENT", lang()])
    selectInput("yearSelect", tr["UI_YEARSELECT", lang()], as.list(yearSelections), selectize=TRUE, selected=selYear())
  })
    
  selYear <- reactive({
    if (is.null(input$yearSelect)) return(as.integer(rev(rownames(avInfo))[1]))
    as.integer(input$yearSelect)
  })
  
  # *** Tabs and tab switcher ***
  
  observe({glb$currTab <<- input$tabset })
  
  observeEvent(input$tabset, {
    updateTabsetPanel(session, "tabset", selected=glb$currTab)
  })
  
  output$ui_tabPanels <- renderUI({
    a <- avInfo[as.character(selYear()),]
    TABS <- list(
      tabPanel(tr["UI_TBTL_OVERVIEW",lang()],
        div(HTML(tr[sprintf("UI_OVERVIEW_%s",selYear()),lang()])),
        value="0"
      ),
      tabPanel(tr["UI_TBTL_SENTINEL",lang()],
        plotOutput("plotSentinel"),
        checkboxInput("showPanel", "Show panel", FALSE),
#         conditionalPanel(condition = "document.getElementById('showPanel') && input.showPanel", 
#           wellPanel("oifjdoijsdoijf")
#         ), h3("Hi there"), p("Hi there"), br(), br(),
        value="1"
      ),
      tabPanel(tr["UI_TBTL_SWABS", lang()],
        plotOutput("plotSwabs"),
        value="2"
      ),
      tabPanel(tr["UI_TBTL_METH", lang()],
        plotOutput("plotMeth"),
        value="3"
      )
    )[c(1,which(a>0)+1)]
    if (!is.null(glb$currTab) && is.na(match(glb$currTab, which(a>0)))) glb$currTab <- "0"
    TABS <- c(TABS, id="tabset", selected=glb$currTab)
    do.call(tabsetPanel, TABS)
  })
  
  
  
  # ***** Elementary Panels *****
  
  output$plotSentinel <- renderPlot({
    y <- selYear()
    if (!avInfo[as.character(y),1]) return()
    outputOptions(output, "plotSentinel", suspendWhenHidden=FALSE)
    sentinel_graph(y-(1:0), col=c("navyblue", "red3"), lty=c(3,1), lwd=c(1,1.5), ci=TRUE, alpha=c(0.08,0.15), lang=lang())
  }, res=80)
  
  output$plotSwabs <- renderPlot({
    y <- selYear()
    if (!avInfo[as.character(y),2]) return()
    outputOptions(output, "plotSwabs", suspendWhenHidden=FALSE)
    swabPlot(y, y*100+120, lang=lang())
  }, res=90)
  
  output$plotMeth <- renderPlot({
    y <- selYear()
    if (!avInfo[as.character(y),3]) return()
    outputOptions(output, "plotMeth", suspendWhenHidden=FALSE)
    methDeathPlot(y, y*100+120, lang=lang(), death=FALSE)
  }, res=90)
  
  
  
})

shinyApp(ui = ui, server = server)

