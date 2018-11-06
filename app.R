library(shiny)
library(WriteXLS)
load("fluData.RData")
load("data_translations.RData")
source("include.R")

#options(shiny.usecairo=FALSE)
if (suppressWarnings(require(Cairo))) {
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
#  img(src='flu.png', style="position: absolute; right:1%; top: 1%", width=110, height=110),
   img(src='flu.png', style="float:right; margin: 1em", width=110, height=110),
  uiOutput("ui_title"), 
  uiOutput("ui_appsubtitle"),
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
#    span(tr["UI_APPSUBTITLE", lang()])
# minus-circle arrow-circle-up arrow-circle-down
    wk <- as.integer(unlist(strsplit(tr["UI_CURRWEEK", lang()],"/"))[1])
    res <- list()
    res[[1]] <- div(
        div(icon("calendar", "fa-3x"), style="display:table-cell; vertical-align:middle; padding-right:1em"), 
        div(strong(c(GR="Εβδομάδα: ", EN="Week: ")[lang()], tr["UI_CURRWEEK", lang()]), style="display:table-cell; vertical-align:middle;"),
        style="display:inline-block; padding-right:4em", id="sb-icon-cal")
    if (wk>20 || wk<40) {
      ints <- tr[c("UI_W_LOW", "UI_W_MEDIUM", "UI_W_HIGH", "UI_W_VERYHIGH"), lang()][as.integer(tr["UI_CURRINTENSITY", lang()])+1]
      trnd <- tr[c("UI_W_DECR", "UI_W_STABLE", "UI_W_INCR"), lang()][as.integer(tr["UI_CURRTREND", lang()])+2]
      intsIcon <- c("arrow-circle-down", "minus-circle", "arrow-circle-up")[as.integer(tr["UI_CURRINTENSITY", lang()])+1]
      trndIcon <- c("arrow-circle-down", "minus-circle", "arrow-circle-up")[as.integer(tr["UI_CURRTREND", lang()])+2]
      veryHigh <- as.integer(tr["UI_CURRINTENSITY", lang()])==3
      res <- c(res,
        list(div(
          div(strong(tr["UI_IND_INTENSITY", lang()], ": ", ints), style="display:table-cell; vertical-align:middle; padding-right:1em"), 
          div(icon(intsIcon, "fa-3x"), 
            if (veryHigh) icon("arrow-circle-up", "fa-3x") else NULL,
            style="display:table-cell; vertical-align:middle"), 
          style="display:inline-block; padding-right:3em", id="sb-icon-intensity")),
        list(div(
          div(strong(tr["UI_IND_TREND", lang()], ": ", trnd), style="display:table-cell; vertical-align:middle; padding-right:1em"), 
          div(icon(trndIcon, "fa-3x"), style="display:table-cell; vertical-align:middle"), 
          style="display:inline-block; padding-right:3em", id="sb-icon-trend"))
      )
    } else {
      res <- c(res, list(div(div(strong(tr["UI_OUTOFSEASON",lang()]), 
        style="display:table-cell; vertical-align:middle;"), style="display:inline-block; padding-right:3em")))
    }
    do.call(div, res)
  })
  
  output$ui_lastUpd <- renderUI({
    div(tr["UI_LASTUPD", lang()],
      format(lastUpd, "%d/%m/%Y"), style="text-align:center; line-height:140%;")
  })

  # *** Language selector ***
  
  output$ui_lang <- renderUI({
      tags$div(HTML(sprintf("<div id=\"lang\" class=\"form-group shiny-input-radiogroup shiny-input-container\">\n  <label class=\"control-label\" for=\"lang\">%s</label>\n  <div class=\"shiny-options-group\">\n    <div class=\"radio\">\n      <label>\n        <input type=\"radio\" name=\"lang\" value=\"GR\"%s/>\n        <span><img src=\"flag_GR.png\"/> Ελληνικά</span>\n      </label>\n    </div>\n    <div class=\"radio\">\n      <label>\n        <input type=\"radio\" name=\"lang\" value=\"EN\"%s/>\n        <span><img src=\"flag_GB.png\"/> English</span>\n      </label>\n    </div>\n  </div>\n</div>", tr["UI_LANG", lang()], ifelse(lang()=="GR", " checked=\"checked\"", ""), ifelse(lang()=="EN", " checked=\"checked\"", ""))))
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
  
  currYear <- reactive({ as.integer(rev(rownames(avInfo))[1]) == selYear() })
  
  # *** Tabs and tab switcher ***
  
  observe({glb$currTab <<- input$tabset })
  
  observeEvent(input$tabset, {
    updateTabsetPanel(session, "tabset", selected=glb$currTab)
  })
  
  output$ui_tabPanels <- renderUI({
    y <- selYear()
    a <- avInfo[as.character(y),]
    TABS <- list(
      tabPanel(tr["UI_TBTL_OVERVIEW",lang()],
        h5(sprintf(tr["UI_SUMMARYHEADER",lang()], y, y+1)),
        if (y==as.integer(tr["UI_CURRYEAR",lang()])) {
          h6(sprintf(tr["UI_LIMWEEK",lang()], ""), strong(tr["UI_CURRWEEK",lang()]), sprintf("(%s)", wkLims(tr["UI_CURRWEEK",lang()], lang())))
        } else { "" },
        div(HTML(tr[sprintf("UI_OVERVIEW_%s",selYear()), lang()])),
        if (y==as.integer(tr["UI_CURRYEAR",lang()])) {
          if (!is.na(tr["UI_CURRWREP", lang()])) 
            a(sprintf(tr["UI_LEG_CURRWREP", lang()], tr["UI_CURRWEEK", lang()]), href=tr["UI_CURRWREP", lang()])
        } else { 
          if (!is.na(tr[sprintf("UI_ANNREP_%s", selYear()), lang()])) 
            a(sprintf(tr["UI_LEG_ANNREP", lang()], y, y+1), href=tr[sprintf("UI_ANNREP_%s", selYear()), lang()])
        },
        br(), br(),
        value="0"
      ),
      tabPanel(tr["UI_TBTL_SENTINEL",lang()],
        h5(tr["UI_TBTL_SENTINEL",lang()]),
        checkboxInput("showPanelSentinel", tr["UI_METHODPANEL", lang()], FALSE),
        conditionalPanel(condition = "document.getElementById('showPanelSentinel') && input.showPanelSentinel", 
          wellPanel(HTML(tr["METH_SENTINEL",lang()]))
        ),        h6(strong(tr["FIGTITLE_GRAPH", lang()]), sprintf(tr["FIGTITLE_SENTINEL",lang()], y, y+1, y-1, y)),
        plotOutput("plotSentinel"),
        downloadButton("downloadSentinel", tr["UI_DOWNLOADBUTTON",lang()]),
        br(), br(),
        value="1"
      ),
      tabPanel(tr["UI_TBTL_SWABS", lang()],
        h5(tr["UI_TBTL_SWABS",lang()]),
        checkboxInput("showPanelSwabs", tr["UI_METHODPANEL", lang()], FALSE),
        conditionalPanel(condition = "document.getElementById('showPanelSwabs') && input.showPanelSwabs", 
          wellPanel(HTML(tr["METH_SWABS",lang()]))
        ),
        h6(strong(tr["FIGTITLE_GRAPH", lang()]), sprintf(tr["FIGTITLE_SWAB",lang()], y, y+1)),
        plotOutput("plotSwabs"),
        downloadButton("downloadSwabs", tr["UI_DOWNLOADBUTTON",lang()]),
        br(), br(),
        value="2"
      ),
      tabPanel(tr["UI_TBTL_METH", lang()],
        h5(sprintf("%s – %s %s-%s", tr["UI_TBTL_METH", lang()], tr["UI_YEARSELECT",lang()], y, y+1), 
          ifelse(currYear(), sprintf("(%s)", sprintf(tr["UI_LIMWEEK",lang()], tr["UI_CURRWEEK",lang()])), "")),
        div(tableOutput("methDeathTable"), style="font-size:115%; font-weight:500"),
        checkboxInput("showPanelMeth", tr["UI_METHODPANEL", lang()], FALSE),
        conditionalPanel(condition = "document.getElementById('showPanelMeth') && input.showPanelMeth", 
          wellPanel(HTML(tr["METH_METH",lang()]))
        ),
        h6(strong(tr["FIGTITLE_GRAPH", lang()]), sprintf(tr["FIGTITLE_METH",lang()], y, y+1)),
        plotOutput("plotMeth"),
        downloadButton("downloadMeth", tr["UI_DOWNLOADBUTTON",lang()]),
        br(), br(),
        h6(strong(tr["FIGTITLE_GRAPH", lang()]), sprintf(tr["FIGTITLE_LDEATH",lang()], y, y+1)),
        plotOutput("plotLDeath"),
        downloadButton("downloadLDeath", tr["UI_DOWNLOADBUTTON",lang()]),
        br(), br(),
        h6(strong(tr["FIGTITLE_GRAPH", lang()]), sprintf(tr["FIGTITLE_METHLDEATHAGE",lang()], y, y+1)),
        plotOutput("plotMethLDeathAge"),
        downloadButton("downloadMethLDeathAge", tr["UI_DOWNLOADBUTTON",lang()]),
        br(), br(),
        value="3"
      ),
      tabPanel(tr["UI_TBTL_MOMO", lang()],
        h5(tr["UI_TBTL_MOMO",lang()]),
        checkboxInput("showPanelMOMO", tr["UI_METHODPANEL", lang()], FALSE),
        conditionalPanel(condition = "document.getElementById('showPanelMOMO') && input.showPanelMOMO", 
          wellPanel(HTML(tr["METH_MOMO",lang()]))
        ),
        h6(strong(tr["FIGTITLE_GRAPH", lang()]), sprintf(tr["FIGTITLE_MOMO",lang()], y, y+1)),
        plotOutput("plotMomo"),br(),
        downloadButton("downloadMOMO", tr["UI_DOWNLOADBUTTON",lang()]),
        br(), br(),
        value="4"
      ),
      tabPanel(tr["UI_TBTL_GENERALINFO", lang()],
        h5(tr["UI_TBTL_GENERALINFO",lang()]),
        div(HTML(tr["UI_GENERALINFO1",lang()])), 
        h6(strong(tr["UI_GENERALINFO2", lang()])),
        img(src="pyramid.png", style="max-width:750px; height:auto"),
        div(HTML(tr["UI_GENERALINFO3",lang()])), 
        br(), br(),
        value="5"
      )
    )[c(1,which(a>0)+1,6)]
    if (!is.null(glb$currTab) && is.na(match(glb$currTab, c(which(a>0),"5")))) glb$currTab <- "0"
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
  
  output$methDeathTable <- renderTable({
    y <- selYear()
    if (!avInfo[as.character(y),3]) return()
    outputOptions(output, "plotMeth", suspendWhenHidden=FALSE)
    res <- as.data.frame(methDeathTotals(y, y*100+120))
    colnames(res) <- "N"
    rownames(res) <- c(tr["LEG_METH", lang()], tr["LEG_LDEATH", lang()])
    res
  }, rownames=TRUE, colnames=FALSE)
  
  output$plotMeth <- renderPlot({
    y <- selYear()
    if (!avInfo[as.character(y),3]) return()
    outputOptions(output, "plotMeth", suspendWhenHidden=FALSE)
    methDeathPlot(y, y*100+120, lang=lang(), death=FALSE)
  }, res=90)

  output$plotLDeath <- renderPlot({
    y <- selYear()
    if (!avInfo[as.character(y),3]) return()
    outputOptions(output, "plotLDeath", suspendWhenHidden=FALSE)
    methDeathPlot(y, y*100+120, lang=lang(), death=TRUE)
  }, res=90)

  output$plotMethLDeathAge <- renderPlot({
    y <- selYear()
    if (!avInfo[as.character(y),3]) return()
    outputOptions(output, "plotMethLDeathAge", suspendWhenHidden=FALSE)
    methDeathAgePlot(y, y*100+120, lang=lang())
  }, res=90)

  output$plotMomo <- renderPlot({
    y <- selYear()
    if (!avInfo[as.character(y),4]) return()
    outputOptions(output, "plotMomo", suspendWhenHidden=FALSE)
    plotMomo(as.integer(y), lang=lang())
  }, res=88)


  output$downloadSentinel <- downloadHandler(
    filename = function() { 
      y <- selYear()
      sprintf("sentinel_%s-%s.xls", y-1, y)
    },
    content = function(file) {
      y <- selYear()
      out <- sentinel_graph_download(y-(1:0))
      sheetname <- sprintf("sentinel %s-%s", y-1, y)
      WriteXLS("out", file, sheetname, row.names=TRUE)
    }
  )

  output$downloadSwabs <- downloadHandler(
    filename = function() { 
      y <- selYear()
      sprintf("swabs_%s.xls", y)
    },
    content = function(file) {
      y <- selYear()
      out <- swabPlot(y, y*100+120, plot=FALSE)
      sheetname <- sprintf("swabs %s", y)
      WriteXLS("out", file, sheetname, row.names=TRUE)
    }
  )

  output$downloadMeth <- downloadHandler(
    filename = function() { 
      y <- selYear()
      sprintf("meth_%s.xls", y)
    },
    content = function(file) {
      y <- selYear()
      out <- methDeathPlot(y, y*100+120, death=FALSE, plot=FALSE)
      sheetname <- sprintf("meth %s", y)
      WriteXLS("out", file, sheetname, row.names=TRUE)
    }
  )

  output$downloadLDeath <- downloadHandler(
    filename = function() { 
      y <- selYear()
      sprintf("ldeath_%s.xls", y)
    },
    content = function(file) {
      y <- selYear()
      out <- methDeathPlot(y, y*100+120, death=TRUE, plot=FALSE)
      sheetname <- sprintf("meth %s", y)
      WriteXLS("out", file, sheetname, row.names=TRUE)
    }
  )

  output$downloadMethLDeathAge <- downloadHandler(
    filename = function() { 
      y <- selYear()
      sprintf("methLDeathAge_%s.xls", y)
    },
    content = function(file) {
      y <- selYear()
      out <- methDeathAgePlot(y, y*100+120, plot=FALSE)
      sheetname <- sprintf("methLDeathAge %s", y)
      WriteXLS("out", file, sheetname, row.names=TRUE)
    }
  )

  output$downloadMOMO <- downloadHandler(
    filename = function() { 
      y <- selYear()
      sprintf("momo_%s.xls", y)
    },
    content = function(file) {
      y <- selYear()
      out <- plotMomo(as.integer(y), plot=FALSE)
      sheetname <- sprintf("momo %s", y)
      WriteXLS("out", file, sheetname)
    }
  )

  
})

shinyApp(ui = ui, server = server)

