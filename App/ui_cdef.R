# graph generator for cdef
# search + graphs

tabItem_cdef <-
  tabItem(tabName = "tab_Cdef",
    fluidPage(
      titlePanel("Cdef Graph generator"),
      # search bar/results ----------------------
      fluidRow(
        box(width = 12, title = "Cdef Gene Search",
          fluidRow(
            column(6,
              textInput(inputId = "Cd_search_ID",
                label=h4("by Gene name")
              )
            ),
            column(6,
              textInput(inputId = "Cd_search_Ortho",
                label=h4("by Ortholog of Cpal")
              ),
            )
          ),
          hr(),
          fluidRow(
            column(8,
              h3(textOutput(outputId = "Cd_result_n"), align="center")
            ),
            column(4,
              actionButton(inputId = "Cd_show_results",label = "Show Results")
            )
          )
        )
      ),
      # search result table ----------------------
      fluidRow(
        box(title="Results",
          width = 12,
          div(style = 'overflow-x: scroll', DT::dataTableOutput("Cd_search_result"))
        )
      ),
      # search output ------------------------------
      fluidRow(
        box(title="Selected Genes",
          width = 12,
          verbatimTextOutput("Cd_selected.rows"),
          actionButton("Cd_paste_selected","add to graph input"),
          )
      ),
      # graph setting box -------------------------------------------
      fluidRow(
        box(title = "Graph input",
          width=12,
          textInput("Cd_graph_ID",
            label=h4("Gene IDs"),
          ),
          fluidRow(
            column(width=3,
              actionButton("Cd_make_graph","Create Graph")
            ),
            column(width=3,
              actionButton("Cd_clear_input","Clear Input")
            )
          )
        )
      ),
      # General Plot Setting box ------------------------------------
      fluidRow(
        box(title = "Graph setting",
          width=12,
          fluidRow(
            column(width=3,
              radioButtons("Cd_values", label = h5("Y axis value"),
              choices = c("TPM","log2(TPM)", "Normalize", "log2(Normalized)"),
              selected = "log2(Normalized)")
            ),
            column(3,
              radioButtons(inputId = "Cd_num_col", label = h5("Column Number"),
              choices = c(1,2,3,4),
              selected = 2
              )
            ),
            column(3,
              checkboxInput(inputId = "Cd_graph.legend", label = "Legend",
                value = TRUE
              ),
              checkboxInput(inputId = "Cd_scale.free", label = "Scale free",
                value = TRUE
              )
            )
          ),
          fluidRow(
            column(6,
              sliderInput(inputId = "Cd_wid", label = "Graph width",
                min = 100, max = 1500, value=400, step = 100
              )
            ),
            column(6,
              sliderInput(inputId = "Cd_high", label = "Graph height",
                   min = 100, max = 1500, value=400, step = 100
              )
            )
          )
        )
      ),
      # plot box --------------------------------
      fluidRow(
        box(title = "Gragh",width = 12,
          uiOutput("Cd_plot.ui")
        )
      ),
      # Export graph -----------------------------
      fluidRow(
        box(width = 12,
          title = "Graph Export",
          fluidRow(
            column(12,
              textInput(
                inputId = "Cd_g_outname",
                label = h5("output name"),
                value = "Plot"
              )
            )
          ),
          fluidRow(
            column(12,
              radioButtons(
                inputId = "Cd_g_outfmt", label = h5("output format"),
                choices = c(".pdf",".tiff", ".jpg", ".png",".svg",".eps"),
                selected = ".pdf",
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(12,
              sliderInput(inputId = "Cd_dpi",
                label = h5("Dpi"),
                min = 100, max = 1200, value=300, step=100)
            )
          ),
          fluidRow(
            column(6,
              sliderInput(inputId = "Cd_out_width",
                label = h5("Width(mm)"),
                min = 10, max = 300, value=100, step=10
              )
            ),
            column(6,
              sliderInput(inputId = "Cd_out_height",
                label = h5("Height(mm)"),
                min = 10, max = 300, value=100, step=10
              )
            )
          ),
          hr(),
          fluidRow(
            column(12,
            downloadButton("Cd_export_graph", label = "Export")
            )
          )
        )
      )
    )
  )
