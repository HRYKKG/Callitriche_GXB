# graph generator
# search + graphs

# graph generator UI
tabItem_graph <-
  tabItem("tab_Graph",
    fluidPage(
      titlePanel("Graph generator"),
      # search bar/results ----------------------
      fluidRow(
        box(width = 12, title = "Gene Search",
          fluidRow(
            column(4,
              textInput("search_ID",
                label = h4("by Gene name")
              )
            ),
            column(4,
              textInput("search_Ortho",
                label = h4("by Ortholog")
              ),
              p("IDs for Atha, Slyc, Amaj")
            ),
            column(4,
              textInput("search_Desc",
                label = h4("by Description")
              )
            )
          ),
          fluidRow(
            column(4,
              textInput("search_CN",
                label = h4("by Common Name")
              )
            ),
            column(4,
              textInput("search_GO",
                label = h4("by GO term ID")
              ),
              a("GO resource",
                href = "http://geneontology.org/",
                target = "_blank", rel = "noopener noreferrer"
              )
            ),
            column(4,
              textInput("search_KEGG",
                label = h4("by KEGG ontology ID")
              ),
              HTML("<p>e.g. K05282: <a href='https://www.genome.jp/kegg/ko.html' target='_blank' rel='noopener noreferrer'> KEGG</a></p>")
            )
          ),
          fluidRow(
            column(4,
              textInput("search_PFAM",
                label = h4("by PFAM domain")
              )
            ),
            column(4),
            column(4)
          ),
          hr(),
          fluidRow(
            column(8,
              h3(textOutput("result_n"), align = "center")
            ),
            column(4,
              actionButton("show_results", "Show Results")
            )
          )
        )
      ),
      # search result table ----------------------
      fluidRow(
        box(title = "Results",
          width = 12,
          div(style = 'overflow-x: scroll', DT::dataTableOutput("search_result"))
        )
      ),
      # search output ------------------------------
      fluidRow(
        box(title = "Selected Genes",
          width = 6,
          verbatimTextOutput("selected.rows"),
          actionButton("paste_selected", "add to graph input")
        ),
        box(title = "Table Export",
          width = 6,
          textInput("csv_name", label = "File name", value = "result_table"),
          downloadButton("download_csv", label = "download CSV file")
        )
      ),
      # graph setting box -------------------------------------------
      fluidRow(
        box(title = "Graph input",
          width = 12,
          textInput("graph_ID", label = h4("Gene IDs")),
          fluidRow(
            column(width = 3,
              actionButton("make_graph", "Create Graph")
            ),
            column(width = 3,
              actionButton("clear_input", "Clear Input")
            )
          )
        )
      ),
      # General Plot Setting box ------------------------------------
      fluidRow(
        box(title = "Graph setting",
          width = 12,
          fluidRow(
            column(width = 3,
              radioButtons("values", label = h5("Y axis value"),
                choices = c("TPM", "log2(TPM)", "Normalize", "log2(Normalized)"),
                selected = "log2(Normalized)"
              )
            ),
            column(3,
              radioButtons(inputId = "num_col", label = h5("Column Number"),
                choices = c(1, 2, 3, 4),
                selected = 2
              )
            ),
            column(3,
              checkboxInput(inputId = "graph.legend", label = "Legend", value = TRUE),
              checkboxInput(inputId = "scale.free", label = "Scale free", value = TRUE)
            )
          ),
          fluidRow(
            column(6,
              sliderInput(inputId = "wid", label = "Graph width",
                min = 100, max = 1500, value = 800, step = 100
              )
            ),
            column(6,
              sliderInput(inputId = "high", label = "Graph height",
                min = 100, max = 1500, value = 600, step = 100
              )
            )
          )
        )
      ),
      # plot box --------------------------------
      fluidRow(
        tabBox(width = 12, id = "tabs", title = "Plot",
          # Leaf tab --------------------------------
          tabPanel("Leaf Development", id = "leaf",
            uiOutput("plot.ui.l"),
            hr(),
            # Leaf tab: sample selection  --------------------------------
            checkboxGroupInput("sample_select.l",
              label = h5("Sample select"),
              choices = leaf_states,      # ここを修正
              selected = leaf_states,
              inline = TRUE
            ),
            HTML("<p><b>*ABA</b> = ABA 10<sup>-8</sup> M in Sub (Submerged leaf with more stomata)</p>")
          ),
          # Tissue tab --------------------------------
          tabPanel("Tissue", id = "tissue",
            uiOutput("plot.ui.t"),
            hr(),
            # Tissue tab: sample selection  --------------------------------
            checkboxGroupInput("sample_select.t",
              label = h5("Sample select"),
              choices = tissue_levels,    # ここを修正
              selected = tissue_levels,
              inline = TRUE
            )
          ),
          # HL tab --------------------------------
          tabPanel("HalfLeaf", id = "HL",
            uiOutput("plot.ui.hl")
          ),
          # Timecourse tab --------------------------------
          tabPanel("Sub TimeCourse", id = "TC",
            uiOutput("plot.ui.tc")
          ),
          # Koga 2021 tab ---------------------------------
          tabPanel("Koga et al., 2021", id = "koga",
            uiOutput("plot.ui.k"),
            hr(),
            # Koga 2021 tab: sample selection  --------------------------------
            checkboxGroupInput("sample_select.k",
              label = h5("Sample select"),
              choices = koga_treatments,  # ここを修正
              selected = koga_treatments,
              inline = TRUE
            ),
            HTML("<p><b>*ABA</b> = ABA 10<sup>-7</sup> M in Sub; <b>Ag</b> = AgNO<sub>3</sub> 10<sup>-7</sup> M in Sub; <b>Uni</b> = Uniconazole 10<sup>-8</sup> M in Sub</p>")
          )
        )
      ),
      # Export graph -----------------------------
      fluidRow(
        column(width = 8,
               box(width = 12,
                   title = "Graph Export",
                   fluidRow(
                     column(6,
                            textInput(inputId = "g_outname",
                                      label = h5("output name"),
                                      value = "Plot"
                            )
                     ),
                     column(6,
                            h5("Active Tab:"),
                            textOutput("selected.tab")
                     )
                   ),
                   fluidRow(
                     column(12,
                            radioButtons(inputId = "g_outfmt", label = h5("output format"),
                                         choices = c(".pdf", ".tiff", ".jpg", ".png", ".svg", ".eps"),
                                         selected = ".pdf",
                                         inline = TRUE
                            )
                     )
                   ),
                   fluidRow(
                     column(12,
                            sliderInput(inputId = "dpi", label = h5("Dpi"),
                                        min = 100, max = 1200, value = 300, step = 100
                            )
                     )
                   ),
                   fluidRow(
                     column(6,
                            sliderInput(inputId = "out_width", label = h5("Width(mm)"),
                                        min = 10, max = 300, value = 100, step = 10
                            )
                     ),
                     column(6,
                            sliderInput(inputId = "out_height", label = h5("Height(mm)"),
                                        min = 10, max = 300, value = 100, step = 10
                            )
                     )
                   ),
                   hr(),
                   fluidRow(
                     column(12,
                            downloadButton("export_graph", label = "Export")
                     )
                   )
               )
        ),
        column(width = 4,
               box(width = 12,
                   title = "Graph Data",
                   downloadButton("export_table_data", label = "Download TSV"),
                   br(), br(),
                   actionButton("copy_clipboard", "Copy preview")
               )
        )
      )
    )
  )