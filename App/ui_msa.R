tabItem_msa <-
  tabItem("tab_MSA",
    fluidPage(
      titlePanel("Alignment handler"),
      # search bar/results ----------------------
      fluidRow(
        box(width = 12, title = "Gene Search",
          fluidRow(
            column(4,
              textInput("seq_search_ID",
                label=h4("by Cpal Gene name")
              )
            ),
            column(4,
              textInput("seq_search_Ortho",
                label=h4("by Ortholog")
              ),
              p("IDs for Atha, Slyc, Amaj")
            ),
            column(4,
              textInput("seq_search_OG",
                label=h4("by Orthogroup ID")
              )
            )
          ),
          hr(),
          fluidRow(
            column(6,
              h3(textOutput("seq_result_n"), align="center")
              #h3("Hit: xxx genes", align="center")
            ),
            column(3,
              actionButton("seq_get_fa","Aquire Multifasta")
            ),
            column(3,
              actionButton("show_tree","Show Tree")
            )

          )
        )
      ),
      # alignment viwer ----------------------
      rclipboardSetup(),
      fluidRow(
        box(title="multiple fasta",
          width = 12,
          textAreaInput("seq_mfasta",
            label = h4("fasta input"),
            row = 20),
          hr(),
          fluidRow(
            column(6,
              uiOutput("clip")#,
              #actionButton("mfasta_copy","Copy to clipboard")
            ),
            column(6,
              actionButton("mfasta_align","Show Alignment")
            )
          )
        )
      ),
      # ggmsa viwer ----------------------
      fluidRow(
        box(title="MSA Viewer",
          width = 12,
          p("It takes some time to draw MSA plot. Please be patient!"),
          div(style = 'height:600px; overflow-y: scroll; overflow-x: scroll', plotOutput("msa_plot"))
        )
      ),
      # ggtree viwer ----------------------
      fluidRow(
        box(title="Tree Viewer",
          width = 12,
          textOutput("tree_text"),
          div(style = 'height:600px; overflow-y: scroll; overflow-x: scroll', plotOutput("tree_plot")),
          hr(),
          h4("NOTE:"),
          p("This tree is an output of orthofinder, so please be aware that it is independent from the alignment above.")
        )
      ),
      # Citations
      fluidRow(
        box(title="NOTE",
          width = 12,
          p("MSA: Clustal Omega"),
          p("MSA visualization: ggmsa"),
          p("Tree visualization: ggtree")
        )
      )
    )
  )
