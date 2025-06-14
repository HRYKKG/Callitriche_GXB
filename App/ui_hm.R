# ui_hm.R

# heatmap generator
# search + hm

tabItem_hm <-
  tabItem("tab_HM",
    fluidPage(
      titlePanel("HeatMap generator"),
      # search bar/results ----------------------
      fluidRow(
        box(width = 12, title = "Filter",
          fluidRow(
            column(4,
              textInput("filter_GO",
                label = h4("by GOterm")
              )
            ),
            column(4,
              textInput("filter_KEGG",
                label = h4("by KEGG")
              )
            ),
            column(4,
              textInput("filter_PFAM",
                label = h4("by PFAM domain")
              )
            )
          ),
          hr(),
          fluidRow(
            column(8,
              h3(textOutput("hm_result_num"), align = "center")
            ),
            column(4,
              actionButton("button_hm_input", "paste to input")
            )
          )
        )
      ),
      # search output ------------------------------
      fluidRow(
        box(
          width = 12,
          textAreaInput("gene_list",
                        label = h4("gene list input"),
                        rows = 12),
          # General Plot Setting box ------------------------------------
          hr(),
          fluidRow(
            column(width = 6,
              radioButtons("hm.values", label = h5("heatmap value"),
                choices = c("TPM", "log2(TPM)", "Normalize", "log2(Normalized)"),
                selected = "TPM")
            ),
            column(6,
              checkboxInput(inputId = "hm.scale", label = "Scaled", value = TRUE),
              #checkboxInput(inputId = "hm.lowexp", label = "Ignore low-expression genes", value = TRUE),
              checkboxInput(inputId = "hm.mean", label = "Mean replicates", value = TRUE)
            )
          ),
          fluidRow(
            column(12,
              actionButton("button_hm", "Create Heatmap")
            )
          )
        )
      ),
      # plot box --------------------------------
      fluidRow(
        tabBox(width = 12, height = 900, id = "hm.tabs", title = "Plot",
          # Leaf tab --------------------------------
          tabPanel("Leaf Development", id = "hm.leaf",
            plotlyOutput("hm.ui.l", height = "700px"),
            hr(),
            # Leaf tab: sample selection  --------------------------------
            checkboxGroupInput("sample_select.hm.l",
              label = h4("Sample select"),
              choices = levels(meta_data$leaf$state),
              selected = levels(meta_data$leaf$state),
              inline = TRUE
            ),
            HTML("<p><b>*ABA</b> = ABA 10<sup>-8</sup> M in Sub (Submerged leaf with more stomata)</p>")
          ),  # tabpanel end
          # Tissue tab --------------------------------
          tabPanel("Tissue", id = "hm.tissue",
            plotlyOutput("hm.ui.t", height = "700px"),
            hr(),
            # Tissue tab: sample selection  ---------------
            checkboxGroupInput("sample_select.hm.t",
              label = h4("Sample select"),
              choices = levels(meta_data$tissue$tissue),
              selected = levels(meta_data$tissue$tissue),
              inline = TRUE
            )
          ),  # tabpanel end
          # Half Leaf tab --------------------------------
          tabPanel("Half Leaf", id = "hm.hl",
            plotlyOutput("hm.ui.hl", height = "700px")
          ),  # tabpanel end
          # Timecourse tab --------------------------------
          tabPanel("Sub TimeCourse", id = "hm.TC",
            h2("TBA")
          ),
          # Koga 2021 tab ---------------------------------
          tabPanel("Koga et al., 2021", id = "hm.koga",
            plotlyOutput("hm.ui.k", height = "700px"),
            hr(),
            # Kogaタブ：サンプル選択 -----------------------------
            checkboxGroupInput("sample_select.hm.k",
              label = h4("Sample select"),
              choices = levels(meta_data$kg$treatment),
              selected = levels(meta_data$kg$treatment),
              inline = TRUE
            ),
            HTML("<p><b>*ABA</b> = ABA 10<sup>-7</sup> M in Sub; <b>Ag</b> = AgNO<sub>3</sub> 10<sup>-7</sup> M in Sub; <b>Uni</b> = Uniconazole 10<sup>-8</sup> M in Sub</p>")
          )
        )  # tab box end
      )
    )
  )
