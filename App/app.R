library(shiny)
library(shinydashboard)
library(Biostrings)
library(tidyverse)
library(DT)
library(ggsci)
library(cowplot)
library(svglite)
library(rclipboard)
library(heatmaply)
library(DBI)
library(RSQLite)

# SQLiteデータベースファイルへの接続
db_file <- "../Data/Callitriche.sqlite"
con <- dbConnect(SQLite(), dbname = db_file)

# search_dataは動作の都合上、起動時に全件読み込み（メモリ上に保持）
search_data <- dbGetQuery(con, "SELECT * FROM search_data")

# RDSメタデータの読み込み
meta_data <- readRDS("../Data/meta_data.rds")

# UI構築に必要なユニーク値を取得
leaf_states    <- levels(meta_data$leaf$state)
tissue_levels  <- levels(meta_data$tissue$tissue)
koga_treatments <- levels(meta_data$kg$treatment)

# その他データの場所指定
data_f <- "../Data/"

# UIスクリプトの読み込み
source('ui_main.R', local = TRUE)
source('ui_graph.R', local = TRUE)
source('ui_hm.R', local = TRUE)
source('ui_msa.R', local = TRUE)
#source('ui_cdef.R', local = TRUE)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Callitriche Browser"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graph", tabName = "tab_Graph", icon = icon("chart-simple")),
      menuItem("Heatmap", tabName = "tab_HM", icon = icon("th")),
      menuItem("MSA", tabName = "tab_MSA", icon = icon("stream")),
      #menuItem("C.deflexa", tabName = "tab_Cdef", icon = icon("envira")),
      menuItem("Info", tabName = "tab_main", icon = icon("file"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem_graph,
      tabItem_hm,
      tabItem_msa,
      #tabItem_cdef,
      tabItem_main
    )
  )
)

server <- shinyServer(function(input, output, session) {
  # セッション終了時にSQLiteの接続をクローズする
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  
  # search_dataはすでにメモリ上にあるので、そのまま利用
  
  # サーバースクリプトの読み込み
  source('server_graph.R', local = TRUE)
  source('server_hm.R', local = TRUE)
  source('server_msa.R', local = TRUE)
  #source('server_cdef.R', local = TRUE)
})

shinyApp(ui, server)