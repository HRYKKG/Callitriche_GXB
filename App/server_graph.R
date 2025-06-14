#server_search.R

#search functions
dataInput <- reactive({
 d <- search_data
 if (input$search_ID != "") {
  d <- d %>% filter(grepl(input$search_ID, gene))
  } #ID
 if (input$search_Ortho != "") {
  d <- d %>% 
   filter(grepl(input$search_Ortho, Atha)|
   grepl(input$search_Ortho, Slyc)|
   grepl(input$search_Ortho, Amaj))
  } #Symbol
 if (input$search_Desc != "") {
 d <- d %>% filter(grepl(input$search_Desc, Description))
  } #Descriptions
 if (input$search_CN != "") {
   d <- d %>% filter(grepl(input$search_CN, Preferred_name))
 } #Common name
 if (input$search_GO != "") {
   d <- d %>% filter(grepl(input$search_GO, GOs))
 } #GO
 if (input$search_KEGG != "") {
   d <- d %>% filter(grepl(input$search_KEGG, KEGG_ko))
 } #KEGG
 if (input$search_PFAM != "") {
   d <- d %>% filter(grepl(input$search_PFAM, PFAMs))
 } #PFAM
 d <- d %>% select(gene, HG, has_pair,rep, Orthogroup, Description, Preferred_name, PFAMs)
 return(d)
})
## search result
output$result_n <- renderText(paste(c("Hit:", nrow(dataInput()),
  "genes"), collapse = " "))

## results table
observeEvent(input$show_results, {
  output$search_result <- DT::renderDataTable({
    dat <- isolate(dataInput())
  },
  options = list(pageLength = 25, autoWidth = T, scrollX = T),
  escape = FALSE
  )
})

## Selected genes
output$selected.rows <- renderText({
  gl <- paste(isolate(dataInput())[input$search_result_rows_selected,"gene"],collapse=",")
})

## Export table
output$download_csv <- downloadHandler(
  filename = function(){paste0(input$csv_name, ".csv")},
  content = function(file){
    write.csv(isolate(dataInput()), file = file, fileEncoding = "UTF-8", eol="\r\n", row.names = FALSE)
  }
)

## paste to gene_id
observeEvent(input$paste_selected, {
  updateTextInput(session, "graph_ID", value = paste(union(gin(),isolate(dataInput())[input$search_result_rows_selected,"gene"]),collapse=","))
})
## clear gene_id
observeEvent(input$clear_input, {
  updateTextInput(session, "graph_ID", value = "")
})

## Graph input observation
gin <- reactive({
  gin <- input$graph_ID
  gin <- gsub(" ", "", gin, fixed = TRUE)
  return(unique(strsplit(gin,",")[[1]]))
})

## Graph input
gid <- eventReactive(input$make_graph, {
  gid <- input$graph_ID
  gid <- gsub(" ", "", gid, fixed = TRUE)
  return(unique(strsplit(gid,",")[[1]]))
})

# --- Plot Leaf Development ---
output$plot.ui.l <- renderUI({
  plotOutput("plot.l", width = input$wid, height = input$high)
})

output$plot.l <- renderPlot({
  graph.l()
})

graph.l <- reactive({
  if(length(gid()) == 0){
    return(NULL)
  } else {
    # SQLクエリ作成：ユーザー入力に応じてleafテーブルから取得
    genes <- paste(sprintf("'%s'", gid()), collapse = ",")
    states <- paste(sprintf("'%s'", input$sample_select.l), collapse = ",")
    query <- paste0("SELECT * FROM leaf WHERE gene IN (", genes, ") AND state IN (", states, ")")
    d.l <- dbGetQuery(con, query)
    
    # geneはユーザー入力の順序に従う
    d.l$gene <- factor(d.l$gene, levels = gid())
    # メタデータに基づく因子の順序設定
    d.l$state <- factor(d.l$state, levels = levels(meta_data$leaf$state))
    d.l$tissue <- factor(d.l$tissue, levels = levels(meta_data$leaf$tissue))
    
    # Y軸の値の選択
    if(input$values == "TPM"){
      g.l <- ggplot(d.l, aes(x = tissue, y = TPM, color = state))
      ylab.l <- "TPM"
    } else if (input$values == "Normalize") {
      g.l <- ggplot(d.l, aes(x = tissue, y = norm.count, color = state))
      ylab.l <- "TCC Normalized"
    } else if (input$values == "log2(TPM)"){
      g.l <- ggplot(d.l, aes(x = tissue, y = log2(TPM+1), color = state))
      ylab.l <- "log2(TPM + 1)"
    } else {
      g.l <- ggplot(d.l, aes(x = tissue, y = log2(norm.count+1), color = state))
      ylab.l <- "log2(TCC Normalized + 1)"
    }
    
    # プロット作成
    g.l <- g.l +
      stat_summary(aes(group = state), fun = mean, geom = "line", position = position_dodge(width = 0.5)) +
      stat_summary(aes(group = state), fun = mean, geom = "point", shape = 4, size = 5, position = position_dodge(width = 0.5)) +
      geom_point(size = 3, alpha = 0.7, position = position_dodge(width = 0.5)) +
      theme_cowplot() + scale_color_nejm()
    
    if(input$scale.free){
      g.l <- g.l + facet_wrap(~ gene, scales = "free", ncol = as.integer(input$num_col))
    } else {
      g.l <- g.l + facet_wrap(~ gene, ncol = as.integer(input$num_col))
    }
    
    g.l <- g.l +
      labs(y = ylab.l) +
      theme(axis.title.x = element_blank(), strip.background = element_blank())
    
    if (!input$graph.legend){
      g.l <- g.l + theme(legend.position = "none")
    }
    return(g.l)
  }
})

# --- Plot Tissue ---
output$plot.ui.t <- renderUI({
  plotOutput("plot.t", width = input$wid, height = input$high)
})

output$plot.t <- renderPlot({
  graph.t()
})

graph.t <- reactive({
  if(length(gid()) == 0){
    return(NULL)
  } else {
    genes <- paste(sprintf("'%s'", gid()), collapse = ",")
    tissues <- paste(sprintf("'%s'", input$sample_select.t), collapse = ",")
    query <- paste0("SELECT * FROM tissue WHERE gene IN (", genes, ") AND tissue IN (", tissues, ")")
    d.t <- dbGetQuery(con, query)
    
    d.t$gene <- factor(d.t$gene, levels = gid())
    # メタデータに基づく順序設定
    d.t$tissue <- factor(d.t$tissue, levels = levels(meta_data$tissue$tissue))
    d.t$state <- factor(d.t$state, levels = levels(meta_data$tissue$state))
    
    if(input$values == "TPM"){
      g.t <- ggplot(d.t, aes(x = tissue, y = TPM, color = state))
      ylab.t <- "TPM"
    } else if (input$values == "Normalize"){
      g.t <- ggplot(d.t, aes(x = tissue, y = norm.count, color = state))
      ylab.t <- "TCC Normalized"
    } else if (input$values == "log2(TPM)"){
      g.t <- ggplot(d.t, aes(x = tissue, y = log2(TPM+1), color = state))
      ylab.t <- "log2(TPM + 1)"
    } else {
      g.t <- ggplot(d.t, aes(x = tissue, y = log2(norm.count+1), color = state))
      ylab.t <- "log2(TCC Normalized + 1)"
    }
    
    g.t <- g.t +
      stat_summary(aes(group = state), fun = mean, geom = "point", shape = 4, size = 5, position = position_dodge(width = 0.5)) +
      stat_summary(aes(group = state), fun = mean, geom = "bar", width = 0.01, fill = "white", position = position_dodge(width = 0.5), show.legend = FALSE) +
      geom_point(size = 3, alpha = 0.7, position = position_dodge(width = 0.5)) +
      theme_cowplot() + scale_color_nejm()
    
    if(input$scale.free){
      g.t <- g.t + facet_wrap(~ gene, scales = "free", ncol = as.integer(input$num_col))
    } else {
      g.t <- g.t + facet_wrap(~ gene, ncol = as.integer(input$num_col))
    }
    
    g.t <- g.t +
      labs(y = ylab.t) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5),
            strip.background = element_blank())
    
    if (!input$graph.legend){
      g.t <- g.t + theme(legend.position = "none")
    }
    
    return(g.t)
  }
})

# --- Plot Half Leaf ---
output$plot.ui.hl <- renderUI({
  plotOutput("plot.hl", width = input$wid, height = input$high)
})

output$plot.hl <- renderPlot({
  graph.hl()
})

graph.hl <- reactive({
  if(length(gid()) == 0){
    return(NULL)
  } else {
    genes <- paste(sprintf("'%s'", gid()), collapse = ",")
    query <- paste0("SELECT * FROM hl WHERE gene IN (", genes, ")")
    d.hl <- dbGetQuery(con, query)
    # 既存の処理: state と part を結合
    d.hl$part <- paste(d.hl$state, d.hl$part, sep = "_")
    d.hl$gene <- factor(d.hl$gene, levels = gid())
    # メタデータに基づく順序設定
    d.hl$state <- factor(d.hl$state, levels = levels(meta_data$hl$state))
    d.hl$tissue <- factor(d.hl$tissue, levels = levels(meta_data$hl$tissue))
    
    if(input$values == "TPM"){
      g.hl <- ggplot(d.hl, aes(x = part, y = TPM, color = state))
      ylab.hl <- "TPM"
    } else if (input$values == "Normalize"){
      g.hl <- ggplot(d.hl, aes(x = part, y = norm.count, color = state))
      ylab.hl <- "TCC Normalized"
    } else if (input$values == "log2(TPM)"){
      g.hl <- ggplot(d.hl, aes(x = part, y = log2(TPM+1), color = state))
      ylab.hl <- "log2(TPM + 1)"
    } else {
      g.hl <- ggplot(d.hl, aes(x = part, y = log2(norm.count+1), color = state))
      ylab.hl <- "log2(TCC Normalized + 1)"
    }
    
    g.hl <- g.hl +
      stat_summary(fun = mean, geom = "point", shape = 4, size = 5, position = position_dodge(width = 0.5)) +
      stat_summary(fun = mean, geom = "bar", width = 0.01, fill = "white", position = position_dodge(width = 0.5), show.legend = FALSE) +
      geom_point(size = 3, alpha = 0.7, position = position_dodge(width = 0.5)) +
      theme_cowplot() + scale_color_nejm()
    
    if(input$scale.free){
      g.hl <- g.hl + facet_wrap(~ gene, scales = "free", ncol = as.integer(input$num_col))
    } else {
      g.hl <- g.hl + facet_wrap(~ gene, ncol = as.integer(input$num_col))
    }
    
    g.hl <- g.hl +
      labs(y = ylab.hl) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5),
            strip.background = element_blank())
    
    if (!input$graph.legend){
      g.hl <- g.hl + theme(legend.position = "none")
    }
    
    return(g.hl)
  }
})
# --- Plot Time Course ---
output$plot.ui.tc <- renderUI({
  plotOutput("plot.tc", width = input$wid, height = input$high)
})

output$plot.tc <- renderPlot({
  graph.tc()
})

graph.tc <- reactive({
  if(length(gid()) == 0){
    return(NULL)
  } else {
    genes <- paste(sprintf("'%s'", gid()), collapse = ",")
    query <- paste0("SELECT * FROM timecourse WHERE gene IN (", genes, ")")
    d.tc <- dbGetQuery(con, query)
    
    d.tc$gene <- factor(d.tc$gene, levels = gid())
    d.tc$t.labels <- factor(d.tc$t.labels, levels = levels(meta_data$tc$t.labels))
    d.tc$state <- factor(d.tc$state, levels = levels(meta_data$tc$state)) 
    
    if(input$values == "TPM"){
      g.tc <- ggplot(d.tc, aes(x = t.labels, y = TPM, color = state))
      ylab.tc <- "TPM"
    } else if (input$values == "Normalize"){
      g.tc <- ggplot(d.tc, aes(x = t.labels, y = norm.count, color = state))
      ylab.tc <- "TCC Normalized"
    } else if (input$values == "log2(TPM)"){
      g.tc <- ggplot(d.tc, aes(x = t.labels, y = log2(TPM+1), color = state))
      ylab.tc <- "log2(TPM + 1)"
    } else {
      g.tc <- ggplot(d.tc, aes(x = t.labels, y = log2(norm.count+1), color = state))
      ylab.tc <- "log2(TCC Normalized + 1)"
    }
    
    g.tc <- g.tc +
      stat_summary(aes(group = state), fun = mean, geom = "line", position = position_dodge(width = 0.5)) +
      stat_summary(aes(group = state), fun = mean, geom = "point", shape = 4, size = 5, position = position_dodge(width = 0.5)) +
      geom_point(size = 3, alpha = 0.7, position = position_dodge(width = 0.5)) +
      theme_cowplot() + scale_color_nejm()
    
    if(input$scale.free){
      g.tc <- g.tc + facet_wrap(~ gene, scales = "free", ncol = as.integer(input$num_col))
    } else {
      g.tc <- g.tc + facet_wrap(~ gene, ncol = as.integer(input$num_col))
    }
    
    g.tc <- g.tc +
      labs(y = ylab.tc) +
      theme(axis.title.x = element_blank(), strip.background = element_blank())
    
    if (!input$graph.legend){
      g.tc <- g.tc + theme(legend.position = "none")
    }
    
    return(g.tc)
  }
})

# --- Plot Koga et al. 2021 ---
output$plot.ui.k <- renderUI({
  plotOutput("plot.k", width = input$wid, height = input$high)
})

output$plot.k <- renderPlot({
  graph.k()
})

graph.k <- reactive({
  if(length(gid()) == 0){
    return(NULL)
  } else {
    genes <- paste(sprintf("'%s'", gid()), collapse = ",")
    treatments <- paste(sprintf("'%s'", input$sample_select.k), collapse = ",")
    query <- paste0("SELECT * FROM KG WHERE gene IN (", genes, ") AND treatment IN (", treatments, ")")
    d.k <- dbGetQuery(con, query)
    
    d.k$gene <- factor(d.k$gene, levels = gid())
    # メタデータに基づく治療条件の順序設定
    d.k$treatment <- factor(d.k$treatment, levels = levels(meta_data$kg$treatment))
    
    if(input$values == "TPM"){
      g.k <- ggplot(d.k, aes(x = treatment, y = TPM, color = treatment))
      ylab.k <- "TPM"
    } else if (input$values == "Normalize"){
      g.k <- ggplot(d.k, aes(x = treatment, y = norm.count, color = treatment))
      ylab.k <- "TCC Normalized"
    } else if (input$values == "log2(TPM)"){
      g.k <- ggplot(d.k, aes(x = treatment, y = log2(TPM+1), color = treatment))
      ylab.k <- "log2(TPM + 1)"
    } else {
      g.k <- ggplot(d.k, aes(x = treatment, y = log2(norm.count+1), color = treatment))
      ylab.k <- "log2(TCC Normalized + 1)"
    }
    
    g.k <- g.k +
      stat_summary(aes(group = treatment), fun = mean, geom = "point", shape = 4, size = 5) +
      stat_summary(aes(group = treatment), fun = mean, geom = "bar", width = 0.01, fill = "white", show.legend = FALSE) +
      geom_point(size = 3, alpha = 0.7) +
      theme_cowplot() + scale_color_nejm()
    
    if(input$scale.free){
      g.k <- g.k + facet_wrap(~ gene, scales = "free", ncol = as.integer(input$num_col))
    } else {
      g.k <- g.k + facet_wrap(~ gene, ncol = as.integer(input$num_col))
    }
    
    g.k <- g.k +
      labs(y = ylab.k) +
      theme(axis.title.x = element_blank(), strip.background = element_blank())
    
    if (!input$graph.legend){
      g.k <- g.k + theme(legend.position = "none")
    }
    
    return(g.k)
  }
})


# Export Graph function
output$selected.tab <- reactive({input$tabs})

output$export_graph <- downloadHandler(
  filename = function(){paste0(input$g_outname, input$g_outfmt)},
  content = function(file){
    if(input$tabs == "Tissue"){
      save_plot(filename = file, plot = isolate(graph.t()), base_width = input$out_width, base_height= input$out_height, dpi = input$dpi, unit="mm")
    }
    else if(input$tabs == "Koga et al., 2021"){
      save_plot(filename = file, plot = isolate(graph.k()), base_width = input$out_width, base_height= input$out_height, dpi = input$dpi, unit="mm")
    }
    else if(input$tabs == "Sub TimeCourse"){
      save_plot(filename = file, plot = isolate(graph.tc()), base_width = input$out_width, base_height= input$out_height, dpi = input$dpi, unit="mm")
    }
    else{
      save_plot(filename = file, plot = isolate(graph.l()), base_width = input$out_width, base_height= input$out_height, dpi = input$dpi, unit="mm")
    }
  }
)

### data download
graph.data <- reactive({
  switch(input$tabs,
         "Tissue" = dbGetQuery(con, paste0("SELECT * FROM tissue WHERE gene IN ('", paste(gid(), collapse = "','"), "')")),
         "Koga et al., 2021" = dbGetQuery(con, paste0("SELECT * FROM KG WHERE gene IN ('", paste(gid(), collapse = "','"), "')")),
         "Sub TimeCourse" = dbGetQuery(con, paste0("SELECT * FROM timecourse WHERE gene IN ('", paste(gid(), collapse = "','"), "')")),
         "HalfLeaf" = dbGetQuery(con, paste0("SELECT * FROM hl WHERE gene IN ('", paste(gid(), collapse = "','"), "')")),
         dbGetQuery(con, paste0("SELECT * FROM leaf WHERE gene IN ('", paste(gid(), collapse = "','"), "')"))
  )
})

output$export_table_data <- downloadHandler(
  filename = function() {
    paste0("graph_data_", tolower(gsub(" ", "_", input$tabs)), ".tsv")
  },
  content = function(file) {
    write.table(graph.data(), file, sep = "\t", quote = FALSE, row.names = FALSE)
  }
)


observeEvent(input$copy_clipboard, {
  showModal(modalDialog(
    title = "Copy manually",
    "Select and copy the table below:",
    renderPrint({ head(graph.data(), n = 10) }),
    easyClose = TRUE
  ))
})