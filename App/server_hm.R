# server_hm.R

# --- Search functions ---

hmInput <- reactive({
  d <- search_data
  if (input$filter_GO != "") {
    d <- d %>% filter(grepl(input$filter_GO, GOs))
  }
  if (input$filter_KEGG != "") {
    d <- d %>% filter(grepl(input$filter_KEGG, KEGG_ko))
  }
  if (input$filter_PFAM != "") {
    d <- d %>% filter(grepl(input$filter_PFAM, PFAMs))
  }
  d <- d %>% select(gene) %>% unlist
  return(d)
})

output$hm_result_num <- renderText({
  paste(c("Hit:", length(hmInput()), "genes"), collapse = " ")
})

observeEvent(input$button_hm_input, {
  updateTextInput(session, "gene_list", value = paste(isolate(hmInput()), collapse = "\n"))
})

# --- Heatmap for Leaf ---
observeEvent(input$button_hm, {
  hm.genes <- unlist(strsplit(x = input$gene_list, split = '[\r\n,]'))
  output$hm.ui.l <- renderPlotly({
    if(length(hm.genes) == 0){
      return(NULL)
    } else {
      # SQLiteからデータを取得
      genes <- paste(sprintf("'%s'", hm.genes), collapse = ",")
      states <- paste(sprintf("'%s'", input$sample_select.hm.l), collapse = ",")
      query <- paste0("SELECT * FROM leaf WHERE gene IN (", genes, ") AND state IN (", states, ")")
      d.hm.l <- dbGetQuery(con, query)
      
      # メタデータに基づく因子の順序設定
      # geneはユーザー入力の順序を維持
      d.hm.l$gene  <- factor(d.hm.l$gene, levels = hm.genes)
      d.hm.l$state <- factor(d.hm.l$state, levels = levels(meta_data$leaf$state))
      d.hm.l$tissue<- factor(d.hm.l$tissue, levels = levels(meta_data$leaf$tissue))
      
      if(input$hm.mean){
        d.hm.l <- d.hm.l %>% 
          group_by(gene, state, tissue) %>% 
          summarise(TPM = mean(TPM, na.rm = TRUE),
                    norm.count = mean(norm.count, na.rm = TRUE),
                    name = first(gsub("^\\d-", "", name)),
                    .groups = "drop") 
      }
      
      if(grepl("TPM", input$hm.values)){
        d.hm.l <- d.hm.l %>% select(gene, name, TPM) %>%
          tidyr::spread(key = name, value = TPM) %>% 
          tibble::column_to_rownames("gene") %>% as.matrix()
      } else {
        d.hm.l <- d.hm.l %>% select(gene, name, norm.count) %>%
          tidyr::spread(key = name, value = norm.count) %>% 
          tibble::column_to_rownames("gene") %>% as.matrix()
      }
      
      d.hm.l <- d.hm.l[rowSums(d.hm.l) > 0, ]
      
      if(startsWith(input$hm.values, "log2")){
        d.hm.l <- log2(d.hm.l + 1)
      }
      
      if(input$hm.scale){
        heatmaply::heatmaply(d.hm.l, scale = "row")
      } else {
        heatmaply::heatmaply(d.hm.l)
      }
    }
  })
})

# --- Heatmap for Tissue ---
observeEvent(input$button_hm, {
  hm.genes <- unlist(strsplit(x = input$gene_list, split = '[\r\n,]'))
  output$hm.ui.t <- renderPlotly({
    if(length(hm.genes) == 0){
      return(NULL)
    } else {
      genes <- paste(sprintf("'%s'", hm.genes), collapse = ",")
      tissues <- paste(sprintf("'%s'", input$sample_select.hm.t), collapse = ",")
      query <- paste0("SELECT * FROM tissue WHERE gene IN (", genes, ") AND tissue IN (", tissues, ")")
      d.hm.t <- dbGetQuery(con, query)
      
      d.hm.t$gene  <- factor(d.hm.t$gene, levels = hm.genes)
      d.hm.t$tissue<- factor(d.hm.t$tissue, levels = levels(meta_data$tissue$tissue))
      d.hm.t$state <- factor(d.hm.t$state, levels = levels(meta_data$tissue$state))
      
      if(input$hm.mean){
        d.hm.t <- d.hm.t %>% 
          group_by(gene, state, tissue) %>% 
          summarise(TPM = mean(TPM, na.rm = TRUE),
                    norm.count = mean(norm.count, na.rm = TRUE),
                    name = first(gsub("^\\d-", "", name)),
                    .groups = "drop")
      }
      
      if(grepl("TPM", input$hm.values)){
        d.hm.t <- d.hm.t %>% select(gene, name, TPM) %>%
          tidyr::spread(key = name, value = TPM) %>% 
          tibble::column_to_rownames("gene") %>% as.matrix()
      } else {
        d.hm.t <- d.hm.t %>% select(gene, name, norm.count) %>%
          tidyr::spread(key = name, value = norm.count) %>% 
          tibble::column_to_rownames("gene") %>% as.matrix()
      }
      
      d.hm.t <- d.hm.t[rowSums(d.hm.t) > 0, ]
      
      if(startsWith(input$hm.values, "log2")){
        d.hm.t <- log2(d.hm.t + 1)
      }
      
      if(input$hm.scale){
        heatmaply::heatmaply(d.hm.t, scale = "row")
      } else {
        heatmaply::heatmaply(d.hm.t)
      }
    }
  })
})

# --- Heatmap for Half Leaf ---
observeEvent(input$button_hm, {
  hm.genes <- unlist(strsplit(x = input$gene_list, split = '[\r\n,]'))
  output$hm.ui.hl <- renderPlotly({
    if(length(hm.genes) == 0){
      return(NULL)
    } else {
      genes <- paste(sprintf("'%s'", hm.genes), collapse = ",")
      query <- paste0("SELECT * FROM hl WHERE gene IN (", genes, ")")
      d.hm.hl <- dbGetQuery(con, query)
      #if(input$hm.lowexp){
      #  d.hm.hl <- d.hm.hl %>% group_by(gene) %>% filter(mean(TPM, na.rm = TRUE) >= 1) %>% ungroup()
      #}
      
      if(input$hm.mean){
        d.hm.hl <- d.hm.hl %>%
          mutate(name = paste(state, part, sep = "_")) %>%
          group_by(gene, name) %>%
          summarise(TPM = mean(TPM, na.rm = TRUE),
                    norm.count = mean(norm.count, na.rm = TRUE),
                    .groups = "drop") %>%
          ungroup()
      }
      
      d.hm.hl$gene <- factor(d.hm.hl$gene, levels = hm.genes)

      
      if(grepl("TPM", input$hm.values)){
        d.hm.hl <- d.hm.hl %>% select(gene, name, TPM) %>%
          tidyr::spread(key = name, value = TPM) %>% 
          tibble::column_to_rownames("gene") %>% as.matrix()
      } else {
        d.hm.hl <- d.hm.hl %>% select(gene, name, norm.count) %>%
          tidyr::spread(key = name, value = norm.count) %>% 
          tibble::column_to_rownames("gene") %>% as.matrix()
      }
      
      d.hm.hl <- d.hm.hl[rowSums(d.hm.hl) > 0, ]
      
      if(startsWith(input$hm.values, "log2")){
        d.hm.hl <- log2(d.hm.hl + 1)
      }
      
      if(input$hm.scale){
        heatmaply::heatmaply(d.hm.hl, scale = "row")
      } else {
        heatmaply::heatmaply(d.hm.hl)
      }
    }
  })
})

# --- Heatmap for Koga et al. 2021 ---
observeEvent(input$button_hm, {
  hm.genes <- unlist(strsplit(x = input$gene_list, split = '[\r\n,]'))
  output$hm.ui.k <- renderPlotly({
    if(length(hm.genes) == 0){
      return(NULL)
    } else {
      genes <- paste(sprintf("'%s'", hm.genes), collapse = ",")
      treatments <- paste(sprintf("'%s'", input$sample_select.hm.k), collapse = ",")
      query <- paste0("SELECT * FROM KG WHERE gene IN (", genes, ") AND treatment IN (", treatments, ")")
      d.hm.k <- dbGetQuery(con, query)
      
      # geneはユーザー入力の順序に従う
      d.hm.k$gene <- factor(d.hm.k$gene, levels = hm.genes)
      # treatmentはメタデータに基づく順序で再設定
      d.hm.k$treatment <- factor(as.character(d.hm.k$treatment), levels = levels(meta_data$kg$treatment))
      
      if(input$hm.mean){
        d.hm.k <- d.hm.k %>% 
          group_by(gene, treatment) %>% 
          summarise(TPM = mean(TPM, na.rm = TRUE),
                    norm.count = mean(norm.count, na.rm = TRUE),
                    .groups = "drop")
      }
      
      if(grepl("TPM", input$hm.values)){
        d.hm.k <- d.hm.k %>% select(gene, treatment, TPM) %>%
          tidyr::spread(key = treatment, value = TPM) %>% 
          tibble::column_to_rownames("gene") %>% as.matrix()
      } else {
        d.hm.k <- d.hm.k %>% select(gene, treatment, norm.count) %>%
          tidyr::spread(key = treatment, value = norm.count) %>% 
          tibble::column_to_rownames("gene") %>% as.matrix()
      }
      
      d.hm.k <- d.hm.k[rowSums(d.hm.k) > 0, ]
      
      if(startsWith(input$hm.values, "log2")){
        d.hm.k <- log2(d.hm.k + 1)
      }
      
      if(input$hm.scale){
        heatmaply::heatmaply(d.hm.k, scale = "row")
      } else {
        heatmaply::heatmaply(d.hm.k)
      }
    }
  })
})