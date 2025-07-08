library(Biostrings)
library(ggmsa)
library(ggtree)
library(msa)

#search functions
seq_dataInput <- reactive({
  seq_d <- search_data
  if (input$seq_search_ID != "") {
    seq_d <- seq_d %>% filter(grepl(input$seq_search_ID, gene))
  } #ID
  if (input$seq_search_Ortho != "") {
    seq_d <- seq_d %>% filter(grepl(input$seq_search_Ortho, Atha)|grepl(input$seq_search_Ortho, Slyc)|grepl(input$seq_search_Ortho, Amaj))
  } #Symbol
  if (input$seq_search_OG != "") {
    seq_d <- seq_d %>% filter(grepl(input$seq_search_OG, Orthogroup))
  } #Symbol
  seq_d <- seq_d %>% distinct(Orthogroup, .keep_all = FALSE) %>% unlist
  return(seq_d)
})

## search result
output$seq_result_n <- renderText({
  if (length(seq_dataInput()) == 1){
    seq_out_txt <- seq_dataInput()
  }
  else{
    seq_out_txt <- paste(c("Hit:", length(seq_dataInput()), "groups"), collapse = " ")
  }
  return(seq_out_txt)
})

## get Multifasta: just outputting the fasta
observeEvent(input$seq_get_fa, {
  if(length(seq_dataInput()) == 1){
    query <- paste0("SELECT fasta FROM orthogroup_data WHERE og_id = '", isolate(seq_dataInput()), "'")
    res <- dbGetQuery(con, query)
    
    if(nrow(res) == 1){
      out <- res$fasta[1]
    } else {
      out <- "ERROR: No sequence found in database."
    }
  } else {
    out <- "ERROR: Please specify a single ortholog group."
  }
  
  updateTextInput(session, "seq_mfasta", value = out)
})

## Copy to clipboard
output$clip <- renderUI({
  rclipButton(
    inputId = "clipbtn",
    label = "Copy to clipboard",
    clipText = input$seq_mfasta,
    icon = icon("clipboard")
  )
})

## get ggMSA
observeEvent(input$mfasta_align, {
  og <- isolate(seq_dataInput())
  if (length(og) != 1) {
    output$msa_plot <- renderPlot({ plot.new(); text(0.5, 0.5, "Please specify a single ortholog group.") })
    return(NULL)
  }
  
  # データベースからFASTA取得
  query <- paste0("SELECT fasta FROM orthogroup_data WHERE og_id = '", og, "'")
  res <- dbGetQuery(con, query)
  
  if (nrow(res) == 1) {
    fasta_text <- res$fasta[1]
    file <- tempfile(fileext = ".fa")
    writeLines(fasta_text, file)
    
    mfa <- readAAStringSet(file, "fasta")
    msa.seqs <- msa(mfa, "ClustalOmega")
    
    output$msa_plot <- renderPlot(
      width = 50 + max(width(msa.seqs@unmasked)) * 22,
      height = 100 + length(msa.seqs@unmasked) * 30,
      res = 120,
      {
        ggmsa(msa.seqs@unmasked, char_width = 0.5, seq_name = TRUE) +
          theme(axis.text = element_text(size = 12)) +
          geom_seqlogo() + geom_msaBar()
      }
    )
  } else {
    output$msa_plot <- renderPlot({ plot.new(); text(0.5, 0.5, "FASTA not found in database.") })
  }
})

## get ggtree
observeEvent(input$show_tree, {
  og <- isolate(seq_dataInput())
  if (length(og) != 1) {
    output$tree_text <- renderText({ "ERROR: Please specify a single ortholog group." })
    output$tree_plot <- NULL
    return(NULL)
  }
  
  output$tree_text <- renderText({ paste("Phylogenetic tree of the ortholog group", og) })
  
  query <- paste0("SELECT tree FROM orthogroup_data WHERE og_id = '", og, "'")
  res <- dbGetQuery(con, query)
  
  if (nrow(res) == 1) {
    tree_str <- res$tree[1]
    tree_file <- tempfile(fileext = ".nwk")
    writeLines(tree_str, tree_file)
    tree.f <- read.tree(tree_file)
    
    output$tree_plot <- renderPlot(
      width = 600,
      height = 600,
      res = 100,
      {
        ggtree(tree.f) + geom_tiplab() + xlim(0, 2)
      }
    )
  } else {
    output$tree_text <- renderText({ "Tree not found in database." })
    output$tree_plot <- NULL
  }
})
