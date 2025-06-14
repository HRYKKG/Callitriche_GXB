library(Biostrings)
library(ggmsa)
library(ggtree)
#library(msa)

fasta_dir=paste0(data_f,"Orthogroup_Sequences")
tree_dir=paste0(data_f,"Gene_Trees")

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
    file <- paste0(fasta_dir,"/",isolate(seq_dataInput()),".fa")
    out <- paste0(readLines(file, encoding = "UTF-8"), collapse = "\n")
  }
  else{
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
  #file <- paste0(fasta_dir,"/",isolate(seq_dataInput()),".fa")
  file <- "/tmp/seq_tmp.fa"
  write(input$seq_mfasta, file, append = FALSE)
  mfa <- readAAStringSet(file, "fasta")
  msa.seqs <- msa(mfa, "ClustalOmega")
  output$msa_plot <- renderPlot(
    width=50+max(width(msa.seqs@unmasked))*22,
    height=100+length(msa.seqs@unmasked)*30,
    res = 120,
    {
    ggmsa(msa.seqs@unmasked, char_width = 0.5, seq_name = T) + theme(axis.text = element_text(size=12)) + geom_seqlogo() + geom_msaBar()
    }
  )
})

## get ggtree
observeEvent(input$show_tree, {
  if(length(seq_dataInput()) == 1){
    output$tree_text <- renderText({return(paste("Phylogenetic tree of the ortholog group", isolate(seq_dataInput())))})
    file <- paste0(tree_dir,"/",isolate(seq_dataInput()),"_tree.txt")
    tree.f <- read.tree(file)
    output$tree_plot <- renderPlot(
      width=600,
      height=600,
      res=100,
      {
      ggtree(tree.f) +  geom_tiplab() + xlim(0, 2)
      ## https://support.bioconductor.org/p/72398/
      }
    )
  }
  else{
    output$tree_text <- renderText({return("ERROR: Please specify a single ortholog group.")})
    output$tree_plot <- NULL
  }
})
