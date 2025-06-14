#server_cdef.R

cdef_search <- readRDS(paste0(data_f,"Cdef_Cpal.rds"))
cdef <- readRDS(paste0(data_f,"Cdef_trans.rds"))

 
#search functions
Cd_dataInput <- reactive({
  d <- cdef_search
  if (input$Cd_search_ID != "") {
    d <- d %>% filter(grepl(input$Cd_search_ID, Cter))
  } #ID
  if (input$Cd_search_Ortho != "") {
    d <- d %>% filter(grepl(input$Cd_search_Ortho, Cpal))
  } #Symbol
#  d <- d %>% select(gene,hid,sp,Orthogroup,Description, Preferred_name,PFAMs)
  return(d)
})
## search result
output$Cd_result_n <- renderText(paste(c("Hit:", nrow(Cd_dataInput()), "genes"), collapse = " "))

## results table
observeEvent(input$Cd_show_results, {
  output$Cd_search_result <- DT::renderDataTable({
    dat <- isolate(Cd_dataInput())
  },
  options = list(pageLength = 25, autoWidth = T, scrollX = T),
  escape = FALSE
  )
})

## Selected genes
output$Cd_selected.rows <- renderText({
  gl <- paste(isolate(Cd_dataInput())[input$Cd_search_result_rows_selected,"Cter"],collapse=",")
})

## paste to gene_id
observeEvent(input$Cd_paste_selected, {
  updateTextInput(session, "Cd_graph_ID", value = paste(union(Cd_gin(),isolate(Cd_dataInput())[input$Cd_search_result_rows_selected,"Cter"]),collapse=","))
})
## clear gene_id
observeEvent(input$Cd_clear_input, {
  updateTextInput(session, "Cd_graph_ID", value = "")
})

## Graph input observation
Cd_gin <- reactive({
  gin <- input$Cd_graph_ID
  gin <- gsub(" ", "", gin, fixed = TRUE)
  return(unique(strsplit(gin,",")[[1]]))
})

## Graph input
Cd_gid <- eventReactive(input$Cd_make_graph, {
  gid <- input$Cd_graph_ID
  gid <- gsub(" ", "", gid, fixed = TRUE)
  return(unique(strsplit(gid,",")[[1]]))
})

# Plot Cdef

output$Cd_plot.ui <- renderUI({
  plotOutput("Cd_plot", width = input$Cd_wid, height = input$Cd_high)
})

output$Cd_plot <- renderPlot({
  Cd_graph()
})

# Plot leaf development
Cd_graph <- reactive({
  #data selection
  if(length(Cd_gid()) == 0){
    NULL
  }
  else{
    d.Cd <- cdef %>% filter(gene %in% Cd_gid())
    d.Cd$gene <- factor(d.Cd$gene, levels = Cd_gid())
    if(input$Cd_values == "TPM"){
      g.Cd <- ggplot(d.Cd, aes(x=state, y= TPM, color=state))
      ylab.Cd <- "TPM"
    }
    else if (input$Cd_values == "Normalize") {
      g.Cd <- ggplot(d.Cd, aes(x=state, y= norm.count, color=state))
      ylab.Cd <- "TCC Normalized"
    }
    else if (input$Cd_values == "log2(TPM)"){
      g.Cd <- ggplot(d.Cd, aes(x=state, y= log2(TPM+1), color=state))
      ylab.Cd <- "log2(TPM + 1)"
    }
    else {
      g.Cd <- ggplot(d.Cd, aes(x=state, y= log2(norm.count+1), color=state))
      ylab.Cd <- "log2(TCC Normalized + 1)"
    }
    #plot
    g.Cd <- g.Cd  +
      stat_summary(aes(group=state),fun=mean, geom = "point", shape=4, size=5) +
      stat_summary(aes(group=state),fun=mean, geom = "bar", width=.01, fill="white",show.legend=FALSE) +
      geom_point(size = 3, alpha=.7) +
      theme_cowplot() + scale_color_nejm()
    if(input$Cd_scale.free){
      g.Cd <- g.Cd  +facet_wrap(~ gene, scales = "free", ncol = as.integer(input$Cd_num_col))
    }
    else{
      g.Cd <- g.Cd  +facet_wrap(~ gene, ncol = as.integer(input$Cd_num_col))
    }
    g.Cd <- g.Cd +
      labs(y=ylab.Cd) +
      theme(axis.title.x = element_blank(),
            strip.background = element_blank())
    if (!input$Cd_graph.legend){
      g.Cd <- g.Cd + theme(legend.position = "none")
    }
    g.Cd
  }
})

# Export Graph function

output$export_graph <- downloadHandler(
  filename = function(){paste0(input$Cd_g_outname, input$Cd_outfmt)},
  content = function(file){
      save_plot(filename = file, plot = isolate(Cd_graph()), base_width = input$Cd_out_width, base_height= input$Cd_out_height, dpi = input$Cd_dpi, unit="mm")
    
  }
)

