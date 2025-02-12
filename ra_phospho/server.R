# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center")))
                   ))
)

credentials = data.frame(
  username_id = c("huffmanlab", "huffmanlab_alt"),
  password   = sapply(c("huffman7276810", "huffman7276810_alt"), sodium::password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

function(input, output, session) {
  
  # https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["password"][which(credentials$username_id==Username),]
            pasverify <- sodium::password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })

  # output$logoutbtn <- renderUI({
  #   req(USER$login)
  #   tags$li(a(icon("sign-out"), "Logout", 
  #             href="javascript:window.location.reload(true)"),
  #           class = "dropdown", 
  #           style = "background-color: #eee !important; border: 0;
  #                   font-weight: bold; margin:5px; padding: 10px;")
  # })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Phosphoproteomics data", 
                 tabName = "phospho", 
                 icon = icon("table")),
        menuItem("KSEA results", 
                 icon = icon("chart-bar"), 
                 tabName = "ksea")
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        
        tabItem(tabName = "phospho",
                
                # define CSS
                # from - https://www.w3schools.com/tags/tag_div.ASP and
                # https://stackoverflow.com/questions/65587869/r-shiny-how-to-box-a-simple-text-on-a-shiny-page
                tags$head(
                  tags$style(HTML("
                              .Mydiv {
                                  border: 1px outset black;
                                  background-color: lightgrey;
                                  text-align: left;
                              }
                              "))),
                
                h3("RA vs HC Phosphoproteomics Volcano Plot"),
                
                br(),
                
                div(class = "Mydiv",
                    p(
                      "Please select a desired Target Protein or Targeting Kinase (or both)."
                    ),
                    p(
                      "For a selected Target Protein, all phosphosites associated with this protein that are found in this dataset will be highlighted in ",
                      span("red", style = "color:red"), 
                      "in the plot below"
                    ),
                    p(
                      "For a selected Kinase, all phosphosites targeted by this kinase that are found in this dataset will be highlighted in ",
                      span("green", style = "color:green"), 
                      "in the plot below"
                    ),
                    p(
                      "Hovering over an individual point will reveal the identity of the corresponding phosphosite."
                    ),
                    p(
                      "Positive log2FC = Increased phosphorylation in RA."
                    )
                ),
                
                fluidRow(
                  column(
                    width = 6, 
                    selectInput("protein",
                                "Target Protein:",
                                choices = c("", unique(ksea_input_liberal$Gene)),
                                multiple = FALSE)
                  ),
                  column(
                    width = 6, 
                    selectInput("kinase",
                                "Targeting Kinase:",
                                choices = c("", unique(Kinase_Substrate_Links_liberal$Kinase.Gene)), # https://stackoverflow.com/questions/24175997/force-no-default-selection-in-selectinput
                                selected = NULL,
                                multiple = FALSE)
                  )
                ),
                
                # volcano plot
                fluidRow(
                  column(
                    width = 10,
                    plotlyOutput("volcano_plot") %>% 
                      shinycssloaders::withSpinner(type  = 7, color = "#8581e9")
                  ),
                  column(
                    width = 10,
                    textOutput("substrate_name")
                  ),
                  column(
                    width = 10,
                    textOutput("kinase_name")
                  )),
                
                h3("Phosphoproteomics Results Table"),
                br(),
                div(class = "Mydiv",
                    p(
                      "The results presented in the table below correspond to the volcano plot above.")
                ),
                
                # limma results table
                DT::DTOutput("limma_table")
        ),
        
        tabItem(tabName = "ksea",
                
                h3("Kinase-Substrate Enrichment Analysis Plot"),
                
                br(),
                
                div(class = "Mydiv",
                    p(
                      "Please choose a minimum number of target phosphosites and threshold p-value below."
                    ),
                    p(
                      "The resulting KSEA plot will show the z-scores of the kinases that meet the above criteria."
                    ),
                    p(
                      "Bars colored ",
                      span("blue", style = "color:blue"), 
                      "indicate kinases with a p-value less than the user-selected threshold."
                    )
                ),
                
                fluidRow(
                  sliderInput("m_val",
                              "Minimum Number of Phosphosites:",
                              min = min(KSEA_Kinase_Scores_with_padj_liberal$m),
                              max = max(KSEA_Kinase_Scores_with_padj_liberal$m),
                              value = 3,
                              step = 1),
                  sliderInput("p_val",
                              "p-value Threshold:",
                              min = 0.05,
                              max = 1,
                              value = 0.05,
                              step = 0.05
                  )
                ), 
                
                # ksea plot
                fluidRow(
                  column(
                    width = 10,
                    plotOutput("ksea_plot",
                               height = "700px") %>% 
                      shinycssloaders::withSpinner(type  = 7, color = "#8581e9")
                  )),
                
                h3("KSEA Scores Table"),
                br(),
                div(class = "Mydiv",
                    p(
                      "The results presented in the table correspond to the plot above.")
                ),
                
                # ksea scores table
                DT::DTOutput("ksea_scores_table"),
                
                
                
                h3("Kinase Substrate Links Table"),
                br(),
                div(class = "Mydiv",
                    p(
                      "The following table shows the individual kinase-substrate links identified in this dataset.")
                ),
                
                # ksea links table
                DT::DTOutput("ksea_links_table")
                
        )
      )
      
    }
    else {
      loginpage
    }
  })
    # create reactives
    phosphosites <- reactive({
      p <- ksea_input_liberal %>% 
              dplyr::filter(Gene %in% input$protein) %>% 
              dplyr::pull(phosphosite)
      p
    })
    
    substrates <- reactive({
      p <- Kinase_Substrate_Links_liberal %>% 
              dplyr::filter(Kinase.Gene == input$kinase) %>% 
              dplyr::pull(phosphosite)
      p
    })

    # volcano plot
    output$volcano_plot <- renderPlotly({
      
      p <- ksea_input_liberal %>%
        ggplot(aes(log2FoldChange, neg_log_p, text = phosphosite)) +
        geom_point(col = "gray") +
        geom_point(data = filter(ksea_input_liberal, 
                                 phosphosite %in% phosphosites()), 
                                 col = "red") +
        geom_point(data = filter(ksea_input_liberal, 
                                 phosphosite %in% substrates()), 
                                 col = "darkgreen") +
        geom_text_repel(data = filter(ksea_input_liberal, 
                                      phosphosite %in% phosphosites()), 
                        aes(label = phosphosite)) +
        geom_text_repel(data = filter(ksea_input_liberal, 
                                      phosphosite %in% substrates()), 
                        aes(label = phosphosite)) +
        theme_bw() +
        geom_vline(xintercept = 0) +
        geom_vline(xintercept = 2, linetype = "dashed") +
        geom_vline(xintercept = -2, linetype = "dashed") +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
        xlab("Log2FC") +
        ylab("-log10(adjusted p-val)") +
        labs(title = "RA vs HC")
      
      ggplotly(p)
      
    })
    
    # show substrate name
    output$substrate_name <- renderText({
      if(input$protein != ""){
      prot_name <- uniprot_df %>% 
        dplyr::filter(protein_symbol == input$protein) %>% 
        dplyr::pull(`Protein names`)
      
      paste0(input$protein, " = ", prot_name)
      } else {
        ""
      }
    })
    
    # show kinase name
    output$kinase_name <- renderText({
      if(input$kinase != ""){
        prot_name <- uniprot_df %>% 
          dplyr::filter(protein_symbol == input$kinase) %>% 
          dplyr::pull(`Protein names`)
        
        paste0(input$kinase, " = ", prot_name)
      } else {
        ""
      }
    })
    
    # phosphoproteomics results table
    output$limma_table <- DT::renderDataTable({
      ksea_input_liberal %>% 
        dplyr::select(-c(Gene, Residue.Both, p, FC)) %>% 
        dplyr::mutate(neg_log_p = round(neg_log_p, 3),
                      log2FoldChange = round(log2FoldChange, 3)) %>% 
        dplyr::rename(`-log10(adjusted p-val)` = neg_log_p,
                      Log2FC = log2FoldChange) %>% 
        dplyr::left_join(uniprot_df, join_by(Protein == Entry)) %>% 
        dplyr::select(-c(Protein, `Gene Names`, `Protein names`)) %>% 
        dplyr::relocate(protein_symbol) %>% 
        dplyr::rename(Protein = protein_symbol,
                      Phosphosite = phosphosite) %>% 
        DT::datatable(.,
                      filter = 'top', 
                      options = list(
                      pageLength = 5, autoWidth = TRUE),
                      caption = htmltools::tags$caption(
                        style = 'caption-side: bottom; text-align: left;',
                        'Protein = Name of the phosphosite-containing protein; Phosphosite = Location of the phosphosite; Peptide = Peptide sequence that contains the phosphosite; Modifications = Type and location of modification(s) within the peptide sequence; log10(ajusted pval) = log10-transformed FDR-corrected p-value; log2FC = log2-transformed fold-change (RA vs HC)'
                      )
                      )
    })
    
    # ksea plot
    output$ksea_plot <- renderPlot({
      
      KSEA_Kinase_Scores_with_padj_liberal %>% 
        dplyr::mutate(p_sig = if_else(p.value < input$p_val,
                                      "blue",
                                      "black"),
                      p_sig = factor(p_sig,
                                     levels = c("blue", "black"))) %>% 
        dplyr::filter(m >= input$m_val) %>% 
        ggplot(aes(fct_reorder(Kinase.Gene, z.score), z.score)) +
        geom_col(aes(fill = p_sig)) +
        theme_bw() +
        coord_flip() +
        theme(legend.position = "none") +
        scale_fill_manual(values = c("blue", "black")) +
        labs(x = "",
             y = "Kinase z-score")
      
    })
    
    # ksea scores table
    output$ksea_scores_table <- DT::renderDataTable({
      datatable(
        KSEA_Kinase_Scores_with_padj_liberal %>% 
          dplyr::select(-c(log2FC, p_adj)) %>%
          dplyr::mutate(
            mS = round(mS, 3),
            Enrichment = round(Enrichment, 3),
            z.score = round(z.score, 3),
            p.value = round(p.value, 3)
          ),
        filter = 'top', 
        options = list(
          pageLength = 5, autoWidth = TRUE
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: left;',
          "Kinase.Gene = The gene name for each predicted kinase; mS = log2-transformed mean fold-change of the kinases's substrates; Enrichment = Background-adjusted value of the kinase's mS; z.score = Normalized score for each kinase; p.value = p-value computed for each z-score"
        )
      )
    })

    # ksea links table
    output$ksea_links_table <- DT::renderDT({
      datatable(
      Kinase_Substrate_Links_liberal %>% 
        dplyr::mutate(
          log2FC = round(log2FC, 3)
        ),
      filter = 'top', 
      options = list(
        pageLength = 5, autoWidth = TRUE
        ),
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        'Kinase.Gene = The gene name for each predicted kinase; phosphosite = Target phosphosite for a given kinase; Substrate.Gene = The gene name for the protein containing the target phosphosite; Substrate.Mod = Phosphosite location with the target protein; Source = Kinase-substrate database used; log2FC = log2-transformed fold-change of the given phosphosite (RA vs HC)'
      )
      )
    })
}
