# 1. PENGATURAN GLOBAL (Diletakkan paling atas untuk mencegah warning)
options(sass.cache = FALSE) # Matikan cache sass secara global

# 2. LOAD PACKAGES
packages <- c("shiny", "bslib", "readxl", "dplyr", "psych", "car", 
              "lmtest", "corrplot", "stargazer", "DT", "shinycssloaders")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 3. UI DESIGN (DIBUNGKUS SUPPRESSWARNINGS UNTUK MENGHILANGKAN PESAN CACHE)
ui <- suppressWarnings(
  page_navbar(
    title = "Analisis Faktor-Faktor Upah (Wage)",
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#A7C7E7",    # Biru Pastel
      secondary = "#FDFD96",  # Kuning Pastel
      base_font = font_google("Poppins"),
      heading_font = font_google("Roboto")
    ),
    
    # Custom CSS untuk mempercantik UI
    header = tags$style(HTML("
      .navbar { background-color: #A7C7E7 !important; }
      .nav-link { color: #444 !important; font-weight: 500; }
      .nav-link.active { background-color: #FDFD96 !important; border-radius: 8px; }
      .card-header { background-color: #FDFD96 !important; color: #444 !important; font-weight: bold; }
      .btn-primary { background-color: #A7C7E7 !important; border-color: #8EAFC1 !important; color: #444 !important; }
      .sidebar { background-color: #f8f9fa !important; border-right: 1px solid #eee; }
    ")),
    
    sidebar = sidebar(
      title = "Kontrol Analisis",
      fileInput("file_input", "Upload File Excel (.xlsx)", accept = ".xlsx"),
      actionButton("run_analysis", "Proses Analisis", icon = icon("play"), class = "btn-primary w-100"),
      hr(),
      uiOutput("obs_info") # Menampilkan jumlah observasi secara dinamis
    ),
    
    # --- TAB 1: DATA ---
    nav_panel("Data", icon = icon("table"),
              card(
                card_header("Preview Dataset"),
                DTOutput("raw_table") %>% withSpinner(color="#A7C7E7")
              )
    ),
    
    # --- TAB 2: DESKRIPTIF ---
    nav_panel("Statistik Deskriptif", icon = icon("chart-bar"),
              card(
                card_header("Output psych::describe()"),
                DTOutput("desc_table")
              )
    ),
    
    # --- TAB 3: KORELASI ---
    nav_panel("Korelasi", icon = icon("circle-nodes"),
              layout_column_wrap(
                width = 1/2,
                card(card_header("Matriks Korelasi (Nilai r)"), verbatimTextOutput("cor_matrix_out")),
                card(card_header("Visualisasi Korelasi"), plotOutput("cor_plot_out"))
              )
    ),
    
    # --- TAB 4: REGRESI ---
    nav_panel("Regresi", icon = icon("calculator"),
              card(
                card_header("Output Summary Model OLS"),
                verbatimTextOutput("reg_summary_out")
              )
    ),
    
    # --- TAB 5: UJI ASUMSI KLASIK ---
    nav_panel("Uji Asumsi Klasik", icon = icon("check-double"),
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header("Normalitas & Multikolinearitas"),
                  strong("Shapiro-Wilk (Normalitas):"), verbatimTextOutput("norm_res"),
                  hr(),
                  strong("VIF (Multikolinearitas):"), verbatimTextOutput("vif_res")
                ),
                card(
                  card_header("Hetero & Autokorelasi"),
                  strong("Breusch-Pagan (Heteroskedastisitas):"), verbatimTextOutput("bp_res"),
                  hr(),
                  strong("Durbin-Watson (Autokorelasi):"), verbatimTextOutput("dw_res")
                )
              )
    ),
    
    # --- TAB 6: DIAGNOSTIK ---
    nav_panel("Diagnostik Model", icon = icon("chart-line"),
              card(
                card_header("4 Grafik Diagnostik Residual"),
                plotOutput("diag_plot_out", height = "600px")
              )
    ),
    
    # --- TAB 7: TABEL REGRESI ---
    nav_panel("Tabel Regresi", icon = icon("file-lines"),
              card(
                card_header("Hasil Regresi (Format Akademik)"),
                htmlOutput("stargazer_out")
              )
    ),
    
    # --- TAB 8: RINGKASAN AKHIR ---
    nav_panel("Ringkasan Akhir", icon = icon("star"),
              card(
                card_header("Status Diagnostik Akhir"),
                tableOutput("summary_final_table")
              )
    )
  )
)

# 4. SERVER LOGIC
server <- function(input, output, session) {
  
  # Reactive membaca data
  data_input <- reactive({
    req(input$file_input)
    read_excel(input$file_input$datapath)
  })
  
  # Pembersihan data sesuai logic syntax asli
  data_reg <- eventReactive(input$run_analysis, {
    req(data_input())
    data_input() %>%
      dplyr::select(wage, educ, exper, tenure, female) %>%
      na.omit()
  })
  
  # Sidebar: Info Observasi
  output$obs_info <- renderUI({
    req(data_reg())
    tagList(
      p(icon("database"), " Total Observasi: ", strong(nrow(data_reg()))),
      p(icon("list"), " Variabel: ", strong(ncol(data_reg())))
    )
  })
  
  # Tab Data
  output$raw_table <- renderDT({ 
    req(data_reg())
    datatable(data_reg(), options = list(pageLength = 10)) 
  })
  
  # Tab Deskriptif (psych)
  output$desc_table <- renderDT({
    req(data_reg())
    res <- psych::describe(data_reg())
    datatable(as.data.frame(res), options = list(scrollX = TRUE))
  })
  
  # Tab Korelasi
  output$cor_matrix_out <- renderPrint({ 
    req(data_reg())
    print(round(cor(data_reg()), 3)) 
  })
  
  output$cor_plot_out <- renderPlot({
    req(data_reg())
    cor_matrix <- cor(data_reg())
    corrplot(cor_matrix,method = "square",type = "upper",
             col = colorRampPalette(c("#EAF4FF", "#BFDFFF", "#7FB3FF"))(200),
             addCoef.col = "black",tl.col = "black",number.cex = 0.7)
  })
  
  # Model Regresi OLS
  model_fit <- reactive({ 
    req(data_reg())
    lm(wage ~ educ + exper + tenure + female, data = data_reg()) 
  })
  
  # Tab Regresi Summary
  output$reg_summary_out <- renderPrint({ 
    req(model_fit())
    summary(model_fit()) 
  })
  
  # Tab Uji Asumsi
  output$norm_res <- renderPrint({ shapiro.test(residuals(model_fit())) })
  output$vif_res <- renderPrint({ car::vif(model_fit()) })
  output$bp_res <- renderPrint({ lmtest::bptest(model_fit()) })
  output$dw_res <- renderPrint({ lmtest::dwtest(model_fit()) })
  
  # Tab Diagnostik
  output$diag_plot_out <- renderPlot({
    req(model_fit())
    par(mfrow = c(2, 2))
    plot(model_fit())
  })
  
  # Tab Stargazer
  output$stargazer_out <- renderUI({
    req(model_fit())
    res <- stargazer(model_fit(), type = "html", 
                     title = "Faktor yang Mempengaruhi Upah",
                     dep.var.labels = "Upah (Wage)",
                     covariate.labels = c("Education", "Experience", "Tenure", "Female", "Constant"))
    HTML(res)
  })
  
  # Tab Ringkasan Akhir
  output$summary_final_table <- renderTable({
    req(model_fit())
    m <- model_fit()
    s <- summary(m)
    norm <- shapiro.test(residuals(m))
    vif_v <- car::vif(m)
    bp <- lmtest::bptest(m)
    dw <- lmtest::dwtest(m)
    reset <- lmtest::resettest(m)
    
    data.frame(
      Indikator = c("R-Squared", "Adjusted R-Squared", "Uji F (p-value)", 
                    "Status Normalitas", "Multikolinearitas", 
                    "Heteroskedastisitas", "Autokorelasi", "Spesifikasi Model"),
      Hasil = c(
        round(s$r.squared, 4),
        round(s$adj.r.squared, 4),
        format.pval(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)),
        ifelse(norm$p.value > 0.05, "NORMAL", "TIDAK NORMAL"),
        ifelse(max(vif_v) < 10, "AMAN (VIF < 10)", "ADA MASALAH"),
        ifelse(bp$p.value > 0.05, "AMAN (HOMOSKEDASTIS)", "ADA HETERO"),
        ifelse(dw$p.value > 0.05, "TIDAK ADA AUTOKORELASI", "ADA AUTOKORELASI"),
        ifelse(reset$p.value > 0.05, "MODEL TEPAT", "SALAH SPESIFIKASI")
      )
    )
  }, striped = TRUE, bordered = TRUE)
}

# 5. RUN APP
shinyApp(ui = ui, server = server)
shinyApp(ui, server)
