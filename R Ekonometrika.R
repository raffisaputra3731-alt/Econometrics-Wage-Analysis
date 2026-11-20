################################################################################
# TUGAS EKONOMETRIKA: ANALISIS REGRESI LINEAR BERGANDA
# Topik: Faktor-Faktor yang Mempengaruhi Upah (Wage)
################################################################################

# 1. LOAD PACKAGES YANG DIBUTUHKAN
################################################################################
# Install package jika belum tersedia, lalu panggil library-nya
packages <- c("readxl", "dplyr", "psych", "car", "lmtest", "corrplot", "stargazer")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 2. IMPORT DATA DARI EXCEL
################################################################################
# Pilih file Excel saat jendela file explorer muncul
data_original <- readxl::read_excel(file.choose())

# 3. PEMBERSIHAN DATA (CLEANING)
################################################################################
# Memilih variabel sesuai tugas: wage, educ, exper, tenure, female
# Serta menghapus baris yang mengandung data kosong (NA)
data_reg <- data_original %>%
  dplyr::select(wage, educ, exper, tenure, female) %>%
  na.omit()

# 4. STATISTIK DESKRIPTIF
################################################################################
cat("\n--- 1. STATISTIK DESKRIPTIF ---\n")
# Menampilkan Mean, Median, Standar Deviasi, Min, dan Max
print(psych::describe(data_reg))

# 5. MATRIKS KORELASI PEARSON (NILAI r)
################################################################################
cat("\n--- 2. MATRIKS KORELASI (Nilai r) ---\n")
cor_matrix <- cor(data_reg)
print(round(cor_matrix, 3))

# Visualisasi Korelasi agar lebih mudah dibaca
corrplot(cor_matrix,method = "square",type = "upper",
         col = colorRampPalette(c("#EAF4FF", "#BFDFFF", "#7FB3FF"))(200),
         addCoef.col = "black",tl.col = "black",number.cex = 0.7)

# 6. ESTIMASI MODEL REGRESI LINEAR BERGANDA (OLS)
################################################################################
# Model: wage = b0 + b1.educ + b2.exper + b3.tenure + b4.female + e
model <- lm(wage ~ educ + exper + tenure + female, data = data_reg)

cat("\n--- 3. HASIL ESTIMASI MODEL (OUTPUT SUMMARY) ---\n")
summary_model <- summary(model)
print(summary_model)

# 7. UJI ASUMSI KLASIK
################################################################################
cat("\n--- 4. UJI ASUMSI KLASIK ---\n")

# A. Uji Normalitas (Shapiro-Wilk)
# H0: Residual berdistribusi normal
norm_test <- shapiro.test(residuals(model))
cat("p-value Shapiro-Wilk:", norm_test$p.value, "\n")

# B. Uji Multikolinearitas (VIF)
# Syarat: Nilai VIF harus < 10 (atau < 5)
vif_val <- car::vif(model)
cat("Nilai VIF:\n")
print(vif_val)

# C. Uji Heteroskedastisitas (Breusch-Pagan)
# H0: Varians residual konstan (Homoskedastisitas)
bp_test <- lmtest::bptest(model)
cat("p-value Breusch-Pagan:", bp_test$p.value, "\n")

# D. Uji Autokorelasi (Durbin-Watson)
# H0: Tidak terdapat autokorelasi
dw_test <- lmtest::dwtest(model)
cat("Statistik Durbin-Watson:", dw_test$statistic, "\n")
cat("p-value Durbin-Watson:", dw_test$p.value, "\n")

# 8. DIAGNOSTIK MODEL (GRAFIK)
################################################################################
# Menampilkan 4 grafik diagnostik untuk melihat sebaran residual
par(mfrow = c(2, 2))
plot(model)

# 9. TABEL REGRESI STANDAR AKADEMIK (STARGAZER)
################################################################################
cat("\n--- 5. TABEL REGRESI UNTUK LAPORAN ---\n")
stargazer(model, type = "text", 
          title = "Hasil Regresi Faktor-Faktor yang Mempengaruhi Upah",
          dep.var.labels = "Upah (Wage)",
          covariate.labels = c("Education", "Experience", "Tenure", "Female", "Constant"),
          out = "tabel_regresi.txt") # Menyimpan hasil ke file text

# 10. RINGKASAN OTOMATIS UNTUK INTERPRETASI
################################################################################
cat("\n========================================================")
cat("\n           RINGKASAN DIAGNOSTIK AKHIR")
cat("\n========================================================")
cat("\n1. R-Squared          :", round(summary_model$r.squared, 4))
cat("\n2. Adjusted R-Squared :", round(summary_model$adj.r.squared, 4))
cat("\n3. Uji F (p-value)    :", pf(summary_model$fstatistic[1], 
                                    summary_model$fstatistic[2], 
                                    summary_model$fstatistic[3], 
                                    lower.tail = FALSE))
# UJI SPESIFIKASI MODEL (RAMSEY RESET)
reset_test <- lmtest::resettest(model)

cat("\n8. Spesifikasi Model :", 
    ifelse(reset_test$p.value > 0.05,
           "MODEL TEPAT",
           "SALAH SPESIFIKASI"))
# Status Normalitas
cat("\n4. Status Normalitas  :", ifelse(norm_test$p.value > 0.05, 
                                        "NORMAL", "TIDAK NORMAL"))

# Status Multikolinearitas
cat("\n5. Multikolinearitas  :", ifelse(max(vif_val) < 10, 
                                        "AMAN (VIF < 10)", "TERDETEKSI MASALAH"))

# Status Heteroskedastisitas
cat("\n6. Heteroskedastisitas:", ifelse(bp_test$p.value > 0.05, 
                                        "AMAN (HOMOSKEDASTIS)", "ADA GEJALA HETERO"))

# Status Autokorelasi
cat("\n7. Autokorelasi       :", ifelse(dw_test$p.value > 0.05, 
                                        "TIDAK ADA AUTOKORELASI", "ADA AUTOKORELASI"))
cat("\n========================================================\n")