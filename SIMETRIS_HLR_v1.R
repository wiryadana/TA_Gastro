# ==========================================
# PERSIAPAN LINGKUNGAN KERJA
# ==========================================
library(googlesheets4)
library(RSelenium)
library(netstat)
library(rvest)
library(stringr)

# ==========================================
# 1. KONFIGURASI GOOGLE SHEETS
# ==========================================
message("Mempersiapkan Google Sheets...")
gs4_auth() 

# URL Sheet HCC_Systemic_Therapy Anda
sheet_url <- "https://docs.google.com/spreadsheets/d/1A6WNMCTLlWBLoow7D_ak3Yjy1DNDGgSv26aveFiQTJY/edit#gid=0"

data_pasien <- read_sheet(sheet_url, sheet = "MAIN")
daftar_rm <- data_pasien$RM

# ==========================================
# 2. INISIALISASI BROWSER
# ==========================================
message("Membuka browser otomatis Firefox...")
# Ditambahkan check = FALSE agar tidak error 402 Bitbucket
rs_driver_server <- rsDriver(browser = "firefox", port = free_port(), 
                             chromever = NULL, phantomver = NULL, iedrver = NULL, check = FALSE)     
remDr <- rs_driver_server$client

# ==========================================
# 3. LOGIN & 2FA KE PORTAL SIMETRIS
# ==========================================
message("Membuka halaman login HLR Simetris...")
remDr$navigate("https://hlr.rsupsanglah.com/login")
Sys.sleep(3) 

# 1. Masukkan Username
remDr$findElement(using = "id", value = "inputUsername")$sendKeysToElement(list("ka.wiryadana@gmail.com"))

# 2. Masukkan Password, lalu langsung tekan ENTER untuk memicu tombol Login pertama
password_field <- remDr$findElement(using = "id", value = "inputPassword")
password_field$sendKeysToElement(list("W1ryadan@", key = "enter"))

message("Menunggu halaman verifikasi OTP dimuat...")
Sys.sleep(4) # Jeda agar kotak input OTP muncul di browser

# 3. Meminta input OTP via Console R
kode_otp <- readline(prompt = "Cek Telegram @rsngoerah_bot, ketik kode OTP di sini lalu tekan Enter: ")

# 4. Masukkan kode OTP dan tekan ENTER untuk memicu Login kedua
otp_field <- remDr$findElement(using = "id", value = "verifikasinumber")
otp_field$sendKeysToElement(list(kode_otp, key = "enter"))

message("Memverifikasi login...")
Sys.sleep(5) 

# 5. Buka halaman ekspertise Radiologi
remDr$navigate("https://hlr.rsupsanglah.com/lab/expertise")
Sys.sleep(4)

# ==========================================
# 4. LOOPING EKSTRAKSI RADIOLOGI SIMETRIS
# ==========================================
for(i in seq_along(daftar_rm)) {
  no_rm_asli <- as.character(daftar_rm[i])
  if(is.na(no_rm_asli) || str_trim(no_rm_asli) == "") next
  
  message(sprintf("\nMemproses Data %d: RM %s", i, no_rm_asli))
  
  tryCatch({
    # --- LOGIKA BRUTE-FORCE UNTUK RM BURAM ---
    rm_to_try <- c(no_rm_asli)
    if (grepl("^[*Xx]", str_trim(no_rm_asli))) {
      core_rm <- str_extract(str_trim(no_rm_asli), "^[*Xx]\\d{7}")
      if(!is.na(core_rm)) {
        rm_to_try <- sprintf("%d%s", 0:9, str_sub(core_rm, 2, 8))
      }
    }
    
    rm_valid <- NULL
    visit_list <- list()
    
    for(try_rm in rm_to_try) {
      cek_rm <- remDr$findElement(using = "id", value = "cekNoRmExpertiseRad")
      if(!cek_rm$isElementSelected()[[1]]) cek_rm$clickElement()
      
      input_rm <- remDr$findElement(using = "id", value = "noRmExpertiseRad")
      input_rm$clearElement()
      input_rm$sendKeysToElement(list(try_rm))
      
      btn_cari <- remDr$findElement(using = "id", value = "btnCari")
      btn_cari$clickElement()
      Sys.sleep(4) 
      
      js_get_visits <- "return $('#dataKunjExpertiseRad').handsontable('getData');"
      v_list <- tryCatch(remDr$executeScript(js_get_visits), error = function(e) list())
      
      if(length(v_list) > 0 && !is.null(v_list[[1]])) {
        rm_valid <- try_rm
        visit_list <- v_list
        message(sprintf("  -> RM %s valid di SIMETRIS. Ditemukan %d total kunjungan.", rm_valid, length(visit_list)))
        if (try_rm != no_rm_asli) {
          range_write(sheet_url, data = data.frame(rm_valid), range = sprintf("B%d", i + 1), col_names = FALSE)
        }
        break 
      }
    }
    
    hasil_rad <- c("Tidak ada", "Tidak ada", "Tidak ada")
    
    # --- JIKA KUNJUNGAN DITEMUKAN, CARI CT SCAN ABDOMEN ---
    if (!is.null(rm_valid)) {
      idx_ct <- 1 
      
      # Looping mundur (paling tua diekstrak duluan)
      for(v_idx in length(visit_list):1) {
        if (idx_ct > 3) break 
        
        visit <- visit_list[[v_idx]]
        id_kunj <- visit$ID_KUNJ
        if(is.null(id_kunj) || is.na(id_kunj)) next
        
        js_load_visit <- sprintf("loadDataExpertiseRad('%s');", id_kunj)
        remDr$executeScript(js_load_visit)
        Sys.sleep(2.5)
        
        page_source <- remDr$getPageSource()[[1]]
        page_html <- read_html(page_source)
        
        tindakan_nodes <- page_html %>% html_nodes("#dataTindakanExpertiseRad tr.row_tindakan td")
        tindakan_texts <- html_text(tindakan_nodes, trim = TRUE)
        
        if(length(tindakan_texts) > 0) {
          match_idx <- which(grepl("abdomen", tindakan_texts, ignore.case = TRUE) & 
                               grepl("msct|ct scan|ct-scan", tindakan_texts, ignore.case = TRUE))
          
          if(length(match_idx) > 0) {
            for(m_idx in match_idx) {
              if (idx_ct > 3) break
              
              target_index <- m_idx - 1 
              js_click_tindakan <- sprintf("$('#dataTindakanExpertiseRad tr.row_tindakan').eq(%d).click();", target_index)
              remDr$executeScript(js_click_tindakan)
              Sys.sleep(1.5) 
              
              # TARIK TANGGAL, URAIAN, DAN KESAN
              page_source_detail <- remDr$getPageSource()[[1]]
              page_html_detail <- read_html(page_source_detail)
              
              xpath_uraian <- "//div[contains(@class, 'content_uraian_hasil') and contains(@class, 'selected')]//td[normalize-space(text())='Urain' or normalize-space(text())='Uraian']/following-sibling::td[2]"
              xpath_kesan <- "//div[contains(@class, 'content_uraian_hasil') and contains(@class, 'selected')]//td[normalize-space(text())='Kesan']/following-sibling::td[2]"
              xpath_tanggal <- "//div[contains(@class, 'content_uraian_hasil') and contains(@class, 'selected')]//td[contains(normalize-space(text()), 'Tanggal Hasil')]/following-sibling::td[2]"
              
              uraian <- page_html_detail %>% html_node(xpath = xpath_uraian) %>% html_text(trim = TRUE)
              kesan <- page_html_detail %>% html_node(xpath = xpath_kesan) %>% html_text(trim = TRUE)
              tanggal <- page_html_detail %>% html_node(xpath = xpath_tanggal) %>% html_text(trim = TRUE)
              
              # Fallback jika "Tanggal Hasil" kosong, cari "Tgl Reg"
              if(is.na(tanggal) || tanggal == "") {
                tanggal_fallback <- page_html_detail %>% html_node(xpath = "//*[contains(text(), 'Tgl Reg :')]") %>% html_text(trim = TRUE)
                tanggal <- str_trim(gsub(".*Tgl Reg :", "", tanggal_fallback))
                if(is.na(tanggal) || tanggal == "") tanggal <- "Tanggal Tidak Ditemukan"
              }
              
              if(!is.na(uraian) || !is.na(kesan)) {
                uraian <- ifelse(is.na(uraian), "-", uraian)
                kesan <- ifelse(is.na(kesan), "-", kesan)
                
                # Format Teks dengan Header Tanggal Pemeriksaan
                final_text <- sprintf("=== [TGL PEMERIKSAAN: %s] ===\n\nURAIAN:\n%s\n\nKESAN:\n%s", tanggal, uraian, kesan)
                
                hasil_rad[idx_ct] <- str_sub(final_text, 1, 45000)
                idx_ct <- idx_ct + 1
              }
            }
          }
        }
      } # Akhir loop kunjungan
      
      if(idx_ct == 1) hasil_rad[1] <- "Order CT Scan Abdomen tidak ditemukan."
      
    } else {
      hasil_rad[1] <- "RM tidak ditemukan atau kosong."
    }
    
    # --- TULIS HASIL KE GOOGLE SHEETS (KOLOM AL, AM, AN) ---
    alamat_sel_rad <- sprintf("AL%d:AN%d", i + 1, i + 1)
    df_tulis <- data.frame(CT1 = hasil_rad[1], CT2 = hasil_rad[2], CT3 = hasil_rad[3])
    range_write(sheet_url, data = df_tulis, range = alamat_sel_rad, col_names = FALSE)
    
  }, error = function(e) {
    message(sprintf("  -> Error: %s", e$message))
    df_err <- data.frame(CT1 = "System Error", CT2 = "", CT3 = "")
    range_write(sheet_url, data = df_err, range = sprintf("AL%d:AN%d", i + 1, i + 1), col_names = FALSE)
  })
  
  tryCatch({
    remDr$executeScript("$('#btnResetExpertiseRad').click();")
    Sys.sleep(1.5)
  }, error = function(e) {
    remDr$refresh()
    Sys.sleep(4)
  })
}

# ==========================================
# 5. PENYELESAIAN
# ==========================================
message("Pencarian SIMETRIS selesai.")
remDr$close()
rs_driver_server$server$stop()