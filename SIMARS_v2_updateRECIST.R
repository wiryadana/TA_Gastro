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
daftar_nama <- data_pasien$NAMA_INISIAL

# ==========================================
# FUNGSI PEMBANTU: PENCOCOKAN NAMA
# ==========================================
is_name_match <- function(nama_excel, nama_web) {
  if(is.na(nama_excel) || is.na(nama_web)) return(FALSE)
  ne <- toupper(str_trim(nama_excel))
  nw <- toupper(str_trim(nama_web))
  if (grepl(ne, nw, fixed = TRUE) || grepl(nw, ne, fixed = TRUE)) return(TRUE)
  words <- strsplit(nw, " ")[[1]]
  words <- words[words != ""]
  inisial_web <- paste(substr(words, 1, 1), collapse = "")
  if (ne == inisial_web) return(TRUE)
  return(FALSE)
}

# ==========================================
# 2. INISIALISASI BROWSER
# ==========================================
message("Membuka browser otomatis Firefox...")
rs_driver_server <- rsDriver(browser = "firefox", port = free_port(), 
                             chromever = NULL, phantomver = NULL, iedrver = NULL, check = FALSE)     
remDr <- rs_driver_server$client

# ==========================================
# 3. LOGIN SIMARS
# ==========================================
message("Membuka halaman login SIMARS...")
remDr$navigate("https://rsupsanglah.com:9024/simrsm/index.php")
Sys.sleep(3)

remDr$findElement(using = "name", value = "username")$sendKeysToElement(list("52664"))
remDr$findElement(using = "xpath", value = "//input[@name='password' and @type='password']")$sendKeysToElement(list("W1ryadana"))

kode_captcha <- readline(prompt = "Lihat Captcha di browser, ketik angkanya di sini, lalu tekan ENTER: ")

captcha_field <- remDr$findElement(using = "name", value = "captcha")
captcha_field$sendKeysToElement(list(kode_captcha))
Sys.sleep(1)

btn_login <- remDr$findElement(using = "css selector", value = "button[type='submit']")
btn_login$clickElement()

message("Login sedang diproses, menunggu halaman dimuat...")
Sys.sleep(6) 

# Tutup pop-up Hakordia di Dashboard
tryCatch({
  remDr$executeScript("if(typeof $.fancybox !== 'undefined') { $.fancybox.close(); }")
  Sys.sleep(2)
}, error = function(e) {})

# ==========================================
# 4. LOOPING EKSTRAKSI DATA RADIOLOGI
# ==========================================
url_pencarian_rad <- "https://rsupsanglah.com:9024/simrsm/index.php?tm=Penunjang%20Medis&glm=Radiologi&lm=List%20Order&mid=1021"

for(i in seq_along(daftar_rm)) {
  no_rm_asli <- as.character(daftar_rm[i])
  nama_target <- as.character(daftar_nama[i])
  
  if(is.na(no_rm_asli) || str_trim(no_rm_asli) == "") next
  message(sprintf("\nMemproses Data %d: RM %s - %s", i, no_rm_asli, nama_target))
  
  tryCatch({
    # A. KEMBALI KE HALAMAN PENCARIAN RADIOLOGI
    remDr$navigate(url_pencarian_rad)
    Sys.sleep(3)
    tryCatch({ remDr$executeScript("if(typeof $.fancybox !== 'undefined') { $.fancybox.close(); }") }, error = function(e) {})
    
    # B. LOGIKA BRUTE-FORCE UNTUK RM BURAM
    rm_to_try <- c(no_rm_asli)
    if (grepl("^[*Xx]", str_trim(no_rm_asli))) {
      core_rm <- str_extract(str_trim(no_rm_asli), "^[*Xx]\\d{7}")
      if(!is.na(core_rm)) {
        rm_to_try <- sprintf("%d%s", 0:9, str_sub(core_rm, 2, 8))
      }
    }
    
    rm_valid <- NULL
    
    for(try_rm in rm_to_try) {
      input_rm <- remDr$findElement(using = "name", value = "norm")
      input_rm$clearElement()
      input_rm$sendKeysToElement(list(try_rm))
      
      btn_cari <- remDr$findElement(using = "name", value = "cari")
      btn_cari$clickElement()
      Sys.sleep(3.5) 
      
      js_check_name <- "
        var tds = document.querySelectorAll('table.bordered > tbody > tr > td:nth-child(4)');
        return tds.length > 0 ? tds[0].innerText.trim() : null;
      "
      nama_web <- remDr$executeScript(js_check_name)[[1]]
      
      if(!is.null(nama_web)) {
        if (is_name_match(nama_target, nama_web)) {
          rm_valid <- try_rm
          message(sprintf("  -> RM %s valid untuk %s", rm_valid, nama_web))
          if (try_rm != no_rm_asli) {
            range_write(sheet_url, data = data.frame(rm_valid), range = sprintf("B%d", i + 1), col_names = FALSE)
          }
          break 
        }
      }
    }
    
    # Siapkan placeholder kosong untuk 3 kolom (CT1, CT2, CT3)
    hasil_rad <- c("Tidak ada", "Tidak ada", "Tidak ada")
    
    # C. JIKA RM DITEMUKAN, CARI SEMUA URL CT SCAN ABDOMEN
    if (!is.null(rm_valid)) {
      
      # PERBAIKAN: Hanya cek teks pada baris (tr) spesifik, bukan seluruh tabel
      js_get_all_target_urls <- "
        var links = document.querySelectorAll('a[target=\"_blank\"]');
        var urls = [];
        for(var i=0; i<links.length; i++) {
          var a = links[i];
          if(a.innerHTML.includes('Bacaan')) {
            var row = a.closest('tr[valign=\"top\"]');
            if(!row) row = a.closest('tr');
            
            if(row) {
              var text = row.innerText.toLowerCase();
              if(text.includes('ct scan') && text.includes('abdomen')) {
                 urls.push(a.href);
              }
            }
          }
        }
        return urls;
      "
      target_urls <- unlist(remDr$executeScript(js_get_all_target_urls))
      
      if (length(target_urls) > 0) {
        # Balik urutan: paling tua (diagnostik) diproses duluan
        target_urls <- rev(target_urls)
        message(sprintf("  -> Ditemukan %d order CT Scan Abdomen. Mengekstrak...", length(target_urls)))
        
        idx_ct <- 1 # Indikator pengisian kolom (1=RAW, 2=RECIST_1, 3=RECIST_2)
        
        for (j in seq_along(target_urls)) {
          if (idx_ct > 3) break # Maksimal 3 CT Scan
          
          remDr$navigate(target_urls[j])
          Sys.sleep(5) # Tunggu pop-up bacaan
          
          js_extract_bacaan <- "
            var tanggal = 'Tanggal Tidak Ditemukan';
            var trs = document.querySelectorAll('#table-print tr');
            
            for(var i=0; i<trs.length; i++) {
                var tds = trs[i].querySelectorAll('td');
                if(tds.length === 3 && tds[0].innerText.includes('Tanggal') && !tds[0].innerText.includes('Lahir')) {
                    tanggal = tds[2].innerText.trim();
                    break;
                }
            }
            
            var bacaanTds = document.querySelectorAll('#table-print td[colspan=\"3\"]');
            var res = [];
            for(var i=0; i<bacaanTds.length; i++) {
               var txt = bacaanTds[i].innerText.trim();
               if(txt.length > 10) res.push(txt);
            }
            
            var header = '=== [TGL PEMERIKSAAN: ' + tanggal + '] ===\\n\\n';
            return header + res.join('\\n\\n====================\\n\\n');
          "
          bacaan_text <- remDr$executeScript(js_extract_bacaan)
          
          if (!is.null(bacaan_text[[1]]) && bacaan_text[[1]] != "") {
            teks_bacaan <- str_sub(bacaan_text[[1]], 1, 45000)
            
            # FILTER KONTEN: Pastikan teks hasil bacaan benar-benar mengandung kata MSCT atau CT Scan
            if (grepl("MSCT|CT Scan|CT-Scan", teks_bacaan, ignore.case = TRUE)) {
              hasil_rad[idx_ct] <- teks_bacaan
              idx_ct <- idx_ct + 1
            } else {
              message("  -> Skip: Teks bacaan tidak mengandung MSCT/CT Scan (USG/X-Ray).")
            }
            
          } else {
            message("  -> Skip: Teks kosong.")
          }
        }
        
      } else {
        hasil_rad[1] <- "Order CT Scan Abdomen tidak ditemukan."
      }
    } else {
      hasil_rad[1] <- "Gagal mencocokkan RM."
    }
    
    # D. TULIS HASIL KE 3 KOLOM SEKALIGUS (AI, AJ, AK)
    alamat_sel_rad <- sprintf("AI%d:AK%d", i + 1, i + 1)
    df_tulis <- data.frame(CT1 = hasil_rad[1], CT2 = hasil_rad[2], CT3 = hasil_rad[3])
    range_write(sheet_url, data = df_tulis, range = alamat_sel_rad, col_names = FALSE)
    
  }, error = function(e) {
    message(sprintf("  -> Error: %s", e$message))
    df_err <- data.frame(CT1 = "System Error", CT2 = "", CT3 = "")
    range_write(sheet_url, data = df_err, range = sprintf("AI%d:AK%d", i + 1, i + 1), col_names = FALSE)
  })
}

# ==========================================
# 5. PENYELESAIAN
# ==========================================
message("Pencarian Radiologi SIMARS selesai.")
remDr$close()
rs_driver_server$server$stop()