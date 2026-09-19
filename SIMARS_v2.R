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
    # A. KEMBALI KE HALAMAN PENCARIAN RADIOLOGI UNTUK PASIEN BARU
    remDr$navigate(url_pencarian_rad)
    Sys.sleep(3)
    
    # Amankan dari pop-up nyasar
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
    hasil_ekstraksi <- "Tidak ditemukan / RM Salah"
    
    # Coba cari satu per satu
    for(try_rm in rm_to_try) {
      input_rm <- remDr$findElement(using = "name", value = "norm")
      input_rm$clearElement()
      input_rm$sendKeysToElement(list(try_rm))
      
      btn_cari <- remDr$findElement(using = "name", value = "cari")
      btn_cari$clickElement()
      Sys.sleep(3) 
      
      # Cek apakah nama pasien muncul di tabel order (Kolom ke-4)
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
    
    # C. JIKA RM DITEMUKAN, CARI URL HASIL BACAAN CT SCAN ABDOMEN
    if (!is.null(rm_valid)) {
      js_get_target_url <- "
        var links = document.querySelectorAll('a[target=\"_blank\"]');
        for(var i=0; i<links.length; i++) {
          var a = links[i];
          if(a.innerHTML.includes('Bacaan')) {
            var container = a.closest('table.bordered');
            var text = container.innerText.toLowerCase();
            if(text.includes('ct scan') && text.includes('abdomen')) {
               return a.href;
            }
          }
        }
        return null;
      "
      target_url <- remDr$executeScript(js_get_target_url)
      
      if (!is.null(target_url[[1]])) {
        message("  -> CT Scan Abdomen ditemukan! Mengekstrak hasil bacaan...")
        
        remDr$navigate(target_url[[1]])
        Sys.sleep(5) 
        
        # JAVASCRIPT BARU: Mengambil Tanggal dan Teks Bacaan
        js_extract_bacaan <- "
          var tanggal = 'Tanggal Tidak Ditemukan';
          var trs = document.querySelectorAll('#table-print tr');
          
          // 1. Cari baris Tanggal Pemeriksaan
          for(var i=0; i<trs.length; i++) {
              var tds = trs[i].querySelectorAll('td');
              if(tds.length === 3 && tds[0].innerText.includes('Tanggal') && !tds[0].innerText.includes('Lahir')) {
                  tanggal = tds[2].innerText.trim();
                  break;
              }
          }
          
          // 2. Cari baris Klinis, Hasil, dan Kesan
          var bacaanTds = document.querySelectorAll('#table-print td[colspan=\"3\"]');
          var res = [];
          for(var i=0; i<bacaanTds.length; i++) {
             var txt = bacaanTds[i].innerText.trim();
             if(txt.length > 10) {
                res.push(txt);
             }
          }
          
          // 3. Gabungkan Tanggal di paling atas
          var header = '=== [TGL PEMERIKSAAN: ' + tanggal + '] ===\\n\\n';
          return header + res.join('\\n\\n====================\\n\\n');
        "
        bacaan_text <- remDr$executeScript(js_extract_bacaan)
        
        if (!is.null(bacaan_text[[1]]) && bacaan_text[[1]] != "") {
          hasil_ekstraksi <- bacaan_text[[1]]
        } else {
          hasil_ekstraksi <- "Pop-up Bacaan terbuka, tetapi teks kosong."
        }
        
      } else {
        hasil_ekstraksi <- "Order CT Scan Abdomen tidak ditemukan pada pasien ini."
      }
    }
    
    # D. TULIS HASIL KE KOLOM AI (RAW_RADIOLOGI_SIMETRIS)
    alamat_sel_rad <- sprintf("AI%d", i + 1)
    # Batasi 45000 karakter agar Google Sheets tidak error
    if(nchar(hasil_ekstraksi) > 45000) hasil_ekstraksi <- str_sub(hasil_ekstraksi, 1, 45000)
    
    range_write(sheet_url, data = data.frame(hasil_ekstraksi), range = alamat_sel_rad, col_names = FALSE)
    
  }, error = function(e) {
    message(sprintf("  -> Error: %s", e$message))
    range_write(sheet_url, data = data.frame("System Error"), range = sprintf("AI%d", i + 1), col_names = FALSE)
  })
}

# ==========================================
# 5. PENYELESAIAN
# ==========================================
message("Pencarian Radiologi SIMARS selesai.")
remDr$close()
rs_driver_server$server$stop()