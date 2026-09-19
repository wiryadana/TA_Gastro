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

# Membaca data target
data_pasien <- read_sheet(sheet_url, sheet = "MAIN")
daftar_rm <- data_pasien$RM
daftar_nama <- data_pasien$NAMA_INISIAL

# ==========================================
# FUNGSI PEMBANTU: PENCOCOKAN NAMA
# ==========================================
# Fungsi ini mencocokkan "Ni Ketut Suartini" / "NKS" dengan "NI KETUT SUARTINI" di web
is_name_match <- function(nama_excel, nama_web) {
  if(is.na(nama_excel) || is.na(nama_web)) return(FALSE)
  
  ne <- toupper(str_trim(nama_excel))
  nw <- toupper(str_trim(nama_web))
  
  # 1. Cek kecocokan parsial (Misal: "SUARTINI" ada di dalam "NI KETUT SUARTINI")
  if (grepl(ne, nw, fixed = TRUE) || grepl(nw, ne, fixed = TRUE)) return(TRUE)
  
  # 2. Cek Inisial (Misal "NKS" cocok dengan "NI KETUT SUARTINI")
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
rs_driver_server <- rsDriver(browser = "firefox", 
                             port = free_port(),
                             chromever = NULL,   
                             phantomver = NULL,  
                             iedrver = NULL,
                             check = FALSE)      # <--- TAMBAHKAN BARIS INI

remDr <- rs_driver_server$client

# ==========================================
# 3. LOGIN SIMARS (IMPROVED)
# ==========================================
message("Membuka halaman login SIMARS...")
remDr$navigate("https://rsupsanglah.com:9024/simrsm/index.php")
Sys.sleep(3)

# Memasukkan Username dan Password secara otomatis
# Menggunakan "name" karena lebih spesifik di struktur HTML SIMARS
remDr$findElement(using = "name", value = "username")$sendKeysToElement(list("52664"))
remDr$findElement(using = "name", value = "password")$sendKeysToElement(list("W1ryadana"))

# Meminta input Captcha via Console R
kode_captcha <- readline(prompt = "Lihat Captcha di browser, ketik angkanya di sini, lalu tekan ENTER: ")

# Memasukkan Captcha ke web dan langsung menekan tombol Enter secara otomatis
captcha_field <- remDr$findElement(using = "name", value = "captcha")
captcha_field$sendKeysToElement(list(kode_captcha, key = "enter"))

message("Login sedang diproses, menunggu halaman dimuat...")
Sys.sleep(5) # Jeda untuk memastikan proses verifikasi login selesai di server


# ==========================================
# 3B. BYPASS POP-UP PENGUMUMAN
# ==========================================
message("Mengecek dan menutup pop-up pengumuman jika ada...")
tryCatch({
  # Cara 1: Menutup semua popup Fancybox menggunakan JavaScript bawaan web SIMARS
  remDr$executeScript("if(typeof $.fancybox !== 'undefined') { $.fancybox.close(); }")
  Sys.sleep(2)
  
  # Cara 2: Backup - Jika pop-up tidak pakai fancybox, cari tombol yang mengandung teks "TUTUP"
  btn_tutup <- remDr$findElement(using = "xpath", value = "//*[contains(translate(text(), 'TUP', 'tup'), 'tutup')]")
  btn_tutup$clickElement()
  Sys.sleep(2)
}, error = function(e) {
  # Jika error berarti pop-up memang tidak ada, aman untuk dilanjutkan
})

# ==========================================
# 4. NAVIGASI KE HALAMAN PENCARIAN RIWAYAT PASIEN
# ==========================================
url_pencarian <- "https://rsupsanglah.com:9024/simrsm/index.php?tm=Pelayanan&glm=Pemeriksaan%20Pasien&lm=Riwayat%20Pasien&mid=1930&fid=&lk=&mo=dokter/medical_checkup/pemeriksaan.php&"
remDr$navigate(url_pencarian)
Sys.sleep(5)

# Eksekusi tutup popup sekali lagi untuk berjaga-jaga jika pengumuman muncul di halaman pencarian
tryCatch({
  remDr$executeScript("if(typeof $.fancybox !== 'undefined') { $.fancybox.close(); }")
  Sys.sleep(1)
}, error = function(e) {})


# ==========================================
# 5. LOOPING EKSTRAKSI DATA PASIEN
# ==========================================
for(i in seq_along(daftar_rm)) {
  no_rm_asli <- as.character(daftar_rm[i])
  nama_target <- as.character(daftar_nama[i])
  
  if(is.na(no_rm_asli) || str_trim(no_rm_asli) == "") next
  message(sprintf("\nMemproses Data %d: RM %s - %s", i, no_rm_asli, nama_target))
  
  tryCatch({
    # --- LOGIKA BRUTE-FORCE UNTUK RM BURAM ---
    rm_to_try <- c(no_rm_asli)
    if (grepl("^[*Xx]", str_trim(no_rm_asli))) {
      core_rm <- str_extract(str_trim(no_rm_asli), "^[*Xx]\\d{7}")
      if(!is.na(core_rm)) {
        rm_to_try <- sprintf("%d%s", 0:9, str_sub(core_rm, 2, 8))
        message("  -> Mendeteksi RM tersamar. Memulai pencocokan 0-9...")
      }
    }
    
    rm_valid <- NULL
    hasil_ekstraksi <- "Tidak ditemukan / RM Salah"
    
    # Coba satu per satu RM
    for(try_rm in rm_to_try) {
      input_rm <- remDr$findElement(using = "name", value = "pcrm")
      input_rm$clearElement()
      input_rm$sendKeysToElement(list(try_rm))
      
      btn_cari <- remDr$findElement(using = "id", value = "srch")
      btn_cari$clickElement()
      Sys.sleep(3.5) # Tunggu loading
      
      nama_web_elem <- tryCatch(remDr$findElement("css selector", ".namax"), error = function(e) NULL)
      
      if(!is.null(nama_web_elem)) {
        nama_web <- nama_web_elem$getElementText()[[1]]
        if (is_name_match(nama_target, nama_web)) {
          rm_valid <- try_rm
          message(sprintf("  -> KECOCOKAN DITEMUKAN! RM %s milik %s", rm_valid, nama_web))
          if (try_rm != no_rm_asli) {
            range_write(sheet_url, data = data.frame(rm_valid), range = sprintf("B%d", i + 1), col_names = FALSE)
          }
          break 
        }
      }
    }
    
    # --- EKSTRAKSI MULTI-KUNJUNGAN JIKA RM DITEMUKAN ---
    if (!is.null(rm_valid)) {
      
      # Injeksi JavaScript untuk mencari baris Rawat Jalan dan Rawat Inap
      js_find_rows <- "
        var rows = document.querySelectorAll('table.bordered.lb12 tbody tr:not(.judul)');
        var target_idx = [];
        for(var i=0; i<rows.length; i++) {
            var tds = rows[i].querySelectorAll('td');
            if(tds.length >= 8) {
                var kunjungan = tds[5].innerText.toLowerCase();
                var instalasi = tds[6].innerText.toLowerCase();
                if(kunjungan.includes('rawat jalan') || kunjungan.includes('rawat inap') || 
                   instalasi.includes('rawat jalan') || instalasi.includes('rawat inap')) {
                    target_idx.push(i);
                }
            }
        }
        return target_idx;
      "
      target_indices <- unlist(remDr$executeScript(js_find_rows))
      
      if (length(target_indices) > 0) {
        # Balik urutan agar dimulai dari yang paling awal/tua (paling bawah di tabel)
        target_indices <- rev(target_indices)
        
        # PENGAMAN: Batasi maksimal 15 kunjungan agar skrip tidak hang berjam-jam jika pasien punya ratusan kunjungan.
        # Anda bisa mengubah angka 15 ini sesuai kebutuhan.
        if(length(target_indices) > 50) {
          target_indices <- tail(target_indices, 50) 
        }
        
        kumpulan_hasil <- c()
        message(sprintf("  -> Menemukan %d kunjungan RJ/RI. Mulai mengekstrak dari yang terawal...", length(target_indices)))
        
        for (idx in target_indices) {
          # Klik tombol detail menggunakan JS agar stabil
          js_click <- sprintf("document.querySelectorAll('table.bordered.lb12 tbody tr:not(.judul)')[%d].querySelector('input.button-form-green').click();", idx)
          remDr$executeScript(js_click)
          
          Sys.sleep(4) # Tunggu popup CPPT terbuka
          
          js_get_table <- "
            var table = document.querySelector('tbody.pucung');
            return table ? table.innerHTML : '';
          "
          html_tbody <- remDr$executeScript(js_get_table)
          
          if (!is.null(html_tbody[[1]]) && html_tbody[[1]] != "") {
            parsed_html <- read_html(paste0("<table><tbody>", html_tbody[[1]], "</tbody></table>"))
            baris_cppt <- html_nodes(parsed_html, "tr")
            
            for (baris in baris_cppt) {
              sel <- html_nodes(baris, "td")
              if (length(sel) >= 5) {
                tanggal <- html_text(sel[1], trim = TRUE)
                kegiatan <- html_text(sel[3], trim = TRUE) 
                diagnosa <- html_text(sel[5], trim = TRUE)
                
                # Saring hanya jika kolom kegiatan tidak kosong (minimal ada teksnya)
                if (nchar(kegiatan) > 5) {
                  entry <- sprintf("=== [TGL: %s] ===\nDIAGNOSA: %s\nASSESSMENT:\n%s\n", 
                                   tanggal, diagnosa, kegiatan)
                  kumpulan_hasil <- c(kumpulan_hasil, entry)
                }
              }
            }
          }
          
          # Tutup pop-up sebelum lanjut ke kunjungan berikutnya
          tryCatch({
            remDr$executeScript("if(typeof $.fancybox !== 'undefined') { $.fancybox.close(); }")
            Sys.sleep(1.5)
          }, error = function(e) {})
        } # Akhir dari loop per kunjungan
        
        # Menggabungkan seluruh hasil menjadi 1 teks panjang
        if (length(kumpulan_hasil) > 0) {
          # Unique digunakan untuk membuang teks yang terduplikasi secara identik oleh sistem rumah sakit
          kumpulan_hasil <- unique(kumpulan_hasil)
          final_text <- paste(kumpulan_hasil, collapse = "\n\n")
          
          # Batasi 45,000 karakter agar tidak error saat dikirim ke Google Sheets (Batas sel Google adalah 50.000)
          if(nchar(final_text) > 45000) {
            final_text <- str_sub(final_text, 1, 45000)
          }
          hasil_ekstraksi <- final_text
        } else {
          hasil_ekstraksi <- "Riwayat RJ/RI ditemukan, tapi catatan CPPT kosong."
        }
        
      } else {
        hasil_ekstraksi <- "Pasien tidak memiliki riwayat Rawat Jalan / Rawat Inap."
      }
    } else {
      hasil_ekstraksi <- "Gagal mencocokkan RM dan Nama (Data tidak sesuai)"
    }
    
    # Tulis hasil ekstraksi ke kolom AG (RAW_CPPT_SIMARS)
    alamat_sel_cppt <- sprintf("AG%d", i + 1)
    range_write(sheet_url, data = data.frame(hasil_ekstraksi), range = alamat_sel_cppt, col_names = FALSE)
    
  }, error = function(e) {
    message(sprintf("  -> Error: %s", e$message))
    range_write(sheet_url, data = data.frame("System Error"), range = sprintf("AG%d", i + 1), col_names = FALSE)
  })
}

# ==========================================
# 6. PENYELESAIAN
# ==========================================
message("Pencarian SIMARS selesai.")
remDr$close()
rs_driver_server$server$stop()