## パッケージ準備
library(rvest)
library(stringr)

# フォルダパスの設定
html_input_dir <- "players_html"
text_output_dir_batting <- "players_text_batting"
text_output_dir_pitching <- "players_text_pitching"

# フォルダ作成
if (!dir.exists(html_input_dir)) dir.create(html_input_dir)
if (!dir.exists(text_output_dir_batting)) dir.create(text_output_dir_batting)
if (!dir.exists(text_output_dir_pitching)) dir.create(text_output_dir_pitching)

# 球団名対応表
team_class_map <- c(
  "sschiroshima" = "広島", "ssckyojin" = "巨人", "sscdena" = "横浜", "sscyoushou" = "横浜", 
  "ssctaiyo" = "横浜", "sscyokohama" = "横浜", "sschanshin" = "阪神", "sscosaka" = "阪神", 
  "sscyakult" = "ヤクルト", "ssckokutetsu" = "ヤクルト", "sscsankei" = "ヤクルト", 
  "sscatoms" = "ヤクルト", "sscchunichi" = "中日", "sscnagoya" = "中日", "sscnipponham" = "日本ハム", 
  "sscnittaku" = "日本ハム", "ssctoei" = "日本ハム", "ssctokyu" = "日本ハム", 
  "sscsoftbank" = "ソフトバンク", "sscnankai" = "ソフトバンク", "sscdaiei" = "ソフトバンク", 
  "ssclotte" = "ロッテ", "sscmainichi" = "ロッテ", "sscdaimai" = "ロッテ", "ssctokyo" = "ロッテ",
  "sscseibu" = "西武", "sscnishitetsu" = "西武", "ssctaiheiyo" = "西武", "ssccrown" = "西武",
  "sscrakuten" = "楽天", "sscorix" = "オリックス", "sschankyu" = "オリックス",
  "ssctakahashi" = "高橋", "sscnishinihon" = "西日本",
  "sscdaie" = "大映", "sscshouchiku" = "松竹", "ssckintetsu" = "近鉄"
)

# --- Step 1: ダウンロード処理 ---
print("--- Step 1: HTMLファイルのダウンロードを開始します ---")
for (year in 2025:1950) {
  url <- paste0("https://2689web.com/", year, ".html")
  file_path_html <- file.path(html_input_dir, paste0("baseball_", year, ".html"))
  
  if (!file.exists(file_path_html)) {
    print(paste("ダウンロード中:", year, "年"))
    tryCatch({
      webpage <- read_html(url, encoding = "Shift_JIS")
      con <- file(file_path_html, "w", encoding = "UTF-8")
      writeLines(as.character(webpage), con)
      close(con)
      Sys.sleep(1) # サーバー負荷軽減のため1秒待機
    }, error = function(e) {
      print(paste("失敗:", year, "年 -", e$message))
    })
  }
}

# CSSもついでに落とす（必要な場合）
css_file <- "b2.css"
css_dest <- file.path(html_input_dir, css_file)
if (!file.exists(css_dest)) {
  try(download.file(paste0("https://2689web.com/", css_file), destfile = css_dest, mode = "wb"))
}

# --- Step 2: データ変換処理 ---
print("--- Step 2: テキスト抽出を開始します ---")
html_files <- list.files(html_input_dir, pattern = "\\.html$", full.names = TRUE)
html_files <- sort(html_files, decreasing = TRUE)

for (html_file in html_files) {
  year_str <- sub(".html", "", sub("baseball_", "", basename(html_file)))
  year <- as.numeric(year_str)
  if (is.na(year)) next
  
  output_file_batting <- file.path(text_output_dir_batting, paste0("baseball_", year_str, ".txt"))
  output_file_pitching <- file.path(text_output_dir_pitching, paste0("baseball_", year_str, ".txt"))
  
  # 初期化（上書き）
  if (file.exists(output_file_batting)) file.remove(output_file_batting)
  if (file.exists(output_file_pitching)) file.remove(output_file_pitching)
  
  # 年度別の対象テーブル設定
  target_tables_batting <- if (year >= 1973 && year <= 1982) c(3, 14) else c(3, 11)
  target_tables_pitching <- if (year >= 1973 && year <= 1982) c(2, 4, 13, 15) else c(2, 4, 10, 12)
  
  webpage <- tryCatch(read_html(html_file), error = function(e) return(NULL))
  if (is.null(webpage)) next
  
  tables <- webpage |> html_elements("table")
  processed_table_content <- list()
  
  if (length(tables) > 0) {
    for (i in 1:length(tables)) {
      current_table_text <- tables[[i]] %>% html_text(trim = TRUE)
      if (current_table_text %in% processed_table_content) next
      
      output_path <- if (i %in% target_tables_batting) {
        output_file_batting
      } else if (i %in% target_tables_pitching) {
        output_file_pitching
      } else {
        NULL
      }
      
      if (is.null(output_path)) next
      processed_table_content <- c(processed_table_content, current_table_text)
      
      cat(paste("\n--- [テーブル ", i, "] ---\n"), file = output_path, append = TRUE)
      rows <- tables[[i]] |> html_elements("tr")
      for (row in rows) {
        cells_text <- row |> html_elements("td, th") |> html_text(trim = TRUE)
        team_cell <- row |> html_element("td[class^='ssc']")
        
        if (!is.na(team_cell)) {
          class_name <- team_cell |> html_attr("class")
          matched_base_class <- names(team_class_map)[str_starts(class_name, names(team_class_map))]
          team_name <- if (length(matched_base_class) > 0) {
            best_match <- matched_base_class[which.max(nchar(matched_base_class))]
            team_class_map[best_match]
          } else {
            paste0("[不明:", class_name, "]") 
          }
          cells_text <- c(team_name, cells_text)
        }
        
        line_of_text <- paste(cells_text[cells_text != ""], collapse = "\t")
        if (line_of_text != "") {
          cat(paste(year_str, line_of_text, sep = "\t"), "\n", file = output_path, append = TRUE)
        }
      }
    }
  }
  print(paste("処理完了:", year, "年"))
}

print("----- 全行程が完了しました！ -----")
