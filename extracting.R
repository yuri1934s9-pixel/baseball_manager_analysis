# 必要なライブラリを読み込み
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# --------------------------------------------------
# ▼▼▼ 設定項目：ここを自由に書き換えてください ▼▼▼
# --------------------------------------------------
# 抽出したい指標をリスト形式で指定します。
# 片方だけで良ければ、不要な行をコメントアウト（#）するか削除すればOKです。
metrics_list <- list(
  batting  = c("打席"),  # 例: c("打率", "安打", "本塁") と複数指定も可能
  pitching = c()           # 例: c("防率", "勝利")
)

# 読み込むファイルの年範囲を指定
start_year <- 1990
end_year   <- 2025

# チームの表示順序（CSV出力時の並び順）
team_order <- c(
  "巨人", "阪神", "中日", "ヤクルト", "広島", "横浜", 
  "西武", "日本ハム", "ロッテ", "ソフトバンク", "オリックス", "楽天", "近鉄"
)

# 出力先フォルダの作成
output_dir <- "sub/"
if (!dir.exists(output_dir)) dir.create(output_dir, showWarnings = FALSE)

# --------------------------------------------------
# ▼▼▼ 処理部分（自動判別ループ） ▼▼▼
# --------------------------------------------------

for (type in names(metrics_list)) {
  
  target_metrics <- metrics_list[[type]]
  # 空の文字列を除外するガード
  target_metrics <- target_metrics[target_metrics != ""]
  
  if (length(target_metrics) == 0) next
  
  input_folder   <- paste0("players_text_", type, "/")
  prefix         <- if (type == "batting") "野手" else "投手"
  
  for (target_metric in target_metrics) {
    # target_metricが空でないことを念のため再確認
    if (is.na(target_metric) || target_metric == "") next
    
    print(paste("---", prefix, "データの抽出を開始:", target_metric, "---"))
    
    file_names <- paste0(input_folder, "baseball_", start_year:end_year, ".txt")
    all_metric_data <- list()
    
    for (file_name in file_names) {
      if (!file.exists(file_name)) next
      
      content <- read_file(file_name, locale = locale(encoding = "UTF-8"))
      tables <- str_split(content, "--- \\[テーブル\\s+\\d+\\s+\\] ---")[[1]]
      
      for (table in tables) {
        if (str_trim(table) == "") next
        lines <- str_split(str_trim(table), "\n")[[1]]
        header <- lines[1]
        
        # 指標名が空でない場合のみstr_detectを実行
        if (str_detect(header, fixed(target_metric))) {
          header_columns <- str_split(str_trim(header), "\\s+")[[1]]
          metric_index <- which(header_columns == target_metric)
          if (length(metric_index) == 0) next
          
          for (i in 2:length(lines)) {
            line <- lines[i]
            if (str_trim(line) == "") next
            data <- str_split(str_trim(line), "\\s+")[[1]]
            metric_column_index <- metric_index + 1
            
            if (length(data) >= metric_column_index) {
              temp_df <- data.frame(
                year = as.integer(data[1]),
                team = data[2],
                value = data[metric_column_index],
                stringsAsFactors = FALSE
              )
              names(temp_df)[3] <- target_metric
              all_data_exists <- TRUE
              all_metric_data <- append(all_metric_data, list(temp_df))
            }
          }
        }
      }
    }
    
    if (length(all_metric_data) > 0) {
      df_metric_long <- do.call(rbind, all_metric_data)
      df_metric_wide <- df_metric_long %>%
        distinct(year, team, .keep_all = TRUE) %>%
        pivot_wider(names_from = year, values_from = !!target_metric) %>%
        mutate(team = factor(team, levels = team_order)) %>%
        arrange(team)
      
      output_filepath <- file.path(output_dir, paste0(prefix, "_", target_metric, ".csv"))
      write_excel_csv(df_metric_wide, output_filepath)
      print(paste("完了:", output_filepath))
    }
  }
}

print("----- すべての抽出作業が完了しました -----")
