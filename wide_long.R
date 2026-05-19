# パッケージ読み込み
library("tidyverse")
library("readr") 

# =============================================================================
# パスの設定 
# =============================================================================
# 各フォルダのパスを定義
path_players <- "~/Desktop/baseball/players" 
path_coach1    <- "~/Desktop/baseball/coach1"
path_coach2    <- "~/Desktop/baseball/coach2"    

# =============================================================================
# 汎用データ変換・結合関数
# =============================================================================
process_and_combine_data <- function(data_path) {
  
  cat(paste0("--- Processing Directory: ", basename(data_path), " ---\n"))
  
  file_list <- list.files(path = data_path, pattern = "*.csv", full.names = TRUE)
  
  if (length(file_list) == 0) {
    cat(paste0("Warning: No CSV files found in ", basename(data_path), ".\n"))
    return(NULL)
  }
  
  value_names <- file_list %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    str_extract("^[^_]+")
  
  data_list_long <- map2(file_list, value_names, ~ {
    read_csv(.x, show_col_types = FALSE) %>%
      pivot_longer(
        cols = -team,
        names_to = "year",
        values_to = .y
      ) %>%
      mutate(
        year = as.numeric(year)
      )
  })
  
  if (length(data_list_long) > 0) {
    # full_joinでフォルダ内のデータをすべて結合
    final_panel <- data_list_long %>%
      reduce(full_join, by = c("team", "year"))
  } else {
    final_panel <- tibble(team = character(), year = numeric())
  }
  
  cat(paste0("Finished processing ", basename(data_path), ". Rows created: ", nrow(final_panel), "\n"))
  return(final_panel)
}

# =============================================================================
# メイン処理
# =============================================================================
panel_players_combined <- process_and_combine_data(path_players)

# --- 処理1: coach1 と players の結合 ---
cat("\n--- Combining coach1 and players data ---\n")
panel_coach1_combined <- process_and_combine_data(path_coach1)
final_data_c1 <- inner_join(panel_players_combined, panel_coach1_combined, by = c("team", "year")) %>%
  arrange(team, year) 

# --- 処理2: coach2 と players の結合 ---
cat("\n--- Combining coach2 and players data ---\n")
panel_coach2_combined <- process_and_combine_data(path_coach2)
final_data_c2 <- inner_join(panel_players_combined, panel_coach2_combined, by = c("team", "year")) %>%
  arrange(team, year)

# =============================================================================
# NAを含む行を削除
# =============================================================================
cat("\n--- Removing rows with NA values ---\n")

# --- coach1データからNAを含む行を削除 ---
final_data_c1_cleaned <- final_data_c1 %>%
  na.omit()
cat("Rows in coach1 data before cleaning:", nrow(final_data_c1), 
    "-> after cleaning:", nrow(final_data_c1_cleaned), "\n")

# --- coach2データからNAを含む行を削除 ---
final_data_c2_cleaned <- final_data_c2 %>%
  na.omit()
cat("Rows in coach2 data before cleaning:", nrow(final_data_c2), 
    "-> after cleaning:", nrow(final_data_c2_cleaned), "\n")


# =============================================================================
# 出力
# =============================================================================
cat("\n--- Writing final CSV files ---\n")

# 出力先指定
output_dir <- "監督効果"

# データを書き出す
write_excel_csv(final_data_c1_cleaned, file = file.path(output_dir, "パネルデータ_players_coach1.csv"))
write_excel_csv(final_data_c2_cleaned, file = file.path(output_dir, "パネルデータ_players_coach2.csv"))

cat("パネルデータの出力完了.\n")
