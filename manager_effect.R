## パッケージ準備
library("estimatr")
library("dplyr")
library("readr")
library("broom")
library("modelsummary")
library("tidyr")
library("tools")

# 分析処理を関数化
analyze_manager_data <- function(file_path) { 
  
  if (!file.exists(file_path)) return(NULL)
  
# --- データ読み込み ---
  df <- read_csv(file_path, show_col_types = FALSE)
  colnames(df) <- gsub("\\s+", "", colnames(df))

  # 出力ファイル名の共通部分を定義
  base_name <- file_path_sans_ext(basename(file_path)) 
  output_suffix <- gsub("パネルデータ_players_", "", base_name)
  
  # --- モデル構築と推定 ---
  # モデル式の定義
  # 戦力(改良前)
  predictors_str_1 <- 
    paste("打率 + 本塁打 + 防御率")
  
  # 戦力(改良後)
  predictors_str_2 <- 
    paste("wOBA + wSB + FIP + DER")
  
  predictors_str_3 <- 
    paste("wOBA + wSB + FIP + DER + リーグ")
  
  predictors_str_4 <- 
    paste("wOBA + wSB + FIP + DER + Aクラス")

  predictors_str_5 <- 
    paste("wOBA + wSB + FIP + DER + リーグ + Aクラス")
  
  # 監督ダミーの処理
  all_dummy_vars <- colnames(df)[13:ncol(df)]
  all_manager_dummies_str <- paste(sprintf("`%s`", all_dummy_vars), collapse = " + ")
  
  # 最終的なモデル式
  full_formula_1 <- as.formula(
    paste("勝率 ~", predictors_str_1, "+", all_manager_dummies_str))

  full_formula_2 <- as.formula(
    paste("勝率 ~", predictors_str_2, "+", all_manager_dummies_str))

  full_formula_3 <- as.formula(
    paste("勝率 ~", predictors_str_3, "+", all_manager_dummies_str))

  full_formula_4 <- as.formula(
    paste("勝率 ~", predictors_str_4, "+", all_manager_dummies_str))

  full_formula_5 <- as.formula(
    paste("勝率 ~", predictors_str_5, "+", all_manager_dummies_str))

  # モデルの推定
  result_0 <- lm_robust(full_formula_1, data = df, fixed_effects = year, clusters = team, se_type = "stata")
  result_1 <- lm_robust(full_formula_2, data = df, clusters = team, se_type = "stata")
  result_2 <- lm_robust(full_formula_2, data = df, fixed_effects = year, clusters = team, se_type = "stata")
  result_3 <- lm_robust(full_formula_3, data = df, fixed_effects = year, clusters = team, se_type = "stata")
  result_4 <- lm_robust(full_formula_4, data = df, fixed_effects = year, clusters = team, se_type = "stata")
  result_5 <- lm_robust(full_formula_5, data = df, fixed_effects = year, clusters = team, se_type = "stata")
    
  #AICが最小のモデルを指定
  model_list <- list(
    result_1 = result_1,
    result_2 = result_2,
    result_3 = result_3,
    result_4 = result_4,
    result_5 = result_5
  )
    
  adj_r2_values <- sapply(model_list, function(m) summary(m)$adj.r.squared)
  best_model_name <- names(which.max(adj_r2_values))
  best_model <- model_list[[best_model_name]]
  
  cat(sprintf("📊 最良のモデル: %s \n", best_model_name))
  
  ## 残差プロット
  model_plot <- lm(full_formula_5　, data = df)
  png("監督効果_残差.png", width = 1200, height = 1200, res = 150)
  
  # 2. 画面を2x2に分割して表示
  par(mfrow = c(2, 2))
  plot(model_plot)
  dev.off()
  par(mfrow = c(1, 1)) # 設定を戻す
 
  # --- 結果の整形 ---
  tidy_result <- tidy(best_model)
  manager_coefs_df <- tidy_result %>%
    filter(term %in% all_dummy_vars) %>%
    dplyr::select(Dummy = term, Coef_Dummy = estimate) 

  all_dummies_df <- data.frame(Dummy = all_dummy_vars)

  result_df <- all_dummies_df %>%
    left_join(manager_coefs_df, by = "Dummy") %>%
    mutate(Coef_Dummy = ifelse(is.na(Coef_Dummy), 0, Coef_Dummy)) %>%
    rename(manager = Dummy, manager_effect = Coef_Dummy)
  
  # 監督効果の相対値を作成
  # 1. 全監督の中から効果が最小の値を特定
  min_effect <- min(result_df$manager_effect)
  
  # 2. 最小値だった監督の名前を表示
  min_effect_manager <- result_df %>%
    filter(manager_effect == min_effect) %>%
    pull(manager)
  cat(sprintf("📊 新しい基準監督（効果が最小）: %s (元の係数値: %.4f)\n", min_effect_manager[1], min_effect))
  
  # 3. 全員の係数値から最小値を引いて、新しい相対的な係数を計算
  result_df_relative <- result_df %>%
    mutate(
      manager_effect = (manager_effect - min_effect) * 100
    )

  # --- 就任年数の計算 ---
  manager_years <- df %>%
    dplyr::select(all_of(all_dummy_vars)) %>%
    summarise(across(everything(), sum)) %>% # 各監督ダミー列の合計 (=就任年数)
    pivot_longer(cols = everything(), names_to = "manager", values_to = "year_count")
  
  # 相対効果に就任年数を結合 
  result_df_relative <- result_df_relative %>%
    left_join(manager_years, by = "manager")
  
  # --- ランク付け（全監督） ---
  manager_dummies <- df %>%
    dplyr::select(all_of(all_dummy_vars)) %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(cols = everything(), names_to = "manager", values_to = "count") %>%
    pull(manager)
  
  result_ranked <- result_df_relative %>%
    filter(manager %in% manager_dummies) %>%
    # 相対値でランク付け
    arrange(desc(manager_effect)) %>%
    # CSV出力用に列を選択・リネーム
    dplyr::select(監督 = manager, 監督効果 = manager_effect) 
  
  # --- ランク付け（2年以上） ---
  # フィルタリングのためにyear_countが内部的に必要
  result_ranked_2yrs_plus <- result_df_relative %>% 
    filter(year_count >= 2) %>% 
    dplyr::select(監督 = manager, 監督効果 = manager_effect) %>% 
    arrange(desc(監督効果)) # 2年以上の監督でランク付け
  
  # --- 出力ファイル名の生成 ---
  base_name <- file_path_sans_ext(basename(file_path)) 
  csv_output_name <- paste0("監督効果_", output_suffix, ".csv")
  csv_output_name_2yrs_plus <- paste0("監督効果_2年以上_", output_suffix, ".csv") # 新しいファイル名

  # --- 結果出力 ---
  model_list <- list(
    "(a)" = result_1,
    "(b)" = result_2,
    "(c)" = result_3,
    "(d)" = result_4,
    "(e)" = result_5
    )
  
  gof_map_custom <- tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "観測数", 0,
    "adj.r.squared", "調整済み R2", 4
    )

  map_vars_1 <- c(
    "打率", 
    "本塁打", 
    "防御率"
  )
  
  map_vars_2 <- c(
    "wOBA", 
    "wSB",
    "FIP", 
    "DER",
    "リーグ",
    "Aクラス"
    )
  
  rows_fe_1 <- tribble(
    ~term, ~"改良前",
    "年効果", "YES"
  )
  
  rows_fe_2 <- tribble(
    ~term, ~"(a)", ~"(b)", ~"(c)", ~"(d)", ~"(e)",
    "年効果", "NO", "YES", "YES", "YES", "YES"
  )
  
  msummary(
    list("改良前" = result_0),
    coef_map = map_vars_1,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_map = gof_map_custom,
    add_rows = rows_fe_1,
    output = paste0("基本戦力_元_", output_suffix, ".png")
  )
  
  msummary(
    model_list,
    coef_map = map_vars_2,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_map = gof_map_custom,
    add_rows = rows_fe_2,
    output = paste0("基本戦力_", output_suffix, ".png")
  )
  cat("✅ 戦力変数の結果を保存しました。\n")
  
  # 全監督の結果を出力
  write_excel_csv(result_ranked, csv_output_name)
  cat("✅ 全監督効果の結果を保存しました。（年数なし）\n")
  
  # 2年以上の監督の結果を出力
  write_excel_csv(result_ranked_2yrs_plus, csv_output_name_2yrs_plus)
  cat("✅ 2年以上の監督効果の結果を '%s' に保存しました。（年数なし）\n")
  
  cat(sprintf("======= 分析完了: %s =======\n\n", file_path))
}

# 分析を実行したいファイルリスト
file_list <- c(
  "パネルデータ_players_coach1.csv",
  "パネルデータ_players_coach2.csv" 
)

# リスト内の各ファイルに対して分析を実行
lapply(file_list, analyze_manager_data)

cat("全ての処理が完了しました。\n")
