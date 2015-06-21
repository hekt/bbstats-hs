DELETE FROM at_bat;

INSERT INTO at_bat (
  game_id,
  player_id,
  at_bat_number,
  inning,
  rbi,
  out_count,
  result_text,
  result_kind,
  is_risp,
  is_counts_at_bat
)
VALUES
(1, 1, 1, 1, 0, 0, '振逃', 'so', false, true),
(1, 1, 2, 3, 0, 0, '投ゴ', 'go', false, true),
(1, 1, 3, 5, 0, 0, '三振', 'so', false, true),
(1, 1, 4, 7, 0, 0, '一ゴ', 'go', false, true),
(1, 2, 1, 1, 0, 0, '二飛', 'fo', false, true),
(1, 2, 2, 3, 0, 1, '捕飛', 'fo', false, true),
(1, 2, 3, 5, 0, 1, '死球', 'hbp', false, false),
(1, 2, 4, 7, 0, 1, '遊飛', 'fo', false, true);
