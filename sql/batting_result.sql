DELETE FROM batting_result;

INSERT INTO batting_result (
  game_id,
  player_id,
  batting_order,
  appearance_order,
  positions,
  runs,
  stolen_bases,
  errors
) values
(1, 1, 1, 1, '[4]', 0, 1, 1),
(2, 1, 1, 1, '[4]', 1, 1, 0);
