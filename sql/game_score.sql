DELETE FROM game_score;

INSERT INTO game_score (
  game_date,
  game_result,
  ground,
  attack_turn,
  runs,
  total_runs,
  total_hits,
  total_errors,
  opponent_name,
  opponent_runs,
  opponent_total_runs,
  opponent_total_hits,
  opponent_total_errors
) values (
  '2015-06-14',
  1,
  '雁来健康公園野球場B',
  0,
  '[2,0,0,1,3,0,0]',
  6,
  4,
  6,
  '北海道日野自動車野球部',
  '[0,5,0,5,1,0]',
  11,
  10,
  2
), (
'2015-05-24',
  1,
  '太平公園',
  0,
  '[0,0,0,0,1,1,0,0,0]',
  2,
  3,
  7,
  'アクエリアス',
  '[0,2,4,1,0,0,0,2]',
  9,
  7,
  0
);
