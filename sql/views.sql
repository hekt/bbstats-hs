CREATE MATERIALIZED VIEW view_batting_stats AS SELECT
  p.id AS player_id,
  p.player_name AS player_name,
  p.uniform_number AS uniform_number,
  p.temp_uniform_number AS temp_uniform_number,
  coalesce(a.pa, 0) AS plate_appearances,
  coalesce(a.ab, 0) AS at_bats,
  coalesce(a.ab_risp, 0) AS at_bats_risp,
  coalesce(a.rbi, 0) AS rbi,
  coalesce(a.h, 0) AS hits,
  coalesce(a.h_risp, 0) AS hits_risp,
  coalesce(a.bb, 0) AS walks,
  coalesce(a.hr, 0) AS home_runs,
  coalesce(b.run, 0) AS runs,
  coalesce(b.sb, 0) AS stolen_bases
FROM
  mst_player p
  RIGHT JOIN (
    SELECT
      player_id,
      count(*) AS pa,
      count(CASE WHEN is_counts_at_bat THEN 1 END) AS ab,
      sum(rbi) AS rbi,
      count(CASE result_kind WHEN 'h' THEN 1 WHEN 'dbl' THEN 1 WHEN 'tpl' THEN 1 WHEN 'hr' THEN 1 END) AS h,
      count(CASE result_kind WHEN 'bb' THEN 1 WHEN 'ibb' THEN 1 WHEN 'hbp' THEN 1 END) AS bb,
      count(CASE result_kind WHEN 'hr' THEN 1 END) AS hr,
      count(CASE WHEN is_counts_at_bat AND is_risp THEN 1 END) as ab_risp,
      count(CASE WHEN is_risp THEN CASE result_kind WHEN 'h' THEN 1 WHEN 'dbl' THEN 1 WHEN 'tpl' THEN 1 WHEN 'hr' THEN 1 END END) AS h_risp
    FROM tbl_at_bat GROUP BY player_id
  ) AS a ON p.id = a.player_id
  LEFT JOIN (
    SELECT
      player_id,
      sum(runs) AS run,
      sum(stolen_bases) AS sb,
      sum(errors) AS e
    FROM tbl_batting_result GROUP BY player_id
  ) AS b ON p.id = b.player_id
;
CREATE UNIQUE INDEX batting_stats_player_id
  ON view_batting_stats (player_id);

CREATE MATERIALIZED VIEW view_pitching_stats AS SELECT
  p.id AS player_id,
  p.player_name AS player_name,
  p.uniform_number AS uniform_number,
  p.temp_uniform_number AS temp_uniform_number,
  coalesce(r.games, 0) AS games,
  coalesce(r.outs, 0) AS outs,
  coalesce(r.batters_faced, 0) AS batters_faced,
  coalesce(r.runs, 0) AS runs,
  coalesce(r.earned_runs, 0) AS earned_runs,
  coalesce(r.strike_outs, 0) AS strike_outs,
  coalesce(r.walks, 0) AS walks,
  coalesce(r.hits, 0) AS hits,
  coalesce(r.home_runs, 0) AS home_runs,
  coalesce(r.errors, 0) AS errors,
  coalesce(r.wins, 0) AS wins,
  coalesce(r.loses, 0) AS loses,
  coalesce(r.holds, 0) AS holds,
  coalesce(r.saves, 0) AS saves
FROM
  mst_player p
  RIGHT JOIN (
    SELECT
      player_id,
      count(player_id) AS games,
      sum(outs) AS outs,
      sum(batters_faced) AS batters_faced,
      sum(runs) AS runs,
      sum(earned_runs) AS earned_runs,
      sum(strike_outs) AS strike_outs,
      sum(walks) AS walks,
      sum(hits) AS hits,
      sum(home_runs) AS home_runs,
      sum(errors) AS errors,
      count(CASE decision WHEN 0 THEN 1 END) AS wins,
      count(CASE decision WHEN 1 THEN 1 END) AS loses,
      count(CASE decision WHEN 2 THEN 1 END) AS holds,
      count(CASE decision WHEN 3 THEN 1 END) AS saves
    FROM tbl_pitching_result GROUP BY player_id
  ) AS r ON p.id = r.player_id
;
CREATE UNIQUE INDEX pitching_stats_player_id
  ON view_pitching_stats (player_id);
