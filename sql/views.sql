CREATE MATERIALIZED VIEW view_batting_stats AS
SELECT
  coalesce(p.id, 0) AS player_id,
  coalesce(a.pa, 0) AS pa,
  coalesce(a.ab, 0) AS ab,
  coalesce(a.ab_risp, 0) AS ab_risp,
  coalesce(a.rbi, 0) AS rbi,
  coalesce(a.h, 0) AS h,
  coalesce(a.h_risp, 0) AS h_risp,
  coalesce(a.bb, 0) AS bb,
  coalesce(a.hr, 0) AS hr,
  coalesce(b.run, 0) AS run,
  coalesce(b.sb, 0) AS sb
FROM
  mst_player p
  LEFT JOIN (
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

CREATE MATERIALIZED VIEW view_pitching_stats AS
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
FROM tbl_pitching_result GROUP BY player_id ORDER BY player_id;
