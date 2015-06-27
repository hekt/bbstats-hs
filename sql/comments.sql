COMMENT ON TABLE player IS 'プレイヤー情報';
COMMENT ON COLUMN player.id                  IS 'プレイヤーID';
COMMENT ON COLUMN player.player_name         IS '登録名';
COMMENT ON COLUMN player.uniform_number      IS '背番号 0から始まることもあるため文字列 助っ人は null';
COMMENT ON COLUMN player.temp_uniform_number IS '助っ人の背番号 一意性は気にしない';
COMMENT ON COLUMN player.created_at          IS '作成日時';
COMMENT ON COLUMN player.updated_at 	     IS '更新日時';

COMMENT ON TABLE game_score IS 'ゲームスコア';
COMMENT ON COLUMN game_score.id                IS 'ゲームID';
COMMENT ON COLUMN game_score.game_date 		   IS '試合日';
COMMENT ON COLUMN game_score.game_number 	   IS 'その日何試合目か';
COMMENT ON COLUMN game_score.game_result 	   IS '試合結果 0=win, 1=lose, 3=draw';
COMMENT ON COLUMN game_score.ground 		   IS 'グラウンド';
COMMENT ON COLUMN game_score.attack_turn 	   IS '先攻か後攻か 0=top, 1=bottom';
COMMENT ON COLUMN game_score.runs 		       IS 'イニング毎の得点の配列を表す文字列';
COMMENT ON COLUMN game_score.total_runs 	   IS '得点数';
COMMENT ON COLUMN game_score.total_hits 	   IS '安打数';
COMMENT ON COLUMN game_score.total_errors      IS '失策数';
COMMENT ON COLUMN game_score.opponent_name 	   IS '相手チーム名';
COMMENT ON COLUMN game_score.opponent_runs 	   IS '相手チームの runs';
COMMENT ON COLUMN game_score.opponent_total_runs   IS '相手チームの total_runs';
COMMENT ON COLUMN game_score.opponent_total_hits   IS '相手チームの total_hits';
COMMENT ON COLUMN game_score.opponent_total_errors IS '相手チームの total_errors';
COMMENT ON COLUMN game_score.created_at 	   IS '作成日時';
COMMENT ON COLUMN game_score.updated_at 	   IS '更新日時';

COMMENT ON TABLE at_bat IS '打席結果';
COMMENT ON COLUMN at_bat.id              IS '打席結果ID';
COMMENT ON COLUMN at_bat.game_id 	IS 'game_score.id';
COMMENT ON COLUMN at_bat.player_id 	IS 'player.id';
COMMENT ON COLUMN at_bat.at_bat_number 	IS '何打席目か';
COMMENT ON COLUMN at_bat.inning 		IS '打席の回ってきたイニング';
COMMENT ON COLUMN at_bat.rbi 		IS 'その打席で記録した打点';
COMMENT ON COLUMN at_bat.out_count 	IS '打撃時点でのアウトカウント 打席に入った時点ではない';
COMMENT ON COLUMN at_bat.result_text 	IS '結果 日本語';
COMMENT ON COLUMN at_bat.result_kind     IS '結果を表すアルファベット略語';
COMMENT ON COLUMN at_bat.is_risp         IS '打撃時点でランナーが得点圏にいたか';
COMMENT ON COLUMN at_bat.is_counts_at_bat IS '打数にカウントするか';
COMMENT ON COLUMN at_bat.created_at 	IS '作成日時';
COMMENT ON COLUMN at_bat.updated_at 	IS '更新日時';

COMMENT ON TABLE batting_result IS '打撃結果';
COMMENT ON COLUMN batting_result.id               IS '打撃結果ID';
COMMENT ON COLUMN batting_result.game_id 	  IS 'game_score.id';
COMMENT ON COLUMN batting_result.player_id 	  IS 'player.id';
COMMENT ON COLUMN batting_result.batting_order 	  IS '打順';
COMMENT ON COLUMN batting_result.appearance_order IS 'その打順に入った何人目か';
COMMENT ON COLUMN batting_result.positions 	  IS 'ポジションを表す数値の配列';
COMMENT ON COLUMN batting_result.runs 		  IS '得点';
COMMENT ON COLUMN batting_result.stolen_bases 	  IS '盗塁';
COMMENT ON COLUMN batting_result.errors 	  IS '失策';
COMMENT ON COLUMN batting_result.created_at 	  IS '作成日時';
COMMENT ON COLUMN batting_result.updated_at 	  IS '更新日時';

COMMENT ON TABLE batting_stats IS '打撃統計';
COMMENT ON COLUMN batting_stats.player_id IS 'player.id';
COMMENT ON COLUMN batting_stats.games IS '試合数';
COMMENT ON COLUMN batting_stats.plate_appearances IS '打席';
COMMENT ON COLUMN batting_stats.at_bats IS '打数';
COMMENT ON COLUMN batting_stats.rbi IS '打点';
COMMENT ON COLUMN batting_stats.hits IS '安打';
COMMENT ON COLUMN batting_stats.walks IS '四死球';
COMMENT ON COLUMN batting_stats.home_runs IS '本塁打';
COMMENT ON COLUMN batting_stats.runs IS '得点';
COMMENT ON COLUMN batting_stats.risp_at_bats IS '得点圏打数';
COMMENT ON COLUMN batting_stats.risp_hits IS '得点圏安打';
COMMENT ON COLUMN batting_stats.stolen_bases IS '盗塁';
COMMENT ON COLUMN batting_stats.errors IS '失策';
COMMENT ON COLUMN batting_stats.created_at IS '作成日時';
COMMENT ON COLUMN batting_stats.updated_at IS '更新日時';

COMMENT ON TABLE pitching_result IS '投球結果';
COMMENT ON COLUMN pitching_result.id               IS '投球結果ID';
COMMENT ON COLUMN pitching_result.game_id          IS 'game_score.id';
COMMENT ON COLUMN pitching_result.player_id        IS 'player.id';
COMMENT ON COLUMN pitching_result.appearance_order IS '投球順';
COMMENT ON COLUMN pitching_result.outs             IS '奪アウト';
COMMENT ON COLUMN pitching_result.batters_faced    IS '対戦打者';
COMMENT ON COLUMN pitching_result.runs             IS '失点';
COMMENT ON COLUMN pitching_result.earned_runs      IS '自責点';
COMMENT ON COLUMN pitching_result.strike_outs      IS '奪三振';
COMMENT ON COLUMN pitching_result.walks            IS '与四死球';
COMMENT ON COLUMN pitching_result.hits             IS '被安打';
COMMENT ON COLUMN pitching_result.homeruns         IS '被本塁打';
COMMENT ON COLUMN pitching_result.errors           IS '被失策 投球中に味方がエラーした数';
COMMENT ON COLUMN pitching_result.decision         IS '責任 WIN|LOSE|HOLD|SAVE';
COMMENT ON COLUMN pitching_result.created_at       IS '作成日時';
COMMENT ON COLUMN pitching_result.updated_at       IS '更新日時';

COMMENT ON TABLE pitching_stats IS '投球統計';
COMMENT ON COLUMN pitching_stats.games IS '試合数';
COMMENT ON COLUMN pitching_stats.outs IS '奪アウト';
COMMENT ON COLUMN pitching_stats.batters_faced IS '対戦打者';
COMMENT ON COLUMN pitching_stats.runs IS '失点';
COMMENT ON COLUMN pitching_stats.earned_runs IS '自責点';
COMMENT ON COLUMN pitching_stats.strike_outs IS '奪三振';
COMMENT ON COLUMN pitching_stats.walks IS '与四死球';
COMMENT ON COLUMN pitching_stats.hits IS '被安打';
COMMENT ON COLUMN pitching_stats.home_runs IS '被本塁打';
COMMENT ON COLUMN pitching_stats.errors IS '投球中の味方エラー';
COMMENT ON COLUMN pitching_stats.wins IS '勝利';
COMMENT ON COLUMN pitching_stats.loses IS '敗北';
COMMENT ON COLUMN pitching_stats.holds IS 'ホールド';
COMMENT ON COLUMN pitching_stats.saves IS 'セーブ';
COMMENT ON COLUMN pitching_stats.created_at IS '作成日時';
COMMENT ON COLUMN pitching_stats.updated_at IS '更新日時';
