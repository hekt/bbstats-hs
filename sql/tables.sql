CREATE TABLE mst_player (
  id                  serial,
  player_name         varchar(16) NOT NULL,
  uniform_number      varchar(4)  DEFAULT NULL,
  temp_uniform_number varchar(4)  DEFAULT NULL,
  created_at          timestamp   NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at          timestamp   NOT NULL DEFAULT CURRENT_TIMESTAMP,
  primary key (id),
  unique (uniform_number),
  unique (player_name)
);

CREATE TABLE tbl_game_score (
  id                     serial,
  game_date              date                     NOT NULL,
  game_number            smallint      	     	  NOT NULL DEFAULT 1,
  game_result	         smallint                 NOT NULL,
  ground                 varchar(32) 	          NOT NULL,
  attack_turn            smallint                 NOT NULL,
  runs                   varchar(32)  	     	  NOT NULL,
  total_runs             smallint    	     	  NOT NULL,
  total_hits  		     smallint    	     	  NOT NULL,
  total_errors 		     smallint    	    	  NOT NULL,
  opponent_name          varchar(32) 	     	  DEFAULT NULL,
  opponent_runs          varchar(32)  	     	  DEFAULT NULL,
  opponent_total_runs    smallint    	     	  DEFAULT NULL,
  opponent_total_hits    smallint    	     	  DEFAULT NULL,
  opponent_total_errors  smallint    	     	  DEFAULT NULL,
  created_at             timestamp   	     	  NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at             timestamp   	     	  NOT NULL DEFAULT CURRENT_TIMESTAMP,
  primary key (id),
  unique (game_date, game_number)
);

CREATE TABLE tbl_at_bat (
  id                   serial,
  game_id              integer    NOT NULL,
  player_id            integer 	  NOT NULL,
  at_bat_number        smallint   NOT NULL,
  inning               smallint   NOT NULL,
  rbi                  smallint   NOT NULL DEFAULT 0,
  out_count            smallint   NOT NULL,
  result_text          varchar(8) NOT NULL,
  result_kind          varchar(4) NOT NULL,
  is_risp              boolean 	  NOT NULL DEFAULT FALSE,
  is_counts_at_bat     boolean 	  NOT NULL,
  created_at           timestamp  NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at           timestamp  NOT NULL DEFAULT CURRENT_TIMESTAMP,
  primary key (id),
  foreign key (game_id)   references tbl_game_score(id),
  foreign key (player_id) references mst_player(id),
  unique (game_id, player_id, at_bat_number)
);

CREATE TABLE tbl_batting_result (
  id                serial,
  game_id           integer     NOT NULL,
  player_id         integer     NOT NULL,
  batting_order     smallint    NOT NULL,
  appearance_order  smallint    NOT NULL,
  positions         varchar(32) NOT NULL,
  runs              smallint    NOT NULL,
  stolen_bases      smallint    NOT NULL,
  errors            smallint    NOT NULL,
  created_at        timestamp   NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at	    timestamp   NOT NULL DEFAULT CURRENT_TIMESTAMP,
  primary key (id),
  foreign key (game_id)   references tbl_game_score(id),
  foreign key (player_id) references mst_player(id),
  unique (game_id, player_id)
);

-- CREATE TABLE tbl_batting_stats (
--   player_id integer NOT NULL,
--   plate_appearances  integer NOT NULL DEFAULT 0,
--   games  integer NOT NULL DEFAULT 0,
--   at_bats  integer NOT NULL DEFAULT 0,
--   rbi  integer NOT NULL DEFAULT 0,
--   hits  integer NOT NULL DEFAULT 0,
--   walks  integer NOT NULL DEFAULT 0,
--   home_runs  integer NOT NULL DEFAULT 0,
--   runs  integer NOT NULL DEFAULT 0,
--   risp_at_bats  integer NOT NULL DEFAULT 0,
--   risp_hits  integer NOT NULL DEFAULT 0,
--   stolen_bases  integer NOT NULL DEFAULT 0,
--   errors  integer NOT NULL DEFAULT 0,
--   created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
--   updated_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
--   PRIMARY KEY (player_id),
--   FOREIGN KEY (player_id) REFERENCES mst_player(id)
-- );

CREATE TABLE tbl_pitching_result (
  id                serial,
  game_id           integer   NOT NULL,
  player_id         integer   NOT NULL,
  appearance_order  smallint  NOT NULL,
  outs              smallint  NOT NULL,
  batters_faced     smallint  NOT NULL,
  runs              smallint  NOT NULL,
  earned_runs       smallint  NOT NULL,
  strike_outs       smallint  NOT NULL,
  walks             smallint  NOT NULL,
  hits              smallint  NOT NULL,
  home_runs         smallint  NOT NULL,
  errors            smallint  NOT NULL,
  decision          smallint  NOT NULL,
  created_at        timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at        timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  primary key (id),
  foreign key (player_id) references mst_player(id),
  foreign key (game_id)   references tbl_game_score(id),
  unique (player_id, game_id)
);

-- CREATE TABLE tbl_pitching_stats (
--   player_id  integer NOT NULL,
--   games  integer NOT NULL,
--   outs  integer NOT NULL,
--   batters_faced  integer NOT NULL,
--   runs  integer NOT NULL,
--   earned_runs  integer NOT NULL,
--   strike_outs  integer NOT NULL,
--   walks  integer NOT NULL,
--   hits  integer NOT NULL,
--   home_runs integer NOT NULL,
--   errors  integer NOT NULL,
--   wins  integer NOT NULL,
--   loses  integer NOT NULL,
--   holds  integer NOT NULL,
--   saves  integer NOT NULL,
--   created_at  timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
--   updated_at  timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
--   PRIMARY KEY (player_id),
--   FOREIGN KEY (player_id) REFERENCES mst_player(id)
-- );
