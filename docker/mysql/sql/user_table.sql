create table mysite.user_table (id int primary key auto_increment, board_name text not null, user_name text not null, hash char(64) binary not null, create_date datetime not null, latest_date datetime null, session text, is_admin bool default 0 not null, cap_text text);
