create table mysite.posted_table (id INTEGER PRIMARY KEY AUTO_INCREMENT, session_data text, appearance_date datetime not null, is_penalty bool, count tinyint default 1, wait_time integer);

