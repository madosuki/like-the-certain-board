create table mysite.posted_ipaddr_table (ipaddr varchar(43) primary key, appearance_date datetime not null, is_penalty bool, count tinyint default 1, wait_time int);

