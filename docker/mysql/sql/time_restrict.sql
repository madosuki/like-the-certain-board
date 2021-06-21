create table if not exists mysite.`time-restrict` (id integer primary key auto_increment, ipaddr varbinary(16) not null, count integer not null default 0, `last-unixtime` bigint not null, `penalty-count` integer not null default 0)

