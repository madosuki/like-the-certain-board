create table if not exists mysite.kakolog (id integer auto_increment, unixtime bigint not null, title text not null, `board-id` integer not null, primary key(id, unixtime))
