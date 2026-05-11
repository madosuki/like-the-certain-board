create table if not exists kakolog (
id serial,
unixtime bigint not null,
title text not null,
"board-id" integer not null,
primary key(id, unixtime),
constraint fk_kakolog foreign key ("board-id") references "board-list" (id))
