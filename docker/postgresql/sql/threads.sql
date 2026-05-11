create table if not exists threads (
id serial primary key,
title text not null,
"create-date" timestamp not null,
"last-modified-date" timestamp not null,
"last-rise-date" timestamp not null,
"res-count" integer not null default 1,
unixtime bigint not null,
"max-line" integer default 1000 not null,
"board-id" integer not null,
constraint uq_threads_unixtime unique ("board-id", unixtime),
constraint fk_threads foreign key ("board-id") references "board-list" (id))
