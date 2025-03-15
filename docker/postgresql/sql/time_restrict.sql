create table if not exists "time-restrict" (id serial, ipaddr bytea not null, count integer not null default 0, "last-unixtime" bigint not null, "penalty-count" integer not null default 0, primary key (id))

