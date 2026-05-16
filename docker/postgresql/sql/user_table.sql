create table if not exists "user-table" (
id serial primary key,
"board-id" integer,
"user-name" text not null,
mail varchar(255) not null,
"password-hash" varchar(255) not null,
"create-date" timestamp not null,
"latest-date" timestamp null
);
