create table if not exists "user-table" (
id serial primary key,
"user-name" text not null,
mail varchar(255) not null,
"password-hash" varchar(255) not null,
"create-date" timestamp not null,
"latest-date" timestamp null
);
