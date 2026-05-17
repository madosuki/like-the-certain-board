create table if not exists "user-table" (
id serial primary key,
"user-name" text not null,
mail varchar(255) not null,
"password-hash" varchar(255) not null,
"create-date" timestamp not null default current_timestamp,
"latest-date" timestamp not null default current_timestamp,
constraint uq_user_table_user_name unique ("user-name")
);
