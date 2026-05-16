create table if not exists caps (
id serial primary key,
"user-id" integer not null,
"cap-text" text not null,
"created-at" timestamp not null default current_timestamp,
"updated-at" timestamp not null default current_timestamp,
constraint uq_user_id unique ("user-id"),
constraint fk_caps_user_id foreign key ("user-id") references "user-table" (id) on delete cascade on update cascade
);
