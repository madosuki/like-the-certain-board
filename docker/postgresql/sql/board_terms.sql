create table if not exists board_terms (
id serial primary key,
"board-id" integer not null,
body text not null,
"created-at" timestamp not null default current_timestamp,
"updated-at" timestamp not null default current_timestamp,
constraint fk_board_terms_board foreign key ("board-id") references "board-list" (id) on delete cascade
)
