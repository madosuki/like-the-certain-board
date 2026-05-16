create table if not exists "user-roles" (
"user-id" integer not null,
"role-id" integer not null,
constraint user_roles_pkey primary key ("user-id", "role-id"),
constraint fk_user_roles_user foreign key ("user-id") references "user-table" (id) on delete cascade on update cascade,
constraint fk_user_roles_role foreign key ("role-id") references roles (id) on delete cascade on update cascade
);
