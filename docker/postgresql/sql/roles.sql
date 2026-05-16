create table if not exists roles (
id serial primary key,
name varchar(50) not null,
constraint uq_role_name unique (name)
);
