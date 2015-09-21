CREATE TABLE users (
	user_id INTEGER PRIMARY KEY NOT NULL UNIQUE,
	ident TEXT NOT NULL UNIQUE
);

CREATE TABLE nick (
	nick_id INTEGER PRIMARY KEY NOT NULL UNIQUE,
	user_id INTEGER,
	nick TEXT,
	FOREIGN KEY (user_id) REFERENCES users (user_id)
);

CREATE TABLE channels (
	channel_id INTEGER PRIMARY KEY NOT NULL UNIQUE,
	channel TEXT NOT NULL UNIQUE,
	modes TEXT,
	password TEXT
);

CREATE TABLE userchannel (
	user_id INTEGER,
	channel_id INTEGER,
	op BOOLEAN,
	banned BOOLEAN,
	FOREIGN KEY (channel_id) REFERENCES channels (channel_id),
	FOREIGN KEY (user_id) REFERENCES users (user_id)
);

CREATE TABLE urls (
	url_id INTEGER PRIMARY KEY NOT NULL UNIQUE,
	url TEXT NOT NULL,
	alias TEXT NOT NULL UNIQUE
);
