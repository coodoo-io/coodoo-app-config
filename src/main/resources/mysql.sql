CREATE TABLE app_config (
  config_key varchar(128) NOT NULL,
  config_type varchar(16) NOT NULL,
  config_value varchar(1024) DEFAULT NULL,
  config_large_value text DEFAULT NULL,
  PRIMARY KEY (config_key),
  UNIQUE (config_key)
);
