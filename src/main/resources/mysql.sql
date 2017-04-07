CREATE TABLE app_config (
  config_key VARCHAR(128) NOT NULL,
  config_type VARCHAR(32) NOT NULL,
  config_value VARCHAR(1024) DEFAULT NULL,
  config_large_value text DEFAULT NULL,
  PRIMARY KEY (config_key),
  UNIQUE (config_key)
);
