CREATE TABLE app_config (
  config_key character varying(128) PRIMARY KEY,
  config_type character varying(16) NOT NULL,
  config_value character varying(1024) DEFAULT NULL,
  config_large_value text DEFAULT NULL
);
