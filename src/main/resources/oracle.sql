CREATE TABLE app_config (
  config_key varchar2(128) NOT NULL,
  config_type varchar2(32) NOT NULL,
  config_value varchar2(1024) DEFAULT NULL,
  config_large_value CLOB DEFAULT NULL,
  CONSTRAINT app_config_pk PRIMARY KEY (config_key)
);
