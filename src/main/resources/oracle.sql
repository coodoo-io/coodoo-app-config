CREATE TABLE app_config (
  config_key VARCHAR2(128) NOT NULL,
  config_type VARCHAR2(32) NOT NULL,
  config_value VARCHAR2(1024) DEFAULT NULL,
  config_large_value CLOB DEFAULT NULL
);

ALTER TABLE app_config ADD (
  CONSTRAINT app_config_pk PRIMARY KEY (id)
);
