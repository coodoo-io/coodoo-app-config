package io.coodoo.framework.appconfig.boundary;

import java.util.List;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.coodoo.framework.appconfig.boundary.annotation.AppConfigEntityManager;
import io.coodoo.framework.appconfig.control.AppConfigSettings;
import io.coodoo.framework.appconfig.entity.AppConfigValue;

/**
 * AppConfigs service
 * 
 * @author coodoo GmbH (coodoo.io)
 */
@Stateless
public class AppConfigs {

    private static Logger log = LoggerFactory.getLogger(AppConfigs.class);

    @Inject
    @AppConfigEntityManager
    EntityManager entityManager;

    public String getString(AppConfigKey key) {

        return (String) AppConfigsUtil.toObject(key.getType(), getRawValue(key));
    }

    public Long getLong(AppConfigKey key) {

        return (Long) AppConfigsUtil.toObject(key.getType(), getRawValue(key));
    }

    public Boolean getBoolean(AppConfigKey key) {

        return (Boolean) AppConfigsUtil.toObject(key.getType(), getRawValue(key));
    }

    public boolean getNativeBoolean(AppConfigKey key) {

        return Boolean.TRUE.equals(getBoolean(key));
    }

    @SuppressWarnings("unchecked")
    public List<String> getStringList(AppConfigKey key) {

        return (List<String>) AppConfigsUtil.toObject(key.getType(), getRawValue(key));
    }

    @SuppressWarnings("unchecked")
    public List<Long> getLongList(AppConfigKey key) {

        return (List<Long>) AppConfigsUtil.toObject(key.getType(), getRawValue(key));
    }

    public void setString(AppConfigKey key, String value) {

        setRawValue(key, value);
    }

    public void setLong(AppConfigKey key, Long value) {

        setRawValue(key, AppConfigsUtil.toString(key.getType(), value));
    }

    public void setBoolean(AppConfigKey key, Boolean value) {

        setRawValue(key, AppConfigsUtil.toString(key.getType(), value));
    }

    public void setStringList(AppConfigKey key, List<String> value) {

        setRawValue(key, AppConfigsUtil.toString(key.getType(), value));
    }

    public void setLongList(AppConfigKey key, List<Long> value) {

        setRawValue(key, AppConfigsUtil.toString(key.getType(), value));
    }

    public String getRawValue(AppConfigKey key) {

        if (!isDBValue(key)) {
            return getDefaultValue(key);
        }

        AppConfigValue config = entityManager.find(AppConfigValue.class, key.getId());

        if (config != null) {

            if (!config.getType().equals(key.getType())) {
                throw new AppConfigException("Abort loading " + key.getId() + ", wrong type: " + key.getType() + ", expected " + config.getType() + "!");
            }

            String value = null;

            if (config.getValue() != null) {
                value = config.getValue();
            } else {
                value = config.getLargeValue();
            }
            if (key.getType().encrypted()) {
                value = AppConfigsUtil.decrypt(value);
            }
            log.debug("Loading {}: {}", key.getId(), value);
            return value;
        }
        return null;
    }

    public void setRawValue(AppConfigKey key, String value) {

        if (!isDBValue(key)) {
            throw new AppConfigException("Can't set value " + key.getId() + " if marked \"isDBValue = false\"!");
        }

        AppConfigValue config = entityManager.find(AppConfigValue.class, key.getId());

        if (config != null && !config.getType().equals(key.getType())) {
            throw new AppConfigException("Abort saving " + key.getId() + ", wrong type: " + key.getType() + ", expected " + config.getType() + "!");
        }

        if (value == null) {
            entityManager.remove(config);
            log.debug("Removing {}", key.getId());
            return;
        }

        String valueString = value;
        if (key.getType().encrypted()) {
            valueString = AppConfigsUtil.encrypt(valueString);
        }
        if (config == null) {

            config = new AppConfigValue();
            config.setKey(key.getId());
            config.setType(key.getType());
            setValueToEntity(valueString, config);
            log.debug("Saving {}: {}", key.getId(), valueString);
            entityManager.persist(config);

        } else {

            if (isImmutable(key)) {
                throw new AppConfigException("Can't update immutable value " + key.getId() + ": " + valueString);
            }
            setValueToEntity(valueString, config);
            log.debug("Updating {}: {}", key.getId(), valueString);
        }
    }

    private void setValueToEntity(String value, AppConfigValue config) {
        if (value.length() <= AppConfigSettings.MAX_LENGTH) {
            config.setValue(value);
            config.setLargeValue(null);
        } else {
            config.setValue(null);
            config.setLargeValue(value);
        }
    }

    private boolean isDBValue(AppConfigKey key) {
        if (key instanceof AppConfigKeyAttributes) {
            return ((AppConfigKeyAttributes) key).isDBValue();
        }
        return true;
    }

    private String getDefaultValue(AppConfigKey key) {

        if (key instanceof AppConfigKeyAttributes) {

            String defaultValue = ((AppConfigKeyAttributes) key).getDefaultValue();
            if (defaultValue == null) {
                log.error("Default value not set for {}!", key.getId());
            } else {
                log.debug("Default value {}: {}", key.getId(), defaultValue);
                return defaultValue;
            }
        }
        return null;
    }

    private boolean isImmutable(AppConfigKey key) {
        if (key instanceof AppConfigKeyAttributes) {
            return ((AppConfigKeyAttributes) key).isImmutable();
        }
        return false;
    }

}
