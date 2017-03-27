package io.coodoo.appconfig.boundary;

import java.io.UnsupportedEncodingException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.coodoo.appconfig.control.AppConfigSettings;
import io.coodoo.appconfig.control.EncryptDecrypt;
import io.coodoo.appconfig.entity.AppConfigValue;

@Stateless
public class AppConfigs {

    private static Logger log = LoggerFactory.getLogger(AppConfigs.class);

    @PersistenceContext
    EntityManager entityManager;

    public String getString(AppConfigKey key) {

        return getValue(key);
    }

    public void setString(AppConfigKey key, String value) {

        setValue(key, value);
    }

    public Long getLong(AppConfigKey key) {

        String value = getValue(key);
        if (value != null && value.matches("^-?\\d{1,37}$")) {
            return Long.valueOf(value);
        }
        return null;
    }

    public void setLong(AppConfigKey key, Long value) {

        setValue(key, value != null ? value.toString() : null);
    }

    public Boolean getBoolean(AppConfigKey key) {

        String value = getValue(key);
        if (value != null) {
            if (value.equals(Boolean.TRUE.toString())) {
                return Boolean.TRUE;
            }
            if (value.equals(Boolean.FALSE.toString())) {
                return Boolean.FALSE;
            }
        }
        return null;
    }

    public boolean getNativeBoolean(AppConfigKey key) {

        Boolean value = getBoolean(key);
        if (value != null) {
            return value;
        }
        return false;
    }

    public void setBoolean(AppConfigKey key, Boolean value) {

        setValue(key, value != null ? String.valueOf(value) : null);
    }

    public String getPassword(AppConfigKey key) {

        String value = getValue(key);
        if (value != null) {
            try {
                EncryptDecrypt encryptDecrypt = new EncryptDecrypt();
                return encryptDecrypt.decrypt(value);

            } catch (UnsupportedEncodingException | NoSuchPaddingException | NoSuchAlgorithmException | InvalidKeyException | InvalidAlgorithmParameterException
                            | BadPaddingException | IllegalBlockSizeException e) {
                log.error("Can't decrypt password", e);
            }
        }
        return null;
    }

    public void setPassword(AppConfigKey key, String value) {

        if (value != null) {
            try {
                EncryptDecrypt encryptDecrypt = new EncryptDecrypt();
                setValue(key, encryptDecrypt.encrypt(value));

            } catch (UnsupportedEncodingException | NoSuchPaddingException | NoSuchAlgorithmException | InvalidKeyException | InvalidAlgorithmParameterException
                            | BadPaddingException | IllegalBlockSizeException e) {
                log.error("Can't encrypt password", e);
            }
        } else {
            setValue(key, null);
        }
    }

    public List<String> getStringList(AppConfigKey key) {

        String value = getValue(key);
        if (value != null) {
            return AppConfigSettings.SPLIT_PATTERN.splitAsStream(value).map(String::new).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    public void setStringList(AppConfigKey key, List<String> value) {

        if (value != null) {
            setValue(key, value.stream().filter(Objects::nonNull).map(s -> s.toString()).collect(Collectors.joining(AppConfigSettings.LIST_SEPARATOR)));
        } else {
            setValue(key, null);
        }
    }

    public List<Long> getLongList(AppConfigKey key) {

        String value = getValue(key);
        if (value != null) {
            return AppConfigSettings.SPLIT_PATTERN.splitAsStream(value).map(Long::valueOf).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    public void setLongList(AppConfigKey key, List<Long> value) {

        if (value != null) {
            setValue(key, value.stream().filter(Objects::nonNull).map(l -> l.toString()).collect(Collectors.joining(AppConfigSettings.LIST_SEPARATOR)));
        } else {
            setValue(key, null);
        }
    }

    private String getValue(AppConfigKey key) {

        if (key.isDBValue()) {

            AppConfigValue config = entityManager.find(AppConfigValue.class, key.getId());

            if (config != null) {

                if (!config.getType().equals(key.getType())) {
                    log.error("Abort loading {}, wrong type: {}, expected {}!", key.getId(), key.getType(), config.getType());
                    return null;
                }
                if (config.getValue() != null) {
                    log.debug("Loading {}: {}", key.getId(), config.getValue());
                    return config.getValue();
                } else {
                    log.debug("Loading {}: {}", key.getId(), config.getLargeValue());
                    return config.getLargeValue();
                }
            }
        } else {
            log.debug("Default value {}: {}", key.getId(), key.getDefaultValue());
            if (key.getDefaultValue() == null) {
                log.error("No default value set for {}!", key.getId());
            }
            return key.getDefaultValue();
        }
        return null;
    }

    private void setValue(AppConfigKey key, String value) {

        if (key.isDBValue()) {

            AppConfigValue config = entityManager.find(AppConfigValue.class, key.getId());

            if (config != null && config.getType().equals(key.getType())) {
                log.error("Abort saving {}, wrong type: {}, expected {}!", key.getId(), key.getType(), config.getType());
                return;
            }

            if (value == null) {
                entityManager.remove(config);
                config = null;
                log.debug("Removing {}", key.getId());
            }

            if (config == null) {

                config = new AppConfigValue();
                config.setKey(key.getId());
                config.setType(key.getType());
                setValueToEntity(value, config);
                log.debug("Saving {}: {}", key.getId(), value);
                entityManager.persist(config);

            } else {
                setValueToEntity(value, config);
                log.debug("Updating {}: {}", key.getId(), value);
            }
        } else {
            log.error("Can't set value if marked \"isDBValue = false\"!", key.getId());
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
}
