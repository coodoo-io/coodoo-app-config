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
import javax.inject.Inject;
import javax.persistence.EntityManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.coodoo.appconfig.boundary.annotation.AppConfigEntityManager;
import io.coodoo.appconfig.control.AppConfigSettings;
import io.coodoo.appconfig.control.EncryptDecrypt;
import io.coodoo.appconfig.entity.AppConfigValue;

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

        if (!isDBValue(key)) {
            return getDefaultValue(key);
        }

        AppConfigValue config = entityManager.find(AppConfigValue.class, key.getId());

        if (config != null) {

            if (!config.getType().equals(key.getType())) {
                log.error("Abort loading {}, wrong type: {}, expected {}!", key.getId(), key.getType(), config.getType());
                return null;
            }

            String value = null;

            if (config.getValue() != null) {
                value = config.getValue();
            } else {
                value = config.getLargeValue();
            }

            value = consultDecryption(key, value);

            log.debug("Loading {}: {}", key.getId(), value);
            return value;
        }
        return null;
    }

    private void setValue(AppConfigKey key, String value) {

        if (isDBValue(key)) {
            log.error("Can't set value if marked \"isDBValue = false\"!", key.getId());
            return;
        }

        AppConfigValue config = entityManager.find(AppConfigValue.class, key.getId());

        if (config != null && !config.getType().equals(key.getType())) {
            log.error("Abort saving {}, wrong type: {}, expected {}!", key.getId(), key.getType(), config.getType());
            return;
        }

        if (value == null) {
            entityManager.remove(config);
            log.debug("Removing {}", key.getId());
            return;
        }

        if (config == null) {

            config = new AppConfigValue();
            config.setKey(key.getId());
            config.setType(key.getType());
            setValueToEntity(consultEncryption(key, value), config);
            log.debug("Saving {}: {}", key.getId(), value);
            entityManager.persist(config);

        } else {
            if (isImmutable(key)) {
                log.error("Can't update immutable value {}: {}", key.getId(), value);
                return;
            }
            setValueToEntity(consultEncryption(key, value), config);
            log.debug("Updating {}: {}", key.getId(), value);
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

    private String consultEncryption(AppConfigKey key, String value) {

        if (key.getType().encrypted()) {
            try {
                EncryptDecrypt encryptDecrypt = new EncryptDecrypt();
                return encryptDecrypt.encrypt(value);

            } catch (UnsupportedEncodingException | NoSuchPaddingException | NoSuchAlgorithmException | InvalidKeyException | InvalidAlgorithmParameterException
                            | BadPaddingException | IllegalBlockSizeException e) {
                log.error("Can't encrypt value", e);
            }
        }
        return value;
    }

    private String consultDecryption(AppConfigKey key, String value) {

        if (key.getType().encrypted()) {
            try {
                EncryptDecrypt encryptDecrypt = new EncryptDecrypt();
                return encryptDecrypt.decrypt(value);

            } catch (UnsupportedEncodingException | NoSuchPaddingException | NoSuchAlgorithmException | InvalidKeyException | InvalidAlgorithmParameterException
                            | BadPaddingException | IllegalBlockSizeException e) {
                log.error("Can't decrypt value", e);
            }
        }
        return value;
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
