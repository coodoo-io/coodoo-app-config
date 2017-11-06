package io.coodoo.framework.appconfig.boundary;

import java.io.UnsupportedEncodingException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
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

import io.coodoo.framework.appconfig.boundary.annotation.AppConfigEntityManager;
import io.coodoo.framework.appconfig.control.AppConfigSettings;
import io.coodoo.framework.appconfig.control.EncryptDecrypt;
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

        return (String) stringToValue(key.getType(), getRawValue(key));
    }

    public Long getLong(AppConfigKey key) {

        return (Long) stringToValue(key.getType(), getRawValue(key));
    }

    public Boolean getBoolean(AppConfigKey key) {

        return (Boolean) stringToValue(key.getType(), getRawValue(key));
    }

    public boolean getNativeBoolean(AppConfigKey key) {

        return Boolean.TRUE.equals(getBoolean(key));
    }

    @SuppressWarnings("unchecked")
    public List<String> getStringList(AppConfigKey key) {

        return (List<String>) stringToValue(key.getType(), getRawValue(key));
    }

    @SuppressWarnings("unchecked")
    public List<Long> getLongList(AppConfigKey key) {

        return (List<Long>) stringToValue(key.getType(), getRawValue(key));
    }

    public void setString(AppConfigKey key, String value) {

        setRawValue(key, value);
    }

    public void setLong(AppConfigKey key, Long value) {

        setRawValue(key, valueToString(key.getType(), value));
    }

    public void setBoolean(AppConfigKey key, Boolean value) {

        setRawValue(key, valueToString(key.getType(), value));
    }

    public void setStringList(AppConfigKey key, List<String> value) {

        setRawValue(key, valueToString(key.getType(), value));
    }

    public void setLongList(AppConfigKey key, List<Long> value) {

        setRawValue(key, valueToString(key.getType(), value));
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

            value = consultDecryption(key, value);

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

        String valueString = consultEncryption(key, value);

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

    private Object stringToValue(ValueType valueType, String string) {

        if (string == null) {
            return null;
        }
        if (valueType == null) {
            throw new AppConfigException("ValueType is missing for given value: " + string);
        }
        try {
            switch (valueType) {

                case STRING:
                case STRING_ENCRYPTED:
                    return string;

                case LONG:
                case LONG_ENCRYPTED:
                    return Long.valueOf(string);

                case BOOLEAN:
                case BOOLEAN_ENCRYPTED:
                    if (string.equals(Boolean.TRUE.toString())) {
                        return Boolean.TRUE;
                    }
                    if (string.equals(Boolean.FALSE.toString())) {
                        return Boolean.FALSE;
                    }
                    return null;

                case STRING_LIST:
                case STRING_LIST_ENCRYPTED:
                    return AppConfigSettings.SPLIT_PATTERN.splitAsStream(string).map(String::new).collect(Collectors.toList());

                case LONG_LIST:
                case LONG_LIST_ENCRYPTED:
                    return AppConfigSettings.SPLIT_PATTERN.splitAsStream(string).map(Long::valueOf).collect(Collectors.toList());

                default:
                    throw new AppConfigException("ValueType unknown!");
            }
        } catch (Exception e) {
            throw new AppConfigException("ValueType " + valueType.name() + " can't process given string: " + string, e);
        }
    }

    @SuppressWarnings("unchecked")
    private String valueToString(ValueType valueType, Object value) {

        if (value == null) {
            return null;
        }
        if (valueType == null) {
            throw new AppConfigException("ValueType is missing for given value: " + value);
        }
        try {
            switch (valueType) {

                case STRING:
                case STRING_ENCRYPTED:
                case LONG:
                case LONG_ENCRYPTED:
                case BOOLEAN:
                case BOOLEAN_ENCRYPTED:
                    return String.valueOf(value);

                case STRING_LIST:
                case STRING_LIST_ENCRYPTED:
                    return ((List<String>) value).stream().filter(Objects::nonNull).map(s -> s.toString())
                                    .collect(Collectors.joining(AppConfigSettings.LIST_SEPARATOR));

                case LONG_LIST:
                case LONG_LIST_ENCRYPTED:
                    return ((List<Long>) value).stream().filter(Objects::nonNull).map(l -> l.toString())
                                    .collect(Collectors.joining(AppConfigSettings.LIST_SEPARATOR));

                default:
                    throw new AppConfigException("ValueType unknown!");
            }
        } catch (Exception e) {
            throw new AppConfigException("ValueType " + valueType.name() + " can't process given value: " + value, e);
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
                throw new AppConfigException("Can't encrypt value", e);
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
                throw new AppConfigException("Can't decrypt value", e);
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
