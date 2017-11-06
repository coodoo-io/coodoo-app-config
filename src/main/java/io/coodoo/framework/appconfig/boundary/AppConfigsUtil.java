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

import io.coodoo.framework.appconfig.control.AppConfigSettings;
import io.coodoo.framework.appconfig.control.EncryptDecrypt;

/**
 * AppConfigsUtil
 * 
 * @author coodoo GmbH (coodoo.io)
 */
public class AppConfigsUtil {

    public static Object toObject(ValueType valueType, String string) {

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
    public static String toString(ValueType valueType, Object value) {

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

    public static void validateValue(ValueType valueType, String string) throws AppConfigException {
        AppConfigsUtil.toString(valueType, AppConfigsUtil.toObject(valueType, string));
    }

    public static String encrypt(String value) {

        try {
            EncryptDecrypt encryptDecrypt = new EncryptDecrypt();
            return encryptDecrypt.encrypt(value);

        } catch (UnsupportedEncodingException | NoSuchPaddingException | NoSuchAlgorithmException | InvalidKeyException | InvalidAlgorithmParameterException
                        | BadPaddingException | IllegalBlockSizeException e) {
            throw new AppConfigException("Can't encrypt value", e);
        }
    }

    public static String decrypt(String value) {

        try {
            EncryptDecrypt encryptDecrypt = new EncryptDecrypt();
            return encryptDecrypt.decrypt(value);

        } catch (UnsupportedEncodingException | NoSuchPaddingException | NoSuchAlgorithmException | InvalidKeyException | InvalidAlgorithmParameterException
                        | BadPaddingException | IllegalBlockSizeException e) {
            throw new AppConfigException("Can't decrypt value", e);
        }
    }

}
