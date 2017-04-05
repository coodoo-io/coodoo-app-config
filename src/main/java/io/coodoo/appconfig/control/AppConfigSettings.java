package io.coodoo.appconfig.control;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.regex.Pattern;

import javax.annotation.PostConstruct;
import javax.ejb.Singleton;
import javax.ejb.Startup;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Setting for the AppConfigs service
 * 
 * @author coodoo GmbH (coodoo.io)
 */
@Singleton
@Startup
public class AppConfigSettings {

    private static Logger log = LoggerFactory.getLogger(AppConfigSettings.class);

    /**
     * Maximal length to persist the value as a String, larger values will get persisted as CLOB
     */
    public static int MAX_LENGTH = 1024;

    /**
     * Separator for collection entries represented as String
     */
    public static String LIST_SEPARATOR = ";";

    /**
     * Pattern for the collection separator
     */
    public static final Pattern SPLIT_PATTERN = Pattern.compile(LIST_SEPARATOR);

    /**
     * 1. secret key for password encryption / decryption
     */
    public static String SECRET_KEY_1 = "sadkF$HUy2A#D%kd";

    /**
     * 2. secret key for password encryption / decryption
     */
    public static String SECRET_KEY_2 = "weJiSEvR8yAC5ftB";

    /**
     * Name of the (optional) AppConfig property file
     */
    private static final String appconfigsPropertiesFilename = "coodoo.appconfigs.properties";

    static Properties properties = new Properties();

    @PostConstruct
    public void init() {
        AppConfigSettings.loadProperties();
    }

    public static void loadProperties() {
        InputStream inputStream = null;
        try {
            inputStream = AppConfigSettings.class.getClassLoader().getResourceAsStream(appconfigsPropertiesFilename);

            if (inputStream != null) {

                properties.load(inputStream);
                log.info("Reading {}", appconfigsPropertiesFilename);

                MAX_LENGTH = loadProperty(MAX_LENGTH, "coodoo.appconfigs.maxlength");
                LIST_SEPARATOR = loadProperty(LIST_SEPARATOR, "coodoo.appconfigs.list.separator");
                SECRET_KEY_1 = loadProperty(SECRET_KEY_1, "coodoo.appconfigs.secret.key1");
                SECRET_KEY_2 = loadProperty(SECRET_KEY_2, "coodoo.appconfigs.secret.key2");
            }
        } catch (IOException e) {
            log.info("Couldn't read {}!", appconfigsPropertiesFilename, e);
        } finally {
            try {
                if (inputStream != null) {
                    inputStream.close();
                }
            } catch (IOException e) {
                log.warn("Couldn't close {}!", appconfigsPropertiesFilename, e);
            }
        }
    }

    private static String loadProperty(String value, String key) {

        String property = properties.getProperty(key);
        if (property == null) {
            return value;
        }
        log.info("AppConfigs Property {} loaded: {}", key, property);
        return property;
    }

    private static int loadProperty(int value, String key) {
        String property = properties.getProperty(key);
        if (property != null) {
            try {
                log.info("AppConfigs Property {} loaded: {}", key, property);
                return Integer.valueOf(property).intValue();
            } catch (NumberFormatException e) {
                log.warn("AppConfigs Property {} value invalid: {}", key, property);
            }
        }
        return value;
    }

}
