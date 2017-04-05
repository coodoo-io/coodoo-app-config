package io.coodoo.appconfig.boundary;

/**
 * Configuration key interface
 * 
 * @author coodoo GmbH (coodoo.io)
 */
public interface AppConfigKey {

    /**
     * @return the unique identifier of the configuration
     */
    public String getId();

    /**
     * @return the values type
     */
    public ValueType getType();

}
