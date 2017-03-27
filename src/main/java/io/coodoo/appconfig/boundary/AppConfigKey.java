package io.coodoo.appconfig.boundary;

/**
 * Configuration key interface
 * 
 * @author coodoo
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

    /**
     * @return <code>true</code> if the configuration is stored in the database, <code>false</code> if not. If <code>false</code> is set, the configuration
     *         needs a default value!
     */
    public boolean isDBValue();

    /**
     * @return default value of the configuration if it is not stored in the database
     */
    public String getDefaultValue();

}
