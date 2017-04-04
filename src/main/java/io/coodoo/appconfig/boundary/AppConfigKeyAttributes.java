package io.coodoo.appconfig.boundary;

/**
 * Configuration key attributes interface
 * 
 * @author coodoo
 */
public interface AppConfigKeyAttributes {

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
