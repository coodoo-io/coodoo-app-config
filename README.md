# AppConfig #


*Simple persistent application configuration service*

This library provides you an easy way to store and access properties in the database.
Available value type:
 - **String** *- gets stored as a CLOB if it exceeds a designated length*
 - **Long** *- good for any number so far*
 - **Boolean** *- to dodge null checks you can ask the service for a native boolean result*
 - **Password** *- like String, but it will be stored encrypted*
 - **List of Strings** *- collection of strings that can be empty but never null*
 - **List of Longs** *- you got the idea...*

## Getting started

1. Prepare your database

   Create the AppConfig table `app_config`

   ```sql
   
	CREATE TABLE app_config (
	    config_key varchar(128) NOT NULL,
	    config_type varchar(16) NOT NULL,
	    config_value varchar(1024) DEFAULT NULL,
	    config_large_value text DEFAULT NULL,
	    PRIMARY KEY (config_key),
	    UNIQUE (config_key)
	);
   
    ```
    This is a MySQL example, see [here](https://github.com/coodoo-io/appconfig/tree/master/src/main/resources) for more.

2. Add the following dependency to your project ([published on Maven Central](http://search.maven.org/#artifactdetails%7Cio.coodoo%7Cappconfig%7C1.0.0%7Cjar)):

   ```xml
	<dependency>
	    <groupId>io.coodoo</groupId>
	    <artifactId>appconfig</artifactId>
	    <version>1.0.0</version>
	</dependency>
   ```

3. Define your Configurations

   Create an emum that implements `AppConfigKey` located in the `io.coodoo.appconfig.boundary` package and name it whatever you want.
   

   ```java
   
	public enum AppConfig implements AppConfigKey {

	    IMPORTANT_THINGS_ACTIVE(ValueType.BOOLEAN),
	    IMPORTANT_THINGS(ValueType.LONG_LIST);
	    // ...
	
	    private AppConfig(ValueType type) { this.type = type; }
	    private ValueType type;
	
	    @Override
	    public String getId() { return name(); }
	    @Override
	    public ValueType getType() { return type; }
	    @Override
	    public boolean isDBValue() { return true; }
	    @Override
	    public String getDefaultValue() { return null; }
	}
    ```

   
4. Usage in a Stateless Bean

   Inject `AppConfigs` located in the `io.coodoo.appconfig.boundary` package.
   Just get and set Values as you need it.
   

   ```java
	@Stateless
	public void ExampleService {

	    @Inject
	    AppConfigs appConfigs;    

	    public void doImportantThings() {
	        if(appConfigs.getNativeBoolean(AppConfig.IMPORTANT_THINGS_ACTIVE)){
	            for(Long id : appConfigs.getLongList(AppConfig.IMPORTANT_THINGS)){
	                doImportantThing(id);
	            }
	            appConfigs.setBoolean(AppConfig.IMPORTANT_THINGS_ACTIVE, false);
	        }
	    }
	    // ...
	}
    ```

   
