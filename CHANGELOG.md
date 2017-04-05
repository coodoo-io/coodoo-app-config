

<!--
### Bug Fixes
### Features
### BREAKING CHANGES
-->

<a name="1.1.0"></a>

## 1.1.0 (2017-04-04)

### Features

 * No EJB components anymore, just plain CDI
 * All value types are now capable for encryption
 * A value once set can be immutable using `AppConfigKeyAttributes.isImmutable()`
 * Configuration static loader: `AppConfigSettings.loadProperties()`

### BREAKING CHANGES

 * The value type PASSWORD is no longer available, so are the methods `AppConfigs.getPassword()` and `AppConfigs.setPassword()`
 * The methods `AppConfigKey.isDBValue()` and `AppConfigKey.getDefaultValue()` moved to the new optional interface `AppConfigKeyAttributes`
 * To provide the EntityManager you have to implement a `@AppConfigsEntityManager` CDI producer

<a name="1.0.1"></a>

## 1.0.1 (2017-03-28)

### Bug Fixes

 * Fixed: Setting values failed on a wrong value type check


<a name="1.0.0"></a>

## 1.0.0 (2017-03-27)

### Features

Initial release:

* Configuration key interface
* Supported types: String, Long, Boolean, Password (encrypted string), List&lt;String&gt; & List&lt;Long&gt;
* Default value
* Settings via property file