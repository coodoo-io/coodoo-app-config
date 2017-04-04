

<!--
### Bug Fixes
### Features
### BREAKING CHANGES
-->

<a name="1.1.0"></a>

## 1.1.0 (2017-04-04)

### Features

 * All value types are now capable for encryption

### BREAKING CHANGES

 * The value type PASSWORD is no longer available, so are the methods `AppConfigs.getPassword()` and `AppConfigs.setPassword()`
 * The methods `AppConfigKey.isDBValue()` and `AppConfigKey.getDefaultValue()` moved to the new optional interface `AppConfigKeyAttributes`


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