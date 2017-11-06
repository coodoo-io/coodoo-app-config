package io.coodoo.framework.appconfig.boundary;

public class AppConfigException extends RuntimeException {

    public AppConfigException(String message) {
        super(message);
    }

    public AppConfigException(String message, Exception exception) {
        super(message, exception);
    }

}
