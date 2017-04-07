package io.coodoo.framework.appconfig.boundary;

/**
 * Available value types, each with an encrypted alternative
 * 
 * @author coodoo GmbH (coodoo.io)
 */
public enum ValueType {

    STRING(false),

    STRING_ENCRYPTED(true),

    LONG(false),

    LONG_ENCRYPTED(true),

    BOOLEAN(false),

    BOOLEAN_ENCRYPTED(true),

    STRING_LIST(false),

    STRING_LIST_ENCRYPTED(true),

    LONG_LIST(false),

    LONG_LIST_ENCRYPTED(true);

    private boolean encrypted;

    private ValueType(boolean encrypted) {
        this.encrypted = encrypted;
    }

    public boolean encrypted() {
        return encrypted;
    }

}
