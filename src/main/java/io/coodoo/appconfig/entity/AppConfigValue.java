package io.coodoo.appconfig.entity;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.Table;

import io.coodoo.appconfig.boundary.ValueType;

/**
 * AppConfig entity
 * 
 * @author coodoo GmbH (coodoo.io)
 */
@SuppressWarnings("serial")
@Entity
@Table(name = "app_config")
public class AppConfigValue implements Serializable {

    @Id
    @Column(name = "config_key")
    private String key;

    @Column(name = "config_type", nullable = false)
    @Enumerated(EnumType.STRING)
    private ValueType type;

    @Column(name = "config_value")
    private String value;

    @Column(name = "config_large_value")
    private String largeValue;

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public ValueType getType() {
        return type;
    }

    public void setType(ValueType type) {
        this.type = type;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getLargeValue() {
        return largeValue;
    }

    public void setLargeValue(String largeValue) {
        this.largeValue = largeValue;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((key == null) ? 0 : key.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        AppConfigValue other = (AppConfigValue) obj;
        if (key == null) {
            if (other.key != null)
                return false;
        } else if (!key.equals(other.key))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "AppConfigValue [key=" + key + ", type=" + type + ", value=" + value + ", largeValue=" + largeValue + "]";
    }

}
