package net.ninjacat.semblance.java;

@SuppressWarnings("ClassNamingConvention")
public class Pojo {

    private final int intValue;
    private final String stringValue;
    private final Double doubleValue;

    public Pojo() {
        intValue = 42;
        stringValue = "";
        doubleValue = 42e42;
    }

    public Pojo(final int intValue, final String stringValue, final Double doubleValue) {

        this.intValue = intValue;
        this.stringValue = stringValue;
        this.doubleValue = doubleValue;
    }

    public Pojo(final String str) {
        doubleValue = 0.0;
        intValue = 0;
        stringValue = str;
    }

    public Pojo(final Pojo other) {
        doubleValue = other.getDoubleValue();
        intValue = other.getIntValue();
        stringValue = other.getStringValue();
    }

    public int getIntValue() {
        return intValue;
    }

    public String getStringValue() {
        return stringValue;
    }

    public Double getDoubleValue() {
        return doubleValue;
    }
}
