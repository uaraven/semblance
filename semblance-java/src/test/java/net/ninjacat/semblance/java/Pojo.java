package net.ninjacat.semblance.java;

@SuppressWarnings("ClassNamingConvention")
public class Pojo {

    public int intValue;
    public String stringValue;
    public Double doubleValue;
    public boolean boolValue;

    public Pojo() {
        intValue = 42;
        stringValue = "";
        doubleValue = 42e42;
        boolValue = true;
    }

    public Pojo(final int intValue, final String stringValue, final Double doubleValue) {

        this.intValue = intValue;
        this.stringValue = stringValue;
        this.doubleValue = doubleValue;
        boolValue = false;
    }

    public Pojo(final String str) {
        doubleValue = 0.0;
        intValue = 0;
        stringValue = str;
        boolValue = false;
    }

    public Pojo(final Pojo other) {
        doubleValue = other.getDoubleValue();
        intValue = other.getIntValue();
        stringValue = other.getStringValue();
        boolValue = other.isBoolValue();
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

    public boolean isBoolValue() {
        return boolValue;
    }

    public double power(final int x, final long y) {
        return Math.pow(x, y);
    }
}
