package net.ninjacat.semblance.java;

import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.iterators.Iter;

@SuppressWarnings("ClassNamingConvention")
public class Pojo {

    public static long sLong;
    public static String sStr;

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

    public static String name() {
        return Pojo.class.getSimpleName();
    }

    public static String[] toStr(final Integer[] input) {
        return Iter.of(input).map(new Func<String, Integer>() {
            @Override
            public String apply(final Integer integer) {
                return String.valueOf(integer);
            }
        }).toArray(new String[input.length]);
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

    public String toStr(final String[] x, final boolean y) {
        return String.format("%s: %s", x[y ? 1 : 0], y);
    }
}
