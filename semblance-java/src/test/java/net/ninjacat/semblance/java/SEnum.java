package net.ninjacat.semblance.java;

public enum SEnum {
    Value1(1),
    Value2(2);

    private final int intValue;

    SEnum(final int intValue) {
        this.intValue = intValue;
    }

    public int getIntValue() {
        return intValue;
    }
}
