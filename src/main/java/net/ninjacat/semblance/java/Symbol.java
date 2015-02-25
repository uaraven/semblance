package net.ninjacat.semblance.java;

/**
 * Java representation of Semblance symbol. Immutable class that holds its value.
 */
public class Symbol {
    public final String value;

    public Symbol(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
