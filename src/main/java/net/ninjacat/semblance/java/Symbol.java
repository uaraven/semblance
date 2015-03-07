package net.ninjacat.semblance.java;

/**
 * Java representation of Semblance symbol. Immutable class that holds its value.
 */
public class Symbol {
    public final String value;

    /**
     * Creates new symbol representation.
     *
     * @param value Symbol value.
     */
    public Symbol(final String value) {
        this.value = value;
    }

    /**
     * @return Symbol value.
     */
    public String getValue() {
        return value;
    }
}
