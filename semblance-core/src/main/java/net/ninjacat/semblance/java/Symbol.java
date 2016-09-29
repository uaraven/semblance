package net.ninjacat.semblance.java;

import java.io.Serializable;

/**
 * Java representation of Semblance symbol. Immutable class that holds its value.
 */
public class Symbol implements Serializable {
    private static final long serialVersionUID = -5794198489049556109L;

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
