package net.ninjacat.semblance.java;

import java.io.Serializable;
import java.util.Objects;

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

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof Symbol)) {
            return false;
        }
        final Symbol symbol = (Symbol) other;
        return Objects.equals(value, symbol.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return "Symbol{'" + value + "'}";
    }
}
