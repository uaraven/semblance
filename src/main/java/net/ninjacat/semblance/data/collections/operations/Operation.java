package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.SymbolAtom;

import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Collection operation names
 */
public enum Operation {
    HEAD(":head"),
    TAIL(":tail"),
    LAST(":last"),
    TAKE(":take"),
    DROP(":drop"),
    REVERSE(":reverse"),
    SORT(":sort"),
    MAP(":map"),
    FILTER(":filter"),
    APPEND(":append"),
    PREPEND(":prepend"),
    LENGTH(":length");

    private final String name;

    Operation(final String name) {
        this.name = name;
    }

    /**
     * @return name as a Symbol
     */
    public SymbolAtom asSymbol() {
        return symbol(name);
    }
}
