package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispValue;

/**
 * Binding representation for exporting from namespace
 */
public class Binding {
    private final SymbolAtom name;
    private final LispValue value;

    /**
     * Creates a new instance of binding
     *
     * @param name  name of the binding
     * @param value bound value
     */
    Binding(final SymbolAtom name, final LispValue value) {
        this.name = name;
        this.value = value;
    }

    /**
     * @return Binding name
     */
    public SymbolAtom getName() {
        return name;
    }

    /**
     * @return Bound value
     */
    public LispValue getValue() {
        return value;
    }
}
