package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SymbolAtom;

/**
 * Base class for all parameters.
 *
 * Created on 28/02/15.
 */
abstract class BaseParameter implements Parameter {
    private static final long serialVersionUID = 5866176494734932354L;
    private final SymbolAtom name;

    BaseParameter(final SymbolAtom name) {
        this.name = name;
    }

    @Override
    public SymbolAtom getName() {
        return name;
    }

    @SuppressWarnings("all")
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BaseParameter that = (BaseParameter) o;

        if (!name.equals(that.name)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return name.toString();
    }

}
