package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SymbolAtom;

/**
 * Created on 28/02/15.
 */
public abstract class BaseParameter implements Parameter {
    private final SymbolAtom name;

    public BaseParameter(SymbolAtom name) {
        this.name = name;
    }

    public SymbolAtom getName() {
        return name;
    }

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
