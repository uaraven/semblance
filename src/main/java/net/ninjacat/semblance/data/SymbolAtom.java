package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.Symbol;

/**
 * Created on 24/02/15.
 */
public class SymbolAtom extends Atom {

    private final String value;

    public SymbolAtom(String value) {
        this.value = value;
    }

    public SymbolAtom(String value, SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    @Override
    public Symbol asJavaObject() {
        return new Symbol(value);
    }

    @Override
    public String repr() {
        return value;
    }


    @Override
    public SemblanceType getType() {
        return SemblanceType.SYMBOL;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SymbolAtom that = (SymbolAtom) o;

        if (!value.equals(that.value)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public String toString() {
        return "SymbolAtom{" + value + '}';
    }
}
