package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.Symbol;

/**
 * Created on 24/02/15.
 */
public class SymbolAtom extends Atom {

    public static final SymbolAtom TRUE = new SymbolAtom("T");
    public static final SymbolAtom FALSE = new SymbolAtom("F");

    private final String value;

    public SymbolAtom(final String value) {
        this.value = value;
    }

    public SymbolAtom(final String value, final SourceInfo sourceInfo) {
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

    @SuppressWarnings("all")
    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        final SymbolAtom that = (SymbolAtom) o;

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
