package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.Symbol;
import net.ninjacat.semblance.utils.Values;
import net.ninjacat.smooth.iterators.Iter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static net.ninjacat.semblance.data.Constants.*;
import static net.ninjacat.semblance.utils.Values.list;

/**
 * Symbol atom.
 */
public class SymbolAtom extends Atom {

    private final String value;
    private final SList hierarchy;

    /**
     * Creates a new symbol atom.
     *
     * @param value value of instantiated atom.
     */
    public SymbolAtom(final String value) {
        this(value, SourceInfo.UNKNOWN);
    }

    /**
     * Creates a new symbol atom.
     *
     * @param value      Value of instantiated atom.
     * @param sourceInfo Source code information.
     */
    public SymbolAtom(final String value, final SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
        hierarchy = buildHierarchy();
    }

    /**
     * Converts boolean into T or F symbol
     *
     * @param boolValue Boolean value
     * @return Either T or F.
     */
    public static SymbolAtom fromBoolean(final boolean boolValue) {
        return boolValue ? TRUE : FALSE;
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

    /**
     * Returns namespace hierarchy of this symbol in form of list.
     * <p/>
     * For symbol {@code list/private/length} following list will be returned:
     * <p/>
     * list(symbol("list"), symbol("private"))
     *
     * @return namespace hierarchy of this symbol.
     */
    public SList getNameHierarchy() {
        return hierarchy;
    }

    private SList buildHierarchy() {
        final String[] splitted = value.split("/");
        final List<String> parts = 1 == splitted.length ?
                new ArrayList<String>() :
                Arrays.asList(splitted).subList(0, value.length() - 1);
        if (parts.isEmpty()) {
            return list(NONE);
        } else {
            return new SList(Iter.of(parts).map(Values.StringToSymbol.INSTANCE).toList());
        }
    }

}
