package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.java.Symbol;

import javax.annotation.Nonnull;

import static net.ninjacat.semblance.data.Constants.FALSE;
import static net.ninjacat.semblance.data.Constants.TRUE;
import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Symbol atom.
 */
public class SymbolAtom extends Atom {

    private static final long serialVersionUID = 2760121266739038022L;

    private final String value;
    private final SymbolAtom localName;
    private final SymbolAtom namespace;

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
        namespace = extractNamespace();
        localName = extractLocalName();
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

    /**
     * @return localname of namespaced symbol
     */
    public SymbolAtom getLocalName() {
        return localName;
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
        return "Symbol{" + value + '}';
    }

    /**
     * Returns namespace namespace of this symbol in form of list.
     * <p/>
     * For symbol {@code list/private/length} following list will be returned:
     * <p/>
     * list(symbol("list"), symbol("private"))
     *
     * @return namespace namespace of this symbol.
     */
    public SymbolAtom getNamespace() {
        return namespace;
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        if (other.getClass().equals(getClass())) {
            return ((SymbolAtom) other).value.compareTo(value);
        } else {
            throw new TypeMismatchException(getType(), other, SourceInfo.UNKNOWN);
        }
    }

    private SymbolAtom extractNamespace() {
        final int splitterIndex = value.lastIndexOf('/');
        if (splitterIndex >= 0) {
            final String nsName = value.substring(0, splitterIndex);
            if (!nsName.isEmpty()) {
                return symbol(nsName);
            }
        }
        return Constants.NONE;
    }

    private SymbolAtom extractLocalName() {
        if (null == namespace || Constants.NONE.equals(namespace)) {
            return this;
        }
        final int splitterIndex = value.lastIndexOf('/');
        if (value.length() > 2 && splitterIndex >= 0) {
            return symbol(value.substring(splitterIndex + 1));
        } else {
            return this;
        }
    }
}
