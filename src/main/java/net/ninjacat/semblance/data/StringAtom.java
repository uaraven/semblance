package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;

import javax.annotation.Nonnull;

/**
 * Created on 24/02/15.
 */
public class StringAtom extends Atom {

    private final String value;

    /**
     * Creates a new string atom.
     *
     * @param value Value of the new string atom.
     */
    public StringAtom(final String value) {
        this.value = value;
    }

    /**
     * Creates a new string atom.
     *
     * @param value      Value of the new string atom.
     * @param sourceInfo Source code information for the atom.
     */
    public StringAtom(final String value, final SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    @Override
    public String repr() {
        return "\"" + value + "\"";
    }

    @Override
    public String asJavaObject() {
        return value;
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.STRING;
    }

    @SuppressWarnings("all")
    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (null == o || getClass() != o.getClass()) return false;

        final StringAtom that = (StringAtom) o;

        if (!value.equals(that.value)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public String toString() {
        return "StringAtom{\'" + value + "\'}";
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        if (other.getClass().equals(getClass())) {
            return value.compareTo(((StringAtom) other).value);
        } else {
            throw new ClassCastException(String.format("%s is not compatible with %s", getType(), other.getType()));
        }
    }
}
