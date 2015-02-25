package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class StringAtom extends Atom {

    private final String value;

    public StringAtom(String value) {
        this.value = value;
    }

    public StringAtom(String value, SourceInfo sourceInfo) {
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        StringAtom that = (StringAtom) o;

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
}
