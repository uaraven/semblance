package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Number atom
 */
public class NumberAtom extends Atom {

    private final long value;

    public NumberAtom(long value) {
        this.value = value;
    }

    public NumberAtom(long value, SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    @Override
    public String repr() {
        return String.valueOf(value);
    }

    @Override
    public Long asJavaObject() {
        return value;
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.NUMBER;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        NumberAtom that = (NumberAtom) o;

        if (value != that.value) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return (int) (value ^ (value >>> 32));
    }

    @Override
    public String toString() {
        return "NumberAtom{" + value + '}';
    }
}
