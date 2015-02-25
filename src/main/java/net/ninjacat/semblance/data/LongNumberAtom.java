package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Basic representation of number atom, used for number that are integer and fit into 31 bit + 1 sign bit.
 */
public class LongNumberAtom extends NumberAtom<Long> {

    private final long value;

    public LongNumberAtom(long value) {
        super();
        this.value = value;
    }

    public LongNumberAtom(long value, SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    @Override
    public SemblanceNumberType getNumberType() {
        return SemblanceNumberType.LONG;
    }

    @Override
    public Long getValue() {
        return value;
    }

    @Override
    public Long asJavaObject() {
        return getValue();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        LongNumberAtom that = (LongNumberAtom) o;

        if (value != that.value) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return (int) (value ^ (value >>> 32));
    }
}
