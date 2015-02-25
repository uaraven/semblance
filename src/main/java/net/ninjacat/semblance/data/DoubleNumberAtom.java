package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Floating point representation of number atom
 */
public class DoubleNumberAtom extends Atom {

    private double value;

    public DoubleNumberAtom(double value) {
        this.value = value;
    }

    public DoubleNumberAtom(Double value, SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    @Override
    public Double asJavaObject() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DoubleNumberAtom that = (DoubleNumberAtom) o;

        if (Double.compare(that.value, value) != 0) return false;

        return true;
    }

    @Override
    public int hashCode() {
        long temp = Double.doubleToLongBits(value);
        return (int) (temp ^ (temp >>> 32));
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.FLOATIG_POINT;
    }

    @Override
    public String repr() {
        return String.valueOf(value);
    }
}
