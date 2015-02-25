package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Floating point representation of number atom
 */
public class DoubleNumberAtom extends NumberAtom<Double> {

    private double value;

    public DoubleNumberAtom(double value) {
        this.value = value;
    }

    public DoubleNumberAtom(Double value, SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    public boolean canBeLong() {
        return Math.floor(value) == value && !Double.isInfinite(value);
    }

    @Override
    public Double asJavaObject() {
        return getValue();
    }

    @Override
    public SemblanceNumberType getNumberType() {
        return SemblanceNumberType.DOUBLE;
    }

    @Override
    public Double getValue() {
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
}
