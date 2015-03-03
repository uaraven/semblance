package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

/**
 * Floating point representation of number atom
 */
public class DoubleNumberAtom extends NumberAtom {

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
    public SemblanceNumberType getNumberType() {
        return SemblanceNumberType.DOUBLE;
    }

    @Override
    public Double getValue() {
        return value;
    }

    @Override
    public NumberAtom<?> add(NumberAtom other) {
        return new DoubleNumberAtom(value + (Double) expandOther(other).getValue());
    }

    @Override
    public NumberAtom<?> sub(NumberAtom other) {
        return new DoubleNumberAtom(value - (Double) expandOther(other).getValue());
    }

    @Override
    public NumberAtom<?> div(NumberAtom other) {
        return new DoubleNumberAtom(value / (Double) expandOther(other).getValue());
    }

    @Override
    public NumberAtom<?> mod(NumberAtom other) {
        throw new net.ninjacat.semblance.errors.runtime.UnsupportedOperationException("%", getSourceInfo());
    }

    @Override
    public NumberAtom<?> mul(NumberAtom other) {
        return new DoubleNumberAtom(value * (Double) expandOther(other).getValue());
    }

    @Override
    public NumberAtom<?> fdiv(NumberAtom other) {
        return new DoubleNumberAtom(value / (Double) expandOther(other).getValue());
    }

    @Override
    public String repr() {
        return String.valueOf(value);
    }

    @Override
    protected NumberAtom expandIfNeeded(NumberAtom other) {
        return this;
    }

    @Override
    protected NumberAtom<?> convertToBigInt() {
        return new BigIntegerNumberAtom(BigInteger.valueOf((long) value));
    }

    @Override
    protected NumberAtom<?> convertToLong() {
        return new LongNumberAtom((long) value);
    }

    @Override
    protected NumberAtom<?> convertToDouble() {
        return this;
    }

    private NumberAtom expandOther(NumberAtom other) {
        if (other.getNumberType() != SemblanceNumberType.DOUBLE) {
            return other.expandIfNeeded(this);
        } else {
            return other;
        }
    }
}
