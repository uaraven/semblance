package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

/**
 * Floating point representation of number atom
 */
public class DoubleNumberAtom extends NumberAtom {

    private final double value;

    /**
     * Creates new Double Atom.
     *
     * @param value Value of the new atom.
     */
    public DoubleNumberAtom(final double value) {
        this.value = value;
    }

    /**
     * Creates new Double Atom.
     *
     * @param value      Value of the new atom.
     * @param sourceInfo Source information of the new atom.
     */
    public DoubleNumberAtom(final Double value, final SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    @Override
    public Double asJavaObject() {
        return value;
    }

    @SuppressWarnings("all")
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
        final long temp = Double.doubleToLongBits(value);
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
    public NumberAtom<?> add(final NumberAtom other) {
        return new DoubleNumberAtom(value + (Double) expandOther(other).getValue());
    }

    @Override
    public NumberAtom<?> sub(final NumberAtom other) {
        return new DoubleNumberAtom(value - (Double) expandOther(other).getValue());
    }

    @Override
    public NumberAtom<?> div(final NumberAtom other) {
        return new DoubleNumberAtom(value / (Double) expandOther(other).getValue());
    }

    @Override
    public NumberAtom<?> mod(final NumberAtom other) {
        throw new net.ninjacat.semblance.errors.runtime.UnsupportedOperationException("%", getSourceInfo());
    }

    @Override
    public NumberAtom<?> mul(final NumberAtom other) {
        return new DoubleNumberAtom(value * (Double) expandOther(other).getValue());
    }

    @Override
    public boolean eq(final NumberAtom other) {
        if (getNumberType() == other.getNumberType()) {
            return 0 == Double.compare(value, (Double) other.getValue());
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> otExp = other.expandIfNeeded(self);
        return self.eq(otExp);
    }

    @Override
    public boolean lt(final NumberAtom other) {
        if (getNumberType() == other.getNumberType()) {
            return Double.compare(value, (Double) other.getValue()) < 0;
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> otExp = other.expandIfNeeded(self);
        return self.lt(otExp);
    }

    @Override
    public boolean gt(final NumberAtom other) {
        if (getNumberType() == other.getNumberType()) {
            return Double.compare(value, (Double) other.getValue()) > 0;
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> otExp = other.expandIfNeeded(self);
        return self.gt(otExp);
    }

    @Override
    public boolean isInfinity() {
        return Double.isInfinite(value);
    }

    @Override
    public String repr() {
        return String.valueOf(value);
    }

    @Override
    protected NumberAtom expandIfNeeded(final NumberAtom other) {
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

    private NumberAtom expandOther(final NumberAtom other) {
        if (SemblanceNumberType.DOUBLE != other.getNumberType()) {
            return other.expandIfNeeded(this);
        } else {
            return other;
        }
    }

}
