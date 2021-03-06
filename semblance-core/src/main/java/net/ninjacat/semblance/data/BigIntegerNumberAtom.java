package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

/**
 * Big Integer representation of number atom. Used for numbers that does not fit into long
 */
@SuppressWarnings("ObjectEquality")
public class BigIntegerNumberAtom extends NumberAtom<BigInteger> {

    private static final long serialVersionUID = -1773564823733513827L;

    private static final BigInteger MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);
    private static final BigInteger MIN_LONG = BigInteger.valueOf(Long.MIN_VALUE);

    private final BigInteger value;

    /**
     * Creates new instance of Number atom backed by BigInteger.
     *
     * @param value Value of the number atom.
     */
    public BigIntegerNumberAtom(final BigInteger value) {
        this.value = value;
    }

    /**
     * Creates new instance of Number atom backed by BigInteger.
     *
     * @param value      Value of the number atom.
     * @param sourceInfo Source code information.
     */
    public BigIntegerNumberAtom(final BigInteger value, final SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    @Override
    public NumberAtom<?> add(final NumberAtom<?> other) {
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> oth = other.expandIfNeeded(this);
        if (SemblanceNumberType.BIG == self.getNumberType()) {
            return make(value.add((BigInteger) oth.getValue()));
        } else {
            return self.add(other);
        }
    }

    @Override
    public NumberAtom<?> sub(final NumberAtom<?> other) {
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self == this) {
            return make(value.subtract((BigInteger) oth.getValue()));
        } else {
            return self.sub(other);
        }
    }

    @Override
    public NumberAtom<?> div(final NumberAtom<?> other) {
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self == this) {
            return make(value.divide((BigInteger) oth.getValue()));
        } else {
            return self.div(other);
        }
    }

    @Override
    public NumberAtom<?> mod(final NumberAtom<?> other) {
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self == this) {
            return make(value.divide((BigInteger) oth.getValue()));
        } else {
            return self.div(other);
        }
    }

    @Override
    public NumberAtom<?> mul(final NumberAtom<?> other) {
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self == this) {
            return make(value.multiply((BigInteger) oth.getValue()));
        } else {
            return self.mul(other);
        }
    }

    @Override
    public NumberAtom<?> neg() {
        return new BigIntegerNumberAtom(value.negate());
    }

    @Override
    public boolean isInfinity() {
        return false;
    }

    @SuppressWarnings("InstanceMethodNamingConvention")
    @Override
    public boolean eq(final NumberAtom other) {
        if (getNumberType() == other.getNumberType()) {
            return 0 == value.compareTo((BigInteger) other.getValue());
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> otExp = other.expandIfNeeded(self);
        return self.eq(otExp);
    }

    @SuppressWarnings("InstanceMethodNamingConvention")
    @Override
    public boolean lt(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            return value.compareTo((BigInteger) other.getValue()) < 0;
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> otExp = other.expandIfNeeded(self);
        return self.lt(otExp);
    }

    @SuppressWarnings("InstanceMethodNamingConvention")
    @Override
    public boolean gt(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            return value.compareTo((BigInteger) other.getValue()) > 0;
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        final NumberAtom<?> otExp = other.expandIfNeeded(self);
        return self.gt(otExp);
    }

    @Override
    public BigInteger asJavaObject() {
        return value;
    }

    @Override
    public SemblanceNumberType getNumberType() {
        return SemblanceNumberType.BIG;
    }

    @Override
    public BigInteger getValue() {
        return value;
    }

    @Override
    @SuppressWarnings("all")
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (null == o || getClass() != o.getClass()) return false;

        final BigIntegerNumberAtom that = (BigIntegerNumberAtom) o;

        if (!value.equals(that.value)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public NumberAtom<?> minify() {
        if (getValue().compareTo(MAX_LONG) <= 0 && getValue().compareTo(MIN_LONG) >= 0) {
            return convertToLong();
        } else {
            return this;
        }
    }

    private static NumberAtom<?> make(final BigInteger value) {
        if (0 < value.compareTo(MIN_LONG) && 0 > value.compareTo(MAX_LONG)) {
            return new LongNumberAtom(value.longValue());
        } else {
            return new BigIntegerNumberAtom(value);
        }
    }

    @Override
    protected NumberAtom<?> convertToBigInt() {
        return this;
    }

    @Override
    protected NumberAtom<?> convertToLong() {
        return new LongNumberAtom(value.longValue(), getSourceInfo());
    }

    @Override
    protected NumberAtom<?> convertToDouble() {
        return new DoubleNumberAtom(value.doubleValue());
    }

    @Override
    protected NumberAtom expandIfNeeded(final NumberAtom other) {
        if (SemblanceNumberType.DOUBLE == other.getNumberType()) {
            return convertToDouble();
        } else {
            return this;
        }
    }
}
