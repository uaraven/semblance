package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

/**
 * Big Integer representation of number atom. Used for numbers that does not fit into long
 */
public class BigIntegerNumberAtom extends NumberAtom<BigInteger> {

    private static final BigInteger MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);
    private static final BigInteger MIN_LONG = BigInteger.valueOf(Long.MIN_VALUE);

    private final BigInteger value;

    public BigIntegerNumberAtom(BigInteger value) {
        this.value = value;
    }

    public BigIntegerNumberAtom(BigInteger value, SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    private static NumberAtom<?> make(BigInteger value) {
        if (value.compareTo(MIN_LONG) > 0 && value.compareTo(MAX_LONG) < 0) {
            return new LongNumberAtom(value.longValue());
        } else {
            return new BigIntegerNumberAtom(value);
        }
    }

    @Override
    public NumberAtom<?> add(NumberAtom<?> other) {
        NumberAtom<?> self = expandIfNeeded(other);
        NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self.getNumberType() == SemblanceNumberType.BIG) {
            return make(value.add((BigInteger) oth.getValue()));
        } else {
            return self.add(other);
        }
    }

    @Override
    public NumberAtom<?> sub(NumberAtom<?> other) {
        NumberAtom<?> self = expandIfNeeded(other);
        NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self == this) {
            return make(value.subtract((BigInteger) oth.getValue()));
        } else {
            return self.sub(other);
        }
    }

    @Override
    public NumberAtom<?> div(NumberAtom<?> other) {
        NumberAtom<?> self = expandIfNeeded(other);
        NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self == this) {
            return make(value.divide((BigInteger) oth.getValue()));
        } else {
            return self.div(other);
        }
    }

    @Override
    public NumberAtom<?> mod(NumberAtom<?> other) {
        NumberAtom<?> self = expandIfNeeded(other);
        NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self == this) {
            return make(value.divide((BigInteger) oth.getValue()));
        } else {
            return self.div(other);
        }
    }

    @Override
    public NumberAtom<?> mul(NumberAtom<?> other) {
        NumberAtom<?> self = expandIfNeeded(other);
        NumberAtom<?> oth = other.expandIfNeeded(this);
        if (self == this) {
            return make(value.multiply((BigInteger) oth.getValue()));
        } else {
            return self.mul(other);
        }
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
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BigIntegerNumberAtom that = (BigIntegerNumberAtom) o;

        if (!value.equals(that.value)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
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
    protected NumberAtom expandIfNeeded(NumberAtom other) {
        if (other.getNumberType() == SemblanceNumberType.DOUBLE) {
            return convertToDouble();
        } else {
            return this;
        }
    }
}
