package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

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

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> add(NumberAtom<?> other) {
        NumberAtom self = expandIfNeeded(other);
        if (self == this) {
            if (willOverflow(value, (Long) other.getValue())) {
                return convertToBigInt().add(other.convertToBigInt());
            } else {
                return new LongNumberAtom(value + (Long) other.getValue());
            }
        } else {
            return self.add(other);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> sub(NumberAtom<?> other) {
        NumberAtom self = expandIfNeeded(other);
        if (self == this) {
            if (willOverflow(value, (Long) other.getValue())) {
                return convertToBigInt().sub(other.convertToBigInt());
            } else {
                return new LongNumberAtom(value - (Long) other.getValue());
            }

        } else {
            return self.sub(other);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> div(NumberAtom<?> other) {
        NumberAtom self = expandIfNeeded(other);
        if (self == this) {
            return new LongNumberAtom(value / (Long) other.getValue());
        } else {
            return self.div(other);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> mod(NumberAtom<?> other) {
        NumberAtom self = expandIfNeeded(other);
        if (self == this) {
            return new LongNumberAtom(value % (Long) other.getValue());
        } else {
            return self.mod(other);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> mul(NumberAtom<?> other) {
        NumberAtom self = expandIfNeeded(other);
        if (self == this) {
            if (willOverflow(value, (Long) other.getValue())) {
                return convertToBigInt().mul(other.convertToBigInt());
            } else {
                return new LongNumberAtom(value * (Long) other.getValue());
            }
        } else {
            return self.mul(other);
        }
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

    @Override
    protected NumberAtom<?> convertToBigInt() {
        return new BigIntegerNumberAtom(new BigInteger(String.valueOf(value)));
    }

    @Override
    protected NumberAtom<?> convertToLong() {
        return this;
    }

    @Override
    protected NumberAtom<?> convertToDouble() {
        return new DoubleNumberAtom((double) value);
    }

    @Override
    protected NumberAtom expandIfNeeded(NumberAtom other) {
        if (other.getNumberType() == SemblanceNumberType.DOUBLE) {
            return convertToDouble();
        } else if (other.getNumberType() == SemblanceNumberType.BIG) {
            return convertToBigInt();
        } else {
            return this;
        }
    }

    private boolean willOverflow(long a, long b) {
        long maximum = Long.signum(a) == Long.signum(b) ? Long.MAX_VALUE : Long.MIN_VALUE;

        return (a != 0 && (b > 0 && b > maximum / a || b < 0 && b < maximum / a));
    }
}
