package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

/**
 * Basic representation of number atom, used for number that are integer and fit into 31 bit + 1 sign bit.
 */
public class LongNumberAtom extends NumberAtom<Long> {

    private final long value;

    /**
     * Creates new number atom.
     *
     * @param value Value of the atom.
     */
    public LongNumberAtom(final long value) {
        super();
        this.value = value;
    }

    /**
     * Creates new number atom.
     *
     * @param value      Value of the atom.
     * @param sourceInfo Source code information.
     */
    public LongNumberAtom(final long value, final SourceInfo sourceInfo) {
        super(sourceInfo);
        this.value = value;
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> add(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            if (willOverflow(value, (Long) other.getValue())) {
                return convertToBigInt().add(other.convertToBigInt());
            } else {
                return new LongNumberAtom(value + (Long) other.getValue());
            }
        }
        final NumberAtom self = expandIfNeeded(other);
        return self.add(other);
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> sub(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            if (willOverflow(value, (Long) other.getValue())) {
                return convertToBigInt().sub(other.convertToBigInt());
            } else {
                return new LongNumberAtom(value - (Long) other.getValue());
            }
        }
        final NumberAtom self = expandIfNeeded(other);
        return self.sub(other);
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> div(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            return new LongNumberAtom(value / (Long) other.getValue());
        }
        final NumberAtom self = expandIfNeeded(other);
        return self.div(other);
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> mod(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            return new LongNumberAtom(value % (Long) other.getValue());
        }
        final NumberAtom self = expandIfNeeded(other);
        return self.mod(other);
    }

    @SuppressWarnings("unchecked")
    @Override
    public NumberAtom<?> mul(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            if (willOverflow(value, (Long) other.getValue())) {
                return convertToBigInt().mul(other.convertToBigInt());
            } else {
                return new LongNumberAtom(value * (Long) other.getValue());
            }
        }
        final NumberAtom self = expandIfNeeded(other);
        return self.mul(other);
    }

    @Override
    public boolean isInfinity() {
        return false;
    }

    @Override
    public boolean eq(final NumberAtom other) {
        if (getNumberType() == other.getNumberType()) {
            return 0 == Long.compare(value, (Long) other.getValue());
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        return self.eq(other);
    }

    @Override
    public boolean lt(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            return 0 < Long.compare(value, (Long) other.getValue());
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        return self.eq(other);
    }

    @Override
    public boolean gt(final NumberAtom<?> other) {
        if (getNumberType() == other.getNumberType()) {
            return 0 > Long.compare(value, (Long) other.getValue());
        }
        final NumberAtom<?> self = expandIfNeeded(other);
        return self.eq(other);
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

    @SuppressWarnings("all")
    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        final LongNumberAtom that = (LongNumberAtom) o;

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
    protected NumberAtom expandIfNeeded(final NumberAtom other) {
        if (SemblanceNumberType.DOUBLE == other.getNumberType()) {
            return convertToDouble();
        } else if (SemblanceNumberType.BIG == other.getNumberType()) {
            return convertToBigInt();
        } else {
            return this;
        }
    }

    private boolean willOverflow(final long l1, final long l2) {
        final long maximum = Long.signum(l1) == Long.signum(l2) ? Long.MAX_VALUE : Long.MIN_VALUE;

        return (0 != l1 && (0 < l2 && l2 > maximum / l1 || 0 > l2 && l2 < maximum / l1));
    }
}
