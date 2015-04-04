package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;

import javax.annotation.Nonnull;
import java.math.BigInteger;

import static net.ninjacat.semblance.utils.Values.asNumber;

/**
 * Number atom
 */
@SuppressWarnings({"BooleanMethodNameMustStartWithQuestion", "InstanceMethodNamingConvention"})
public abstract class NumberAtom<T> extends Atom {

    protected NumberAtom() {
    }

    protected NumberAtom(final SourceInfo sourceInfo) {
        super(sourceInfo);
    }

    /**
     * Makes a number atom out of string.
     *
     * @param token String value.
     * @return Number atom.
     */
    public static NumberAtom<?> make(final String token) {
        return make(token, SourceInfo.UNKNOWN);
    }

    /**
     * Same as {@link #make}, but includes source code information.
     *
     * @param token      String value.
     * @param sourceInfo Source code information.
     * @return Number atom.
     */
    public static NumberAtom<?> make(final String token, final SourceInfo sourceInfo) {
        final BigInteger bigInteger = new BigInteger(token);
        if (32 >= bigInteger.bitLength()) {
            return new LongNumberAtom(bigInteger.longValue(), sourceInfo);
        } else {
            return new BigIntegerNumberAtom(bigInteger, sourceInfo);
        }
    }

    /**
     * Adds this number and a parameter.
     *
     * @param other number to add.
     * @return New instance of number atom with value equal to sum of this number and other number.
     */
    public abstract NumberAtom<?> add(NumberAtom<?> other);

    /**
     * Subtracts other number from this number.
     *
     * @param other number to subtract.
     * @return New instance of number atom with value equal to difference of this number and other number.
     */
    public abstract NumberAtom<?> sub(NumberAtom<?> other);

    /**
     * Divides this number by a parameter.
     *
     * @param other number to divide by.
     * @return New instance of number atom with value equal to quotient of this number and other number.
     */
    public abstract NumberAtom<?> div(NumberAtom<?> other);

    /**
     * Divides this number and a parameter by modulo. Both numbers must be integers.
     *
     * @param other number to divide by.
     * @return New instance of number atom with value equal to remainder of this number and other number.
     */
    public abstract NumberAtom<?> mod(NumberAtom<?> other);

    /**
     * Multiplicates this number by a parameter.
     *
     * @param other number to multiply by.
     * @return New instance of number atom with value equal to multiplication result of this number
     * and other number.
     */
    public abstract NumberAtom<?> mul(NumberAtom<?> other);

    /**
     * @return true if this number represents infinity
     */
    public abstract boolean isInfinity();

    /**
     * Checks if this number is less then other.
     *
     * @param other Other number.
     * @return true or false
     */
    public abstract boolean lt(NumberAtom<?> other);

    /**
     * Checks if this number is equal to other.
     *
     * @param other Other number.
     * @return true of false
     */
    public abstract boolean eq(NumberAtom<?> other);

    /**
     * Checks if this number is greater then other.
     *
     * @param other Other number.
     * @return true or false
     */
    public abstract boolean gt(NumberAtom<?> other);

    /**
     * @return Number type. {@link net.ninjacat.semblance.data.SemblanceNumberType}
     */
    public abstract SemblanceNumberType getNumberType();

    /**
     * @return The value of this atom.
     */
    public abstract T getValue();

    @Override
    public String repr() {
        return String.valueOf(getValue());
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.INTEGER;
    }

    @Override
    public String toString() {
        return "NumberAtom{" + getValue() + '}';
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        if (other.getType() == SemblanceType.INTEGER) {
            final NumberAtom<?> number = asNumber(other);
            if (eq(number)) {
                return 0;
            } else if (gt(number)) {
                return 1;
            } else {
                return -1;
            }
        } else {
            throw new ClassCastException(String.format("%s is not compatible with %s", getType(), other.getType()));
        }
    }

    protected abstract NumberAtom<?> expandIfNeeded(NumberAtom other);

    protected abstract NumberAtom<?> convertToBigInt();

    protected abstract NumberAtom<?> convertToLong();

    protected abstract NumberAtom<?> convertToDouble();
}
