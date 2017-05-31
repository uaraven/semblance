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

    private static final long serialVersionUID = -8962035894103762183L;

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
        if (token.indexOf('.') >= 0 || token.indexOf('e') >= 0) {
            return new DoubleNumberAtom(Double.parseDouble(token), sourceInfo);
        }
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
     * Calculates negation of this number
     *
     * @return Negation of this number
     */

    public abstract NumberAtom<?> neg();

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

    @Override
    public abstract boolean equals(Object obj);

    @Override
    public abstract int hashCode();

    /**
     * @return Number type. {@link SemblanceNumberType}
     */
    public abstract SemblanceNumberType getNumberType();

    /**
     * @return The value of this atom.
     */
    public abstract T getValue();

    /**
     * Shrinks number to a minimal storage. For example BigIntNumber with a value of 10 will be converted to LongNumber,
     * if there is no need to shrink atom, original one will be returned
     *
     * @return Shrinked number
     */
    public abstract NumberAtom<?> minify();

    @Override
    public String repr() {
        return String.valueOf(getValue());
    }

    @Override
    public String printIt() {
        return repr();
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.INTEGER;
    }

    @Override
    public String toString() {
        return "Number{" + getNumberType() + ":" + getValue() + '}';
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

    /**
     * @return representation of this atom's value as Java long value
     */
    public long longValue() {
        return (Long) convertToLong().getValue();
    }

    /**
     * @return representation of this atom's value as Java BigInteger value
     */
    public BigInteger bigIntValue() {
        return (BigInteger) convertToBigInt().getValue();
    }

    /**
     * @return representation of this atom's value as Java double value
     */
    public double doubleValue() {
        return (Double) convertToDouble().getValue();
    }

    protected abstract NumberAtom<?> expandIfNeeded(NumberAtom other);

    protected abstract NumberAtom<?> convertToBigInt();

    protected abstract NumberAtom<?> convertToLong();

    protected abstract NumberAtom<?> convertToDouble();

}
