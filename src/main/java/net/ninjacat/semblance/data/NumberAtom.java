package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

/**
 * Number atom
 */
public abstract class NumberAtom<T> extends Atom {

    public NumberAtom() {
    }

    public NumberAtom(final SourceInfo sourceInfo) {
        super(sourceInfo);
    }

    public static NumberAtom<?> make(final String token) {
        return make(token, SourceInfo.UNKNOWN);
    }

    public static NumberAtom<?> make(final String token, final SourceInfo sourceInfo) {
        final BigInteger bigInteger = new BigInteger(token);
        if (32 >= bigInteger.bitLength()) {
            return new LongNumberAtom(bigInteger.longValue(), sourceInfo);
        } else {
            return new BigIntegerNumberAtom(bigInteger, sourceInfo);
        }
    }

    public abstract NumberAtom<?> add(NumberAtom<?> other);

    public abstract NumberAtom<?> sub(NumberAtom<?> other);

    public abstract NumberAtom<?> div(NumberAtom<?> other);

    public abstract NumberAtom<?> mod(NumberAtom<?> other);

    public abstract NumberAtom<?> mul(NumberAtom<?> other);

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

    public abstract SemblanceNumberType getNumberType();

    public abstract T getValue();

    protected abstract NumberAtom<?> expandIfNeeded(NumberAtom other);

    protected abstract NumberAtom<?> convertToBigInt();

    protected abstract NumberAtom<?> convertToLong();

    protected abstract NumberAtom<?> convertToDouble();
}
