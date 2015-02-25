package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

/**
 * Number atom
 */
public abstract class NumberAtom<T> extends Atom {

    public NumberAtom() {
    }

    public NumberAtom(SourceInfo sourceInfo) {
        super(sourceInfo);
    }

    public static NumberAtom<?> make(String token) {
        if (token.contains(".") || token.contains("e")) { // this is double
            return new DoubleNumberAtom(Double.parseDouble(token));
        }
        BigInteger bigInteger = new BigInteger(token);
        if (bigInteger.bitLength() <= 32) {
            return new LongNumberAtom(bigInteger.longValue());
        } else {
            return new BigIntegerNumberAtom(bigInteger);
        }
    }

    @Override
    public String repr() {
        return String.valueOf(getValue());
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.NUMBER;
    }

    @Override
    public String toString() {
        return "NumberAtom{" + getValue() + '}';
    }

    public abstract SemblanceNumberType getNumberType();

    public abstract T getValue();
}
