package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;

import java.math.BigInteger;

/**
 * Big Integer representation of number atom. Used for numbers that does not fit into long
 */
public class BigIntegerNumberAtom extends NumberAtom<BigInteger> {

    private final BigInteger value;

    public BigIntegerNumberAtom(BigInteger value) {
        this.value = value;
    }

    public BigIntegerNumberAtom(SourceInfo sourceInfo, BigInteger value) {
        super(sourceInfo);
        this.value = value;
    }

    public boolean canBeLong() {
        return value.bitLength() < 32;
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
}
