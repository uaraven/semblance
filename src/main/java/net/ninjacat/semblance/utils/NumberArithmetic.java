package net.ninjacat.semblance.utils;

import net.ninjacat.semblance.data.*;

/**
 * Utilities to help with different number representations
 */
public final class NumberArithmetic {
    private NumberArithmetic() {
    }

    public static NumberAtom simplify(NumberAtom number) {
        if (number.getNumberType() == SemblanceNumberType.BIG) {
            return simplifyBig((BigIntegerNumberAtom) number);
        } else if (number.getNumberType() == SemblanceNumberType.DOUBLE) {
            return simplifyDouble((DoubleNumberAtom) number);
        } else {
            return number;
        }
    }

    private static NumberAtom simplifyBig(BigIntegerNumberAtom number) {
        return number.canBeLong() ? new LongNumberAtom(number.getValue().longValue(), number.getSourceInfo()) : number;
    }

    private static NumberAtom simplifyDouble(DoubleNumberAtom number) {
        return number.canBeLong() ? new LongNumberAtom(number.getValue().longValue(), number.getSourceInfo()) : number;
    }
}
