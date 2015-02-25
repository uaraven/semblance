package net.ninjacat.semblance.utils;

import net.ninjacat.semblance.data.BigIntegerNumberAtom;
import net.ninjacat.semblance.data.LongNumberAtom;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.SemblanceIntType;

/**
 * Utilities to help with different number representations
 */
public final class NumberArithmetic {
    private NumberArithmetic() {
    }

    public static NumberAtom simplify(NumberAtom number) {
        if (number.getNumberType() == SemblanceIntType.BIG) {
            return simplifyBig((BigIntegerNumberAtom) number);
        } else {
            return number;
        }
    }

    private static NumberAtom simplifyBig(BigIntegerNumberAtom number) {
        return number.canBeLong() ? new LongNumberAtom(number.getValue().longValue(), number.getSourceInfo()) : number;
    }

}
