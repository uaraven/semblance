package net.ninjacat.semblance.utils;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.smooth.functions.Func;

/**
 * Created on 24/02/15.
 */
public enum ValueToString implements Func<String, LispValue> {
    REPR;

    @Override
    public String apply(LispValue lispValue) {
        return lispValue.repr();
    }
}
