package net.ninjacat.semblance.data.special;

import net.ninjacat.semblance.data.LispValue;

/**
 * Base interface for interal wrapped values.
 */
public interface WrappedValue {
    /**
     * @return unwrapped value.
     */
    LispValue getValue();
}
