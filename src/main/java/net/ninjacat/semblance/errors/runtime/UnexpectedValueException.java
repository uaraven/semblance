package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 28/02/15.
 */
public class UnexpectedValueException extends SemblanceRuntimeException {

    /**
     * Creates new exception
     *
     * @param value Encountered value.
     */
    public UnexpectedValueException(final LispValue value) {
        super("Unexpected value " + value, Values.getSourceInfo(value));
    }
}
