package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class ValueExpectedException extends SemblanceRuntimeException {

    /**
     * Creates a new exception
     *
     * @param sourceInfo Source code position where value was expected.
     */
    public ValueExpectedException(final SourceInfo sourceInfo) {
        super("Value expected", sourceInfo);
    }
}
