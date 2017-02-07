package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class ValueExpectedException extends SemblanceRuntimeException {

    private static final long serialVersionUID = -8416740804559886930L;

    /**
     * Creates a new exception
     *
     * @param sourceInfo Source code position where value was expected.
     */
    public ValueExpectedException(final SourceInfo sourceInfo) {
        super("Value expected", sourceInfo);
    }
}
