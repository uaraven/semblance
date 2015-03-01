package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class ValueExpectedException extends SemblanceRuntimeException {

    public ValueExpectedException(SourceInfo sourceInfo) {
        super("Value expected", sourceInfo);
    }
}
