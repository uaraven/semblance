package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 02/03/15.
 */
public class UnsupportedOperationException extends SemblanceRuntimeException {

    public UnsupportedOperationException(String operation, SourceInfo sourceInfo) {
        super("Unsupported operation " + operation, sourceInfo);
    }
}
