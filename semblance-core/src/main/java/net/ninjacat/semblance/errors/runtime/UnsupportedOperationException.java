package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 02/03/15.
 */
public class UnsupportedOperationException extends SemblanceRuntimeException {

    private static final long serialVersionUID = -5506451563225214064L;

    /**
     * Creates new exception.
     *
     * @param operation  Encountered operation.
     * @param sourceInfo Source code information.
     */
    public UnsupportedOperationException(final String operation, final SourceInfo sourceInfo) {
        super("Unsupported operation " + operation, sourceInfo);
    }
}
