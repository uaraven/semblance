package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 01/03/15.
 */
public class ParameterException extends SemblanceRuntimeException {
    /**
     * Creates new exception.
     *
     * @param message    Message.
     * @param sourceInfo Source information.
     */
    public ParameterException(final String message, final SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }
}
