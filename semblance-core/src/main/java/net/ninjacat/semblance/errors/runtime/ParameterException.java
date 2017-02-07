package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

public class ParameterException extends SemblanceRuntimeException {
    private static final long serialVersionUID = 8144585859060705534L;

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
