package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public class ValidationException extends SemblanceRuntimeException {

    private static final long serialVersionUID = -1565970832522918866L;

    /**
     * Creates a new instance of ValidationException
     *
     * @param message    Error message
     * @param sourceInfo Source code information
     */
    public ValidationException(final String message, final SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }

    /**
     * Creates a new instance of ValidationException
     *
     * @param message    Error message
     * @param sourceInfo Source code information
     * @param cause      original cause of the exception
     */
    public ValidationException(final String message, final SourceInfo sourceInfo, final Throwable cause) {
        super(message, sourceInfo, cause);
    }
}
