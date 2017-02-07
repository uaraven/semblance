package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Basic class for runtime errors
 */
public class SemblanceRuntimeException extends RuntimeException {
    private static final long serialVersionUID = 4909074721888011066L;

    private final String message;
    private final SourceInfo sourceInfo;

    /**
     * Creates new exception.
     *
     * @param message    Error message.
     * @param sourceInfo Source code information.
     */
    public SemblanceRuntimeException(final String message, final SourceInfo sourceInfo) {
        super(message + " at " + sourceInfo);
        this.message = message;
        this.sourceInfo = sourceInfo;
    }

    /**
     * Creates new exception.
     *
     * @param message    Error message.
     * @param sourceInfo Source code information.
     * @param cause      Cause of the error.
     */
    public SemblanceRuntimeException(final String message, final SourceInfo sourceInfo, final Throwable cause) {
        super(message + " at " + sourceInfo, cause);
        this.message = message;
        this.sourceInfo = sourceInfo;
    }

    @Override
    public String getMessage() {
        return message;
    }

    /**
     * @return Source code information.
     */
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }
}
