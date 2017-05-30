package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Thrown when exception happens in Semblance interpreter or library which is not caused by program that runs
 */
public class InternalSemblanceError extends SemblanceRuntimeException {
    private static final long serialVersionUID = 7668473325436316281L;

    /**
     * Creates new instance of exception with error message
     *
     * @param message Error message
     */
    public InternalSemblanceError(final String message) {
        super(message, SourceInfo.UNKNOWN);
    }

    /**
     * Creates new instance of exception with error message and original cause
     *
     * @param message Error message
     * @param cause   Original exception
     */
    public InternalSemblanceError(final String message, final Throwable cause) {
        super(message, SourceInfo.UNKNOWN, cause);
    }
}
