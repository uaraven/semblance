package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Exception thrown when invalid access to a collection happens
 */
public class CollectionException extends SemblanceRuntimeException {
    /**
     * Creates new exception.
     *
     * @param message    Exception message.
     * @param sourceInfo Source code information.
     */
    public CollectionException(final String message, final SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }

    /**
     * Creates new exception.
     *
     * @param message    Exception message.
     * @param sourceInfo Source code information.
     * @param throwable  Cause of the exception.
     */
    public CollectionException(final String message, final SourceInfo sourceInfo,
                               final IllegalArgumentException throwable) {
        super(message, sourceInfo, throwable);
    }
}
