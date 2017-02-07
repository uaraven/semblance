package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Exception thrown when invalid access to a collection happens
 */
public class CollectionIndexOutOfBoundsException extends SemblanceRuntimeException {

    private static final String MESSAGE = "Index %d is out of bounds, size %d";
    private static final long serialVersionUID = -894785072721261847L;

    /**
     * Create new Out of bounds exception.
     *
     * @param index      Index access to which caused exception.
     * @param size       Size of the collection.
     * @param sourceInfo Source code information.
     */
    public CollectionIndexOutOfBoundsException(final long index, final long size, final SourceInfo sourceInfo) {
        super(String.format(MESSAGE, index, size), sourceInfo);
    }

    /**
     * Create new Out of bounds exception.
     *
     * @param index      Index access to which caused exception.
     * @param size       Size of the collection.
     * @param sourceInfo Source code information.
     * @param cause      Parent exception cause.
     */
    public CollectionIndexOutOfBoundsException(final long index, final long size,
                                               final SourceInfo sourceInfo, final Throwable cause) {
        super(String.format(MESSAGE, index, size), sourceInfo, cause);
    }
}
