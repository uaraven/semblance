package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Exception thrown when invalid access to a collection happens
 */
public class CollectionIndexOutOfBoundsException extends SemblanceRuntimeException {
    public CollectionIndexOutOfBoundsException(long index, long size, SourceInfo sourceInfo) {
        super(String.format("Index %d is out of bounds, size %d", index, size), sourceInfo);
    }

    public CollectionIndexOutOfBoundsException(long index, long size, SourceInfo sourceInfo, Throwable cause) {
        super(String.format("Index %d is out of bounds, size %d", index, size), sourceInfo, cause);
    }
}
