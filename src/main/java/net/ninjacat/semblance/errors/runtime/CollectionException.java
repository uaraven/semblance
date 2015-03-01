package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Exception thrown when invalid access to a collection happens
 */
public class CollectionException extends SemblanceRuntimeException {
    public CollectionException(String message, SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }

    public CollectionException(String s, SourceInfo sourceInfo, IllegalArgumentException ex) {
        super(s, sourceInfo, ex);
    }
}
