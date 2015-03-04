package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class TypeMismatchException extends SemblanceRuntimeException {
    public TypeMismatchException(SemblanceType expectedType, SemblanceType actualType, SourceInfo sourceInfo) {
        super(String.format("Type mismatch. Expected %s, but got %s.", expectedType, actualType), sourceInfo);
    }

    public TypeMismatchException(String expectedType, SemblanceType actualType, SourceInfo sourceInfo) {
        super(String.format("Type mismatch. Expected %s, but got %s.", expectedType, actualType), sourceInfo);
    }

}
