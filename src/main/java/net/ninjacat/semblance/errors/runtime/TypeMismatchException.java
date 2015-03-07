package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class TypeMismatchException extends SemblanceRuntimeException {

    public static final String MESSAGE = "Type mismatch. Expected %s, but got %s.";

    /**
     * Creates new exception.
     *
     * @param expectedType Expected type.
     * @param actualType   Encountered type.
     * @param sourceInfo   Source code information.
     */
    public TypeMismatchException(final SemblanceType expectedType, final SemblanceType actualType, final SourceInfo sourceInfo) {
        super(String.format(MESSAGE, expectedType, actualType), sourceInfo);
    }

    /**
     * Creates new exception.
     *
     * @param expectedType Expected type.
     * @param actualType   Encountered type.
     * @param sourceInfo   Source code information.
     */
    public TypeMismatchException(final String expectedType, final SemblanceType actualType, final SourceInfo sourceInfo) {
        super(String.format(MESSAGE, expectedType, actualType), sourceInfo);
    }

}
