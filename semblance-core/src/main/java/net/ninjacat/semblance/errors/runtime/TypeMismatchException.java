package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class TypeMismatchException extends SemblanceRuntimeException {
    private static final long serialVersionUID = -8762131873899657849L;

    private static final String MESSAGE = "Type mismatch. Expected %s, but got %s: %s";

    /**
     * Creates new exception.
     *
     * @param expectedType Expected type.
     * @param value   Encountered type.
     * @param sourceInfo   Source code information.
     */
    public TypeMismatchException(final SemblanceType expectedType, final LispValue value, final SourceInfo sourceInfo) {
        super(String.format(MESSAGE, expectedType, value.getType(), value.printIt()), sourceInfo);
    }

    /**
     * Creates new exception.
     *
     * @param expectedType Expected type.
     * @param actualType   Encountered type.
     * @param sourceInfo   Source code information.
     */
    public TypeMismatchException(final String expectedType, final LispValue actualType, final SourceInfo sourceInfo) {
        super(String.format(MESSAGE, expectedType, actualType.getType(), actualType), sourceInfo);
    }

}
