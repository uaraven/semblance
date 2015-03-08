package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public class ValidationException extends SemblanceRuntimeException {

    public ValidationException(final String message, final SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }

    public ValidationException(final String message, final SourceInfo sourceInfo, final Throwable cause) {
        super(message, sourceInfo, cause);
    }
}
