package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Exception thrown when Semblance object cannot be converted to Java object
 */
public class JavaConversionException extends SemblanceRuntimeException {
    public JavaConversionException(String message, SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }
}
