package net.ninjacat.semblance.java;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;

/**
 * Runtime exception which covers Java interoperability problems
 */
public class JavaInteropException extends SemblanceRuntimeException {
    public JavaInteropException(final String message, final SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }

    public JavaInteropException(final String message, final SourceInfo sourceInfo, final Throwable cause) {
        super(message, sourceInfo, cause);
    }
}
