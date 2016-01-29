package net.ninjacat.semblance.java;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;

/**
 * Runtime exception which covers Java interoperability problems
 */
public class JavaInteropException extends SemblanceRuntimeException {

    /**
     * Creates a new intsance of JavaInteropException.
     *
     * @param message    Error message.
     * @param sourceInfo Source code information.
     */
    public JavaInteropException(final String message, final SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }

    /**
     * Creates a new intsance of JavaInteropException.
     *
     * @param message    Error message.
     * @param sourceInfo Source code information.
     * @param cause      Original cause of the problem.
     */
    public JavaInteropException(final String message, final SourceInfo sourceInfo, final Throwable cause) {
        super(message, sourceInfo, cause);
    }
}
