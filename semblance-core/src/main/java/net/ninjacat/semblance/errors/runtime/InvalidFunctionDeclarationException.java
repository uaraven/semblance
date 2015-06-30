package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 28/02/15.
 */
public class InvalidFunctionDeclarationException extends SemblanceRuntimeException {
    /**
     * Creates new exception.
     *
     * @param definition Function declaration exception which caused exception.
     */
    public InvalidFunctionDeclarationException(final SList definition) {
        super("Invalid function declaration " + definition, Values.getSourceInfo(definition));
    }
}
