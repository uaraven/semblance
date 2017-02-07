package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 28/02/15.
 */
public class InvalidFunctionDeclarationException extends SemblanceRuntimeException {
    private static final long serialVersionUID = -1070559047478730847L;

    /**
     * Creates new exception.
     *
     * @param definition Function declaration exception which caused exception.
     */
    public InvalidFunctionDeclarationException(final SList definition) {
        super("Invalid function declaration " + definition.printIt(), Values.getSourceInfo(definition));
    }
}
