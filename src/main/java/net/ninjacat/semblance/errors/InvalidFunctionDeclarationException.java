package net.ninjacat.semblance.errors;

import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 28/02/15.
 */
public class InvalidFunctionDeclarationException extends SemblanceRuntimeException {
    public InvalidFunctionDeclarationException(SList definition) {
        super("Invalid function declaration " + definition, Values.getSourceInfo(definition));
    }
}
