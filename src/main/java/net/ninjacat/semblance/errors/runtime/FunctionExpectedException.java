package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 24/02/15.
 */
public class FunctionExpectedException extends SemblanceRuntimeException {

    /**
     * Creates new exception.
     *
     * @param expression Function call expression which caused exception.
     */
    public FunctionExpectedException(final LispValue expression) {
        super(String.format("Function call expected in %s", expression), Values.getSourceInfo(expression));
    }
}
