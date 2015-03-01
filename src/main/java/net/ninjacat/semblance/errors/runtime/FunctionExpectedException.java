package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 24/02/15.
 */
public class FunctionExpectedException extends SemblanceRuntimeException {

    public FunctionExpectedException(LispValue expression) {

        super(String.format("Function call expected in %s", expression), Values.getSourceInfo(expression));
    }
}
