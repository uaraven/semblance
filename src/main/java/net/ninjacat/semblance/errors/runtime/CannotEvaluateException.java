package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 26/02/15.
 */
public class CannotEvaluateException extends SemblanceRuntimeException {

    public CannotEvaluateException(final LispValue expression) {
        super(String.format("Cannot evaluate expression '%s'", expression), Values.getSourceInfo(expression));
    }
}
