package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.errors.runtime.FunctionExpectedException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.UndefinedFunctionStrategy;

import javax.annotation.Nonnull;

/**
 * Allows calling methods on java objects
 */
public class JavaMethodExecutor implements UndefinedFunctionStrategy {

    @Nonnull
    @Override
    public LispValue handle(@Nonnull final Context context,
                            @Nonnull final LispValue head,
                            @Nonnull final LispCollection params) {

        if (head.getType() != SemblanceType.OPAQUE) {
            throw new FunctionExpectedException(head);
        }

        return null;
    }
}
