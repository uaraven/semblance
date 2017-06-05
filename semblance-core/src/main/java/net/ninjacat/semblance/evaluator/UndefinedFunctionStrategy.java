package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;

import javax.annotation.Nonnull;

/**
 * Strategy to handle undefined functions
 */
public interface UndefinedFunctionStrategy {

    @Nonnull
    LispValue handle(@Nonnull final Context context,
                     @Nonnull final LispValue head,
                     @Nonnull final LispCollection params);

}
