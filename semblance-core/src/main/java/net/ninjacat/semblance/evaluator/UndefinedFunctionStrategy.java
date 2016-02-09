package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

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
