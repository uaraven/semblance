package net.ninjacat.semblance.lib;

import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;

import javax.annotation.Nonnull;

/**
 * Regular expression functions
 */
public class RegexLib implements ContextModifier {

    @Override
    public void modify(@Nonnull final Context context) {
        context.addNamespace(new RegexNamespace());
    }

}
