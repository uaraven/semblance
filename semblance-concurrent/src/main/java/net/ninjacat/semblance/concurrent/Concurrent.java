package net.ninjacat.semblance.concurrent;

import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;

import javax.annotation.Nonnull;

/**
 * Context modifier that introduces "async" namespace
 */
public class Concurrent implements ContextModifier {

    @Override
    public void modify(@Nonnull final Context context) {

        context.addNamespace(new ConcurrentNamespace());

    }
}
