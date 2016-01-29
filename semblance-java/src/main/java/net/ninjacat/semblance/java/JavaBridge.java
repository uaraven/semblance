package net.ninjacat.semblance.java;

import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;

import javax.annotation.Nonnull;

/**
 * Context modifier that bridges Semblance and Java by defining new functions and handles which
 * allow to create Java objects from Semblance and access their fields and methods.
 */
public class JavaBridge implements ContextModifier {

    @Override
    public void modify(@Nonnull final Context context) {
        context.addNamespace(new JavaNamespace());
        context.setUndefinedFunctionStrategy(new JavaMethodExecutor());
    }
}
