package net.ninjacat.semblance.evaluator;

import javax.annotation.Nonnull;

/**
 * Interface which allows to modify context, i.e. register additional namespaces, bind symbols, etc.
 */
public interface ContextModifier {

    /**
     * Modifies context.
     *
     * @param context Context to be modified.
     */
    void modify(@Nonnull Context context);
}
