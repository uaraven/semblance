package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;

/**
 * Generic interface for lisp operations
 */
public interface ListOperation {

    /**
     * Performs an operation on a collection.
     *
     * @param context    Execution context.
     * @param source     Source collection.
     * @param parameters Parameters for an operation. These come evaluated.
     * @return Result of the operation.
     */
    LispValue apply(@Nonnull Context context,
                    @Nonnull LispCollection source,
                    @Nonnull LispCollection parameters);
}
