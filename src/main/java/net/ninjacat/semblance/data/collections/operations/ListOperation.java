package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

/**
 * Generic interface for lisp operations
 */
public interface ListOperation {

    /**
     * Performs an operation on a collection.
     *
     * @param source     Source collection.
     * @param parameters Parameters for an operation.
     * @return Result of the operation.
     */
    LispValue apply(LispCollection source, LispCollection parameters);
}
