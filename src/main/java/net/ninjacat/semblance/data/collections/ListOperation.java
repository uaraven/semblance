package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;

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
