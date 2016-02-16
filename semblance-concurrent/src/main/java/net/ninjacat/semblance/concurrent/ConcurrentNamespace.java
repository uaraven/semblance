package net.ninjacat.semblance.concurrent;

import net.ninjacat.semblance.concurrent.func.Async;
import net.ninjacat.semblance.concurrent.func.Await;
import net.ninjacat.semblance.evaluator.BaseNamespace;

import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Namespace providing functions for asynchronous execution
 */
public class ConcurrentNamespace extends BaseNamespace {

    /**
     * Creates a new namespace
     */
    public ConcurrentNamespace() {
        super(symbol("async"));

        bind(symbol("run"), new Async());
        bind(symbol("await"), new Await());
    }
}
