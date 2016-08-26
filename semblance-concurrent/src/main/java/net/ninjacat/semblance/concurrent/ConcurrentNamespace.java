package net.ninjacat.semblance.concurrent;

import net.ninjacat.semblance.concurrent.func.*;
import net.ninjacat.semblance.evaluator.BaseNamespace;

import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Namespace providing functions for asynchronous execution
 */
class ConcurrentNamespace extends BaseNamespace {

    /**
     * Creates a new namespace
     */
    ConcurrentNamespace() {
        super(symbol("async"));

        bind(symbol("run"), new AsyncRun());
        bind(symbol("await"), new Await());
        bind(symbol("delay"), new Delay());
        bind(symbol("go"), new GoFunc());
        bind(symbol("check"), new Check());
    }
}
