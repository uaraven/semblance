package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.concurrent.Concurrent;


class AsyncFixtures {
    private AsyncFixtures() {
    }

    static Interpreter getConcurrentInterpreter() {
        return new Interpreter(new Concurrent());
    }
}
