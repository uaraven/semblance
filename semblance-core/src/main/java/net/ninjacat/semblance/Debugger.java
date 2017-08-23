package net.ninjacat.semblance;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.evaluator.ContextModifier;

/**
 * Semblance debugger
 */
public class Debugger extends SemblanceExecutor {

    /**
     * Creates a new debugger with a supplied root context modifiers. Allows to bind functions, change namespaces, etc.
     *
     * @param contextModifiers Root context modifiers.
     */
    public Debugger(final ContextModifier... contextModifiers) {
        super(contextModifiers);
    }

    public LispValue stepOver() {
        return NilCollection.INSTANCE;
    }

    public LispValue stepInto() {
        return NilCollection.INSTANCE;
    }

    public LispValue stepOut() {
        return NilCollection.INSTANCE;
    }

}
