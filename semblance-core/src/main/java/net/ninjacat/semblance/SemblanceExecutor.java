package net.ninjacat.semblance;

import net.ninjacat.semblance.evaluator.ContextModifier;
import net.ninjacat.semblance.evaluator.RootContext;

/**
 * Base class for executing semblance programs, allows to configure execution environment.
 * <p>
 * Actual implementations include {@link Interpreter} and {@link Debugger}
 */
class SemblanceExecutor {

    private final RootContext rootContext;

    /**
     * Creates a new executor with a supplied root context modifiers. Allows to bind functions, change namespaces, etc.
     *
     * @param contextModifiers Root context modifiers.
     */
    SemblanceExecutor(final ContextModifier... contextModifiers) {
        rootContext = new RootContext();
        for (final ContextModifier modifier : contextModifiers) {
            modifier.modify(rootContext);
        }
    }

    /**
     * Modifies root context of this executor with a given context modifier
     *
     * @param contextModifier Context modifier for executor's root context
     */
    public void injectContextModifier(final ContextModifier contextModifier) {
        contextModifier.modify(rootContext);
    }

    /**
     * @return Root context of this executor
     */
    public RootContext getRootContext() {
        return this.rootContext;
    }
}
