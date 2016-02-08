package net.ninjacat.semblance.java;

import net.ninjacat.semblance.evaluator.BaseNamespace;

import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Java interop namespace
 */
public class JavaNamespace extends BaseNamespace {

    /**
     * Creates a new namespace
     */
    public JavaNamespace() {
        super(symbol("java"));

        bind(symbol("new"), new JavaNew());
        bind(symbol("scall"), new JavaStaticCall());
    }
}
