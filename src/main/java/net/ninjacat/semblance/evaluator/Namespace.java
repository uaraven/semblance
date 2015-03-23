package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.smooth.utils.Option;

/**
 * The namespace.
 */
public interface Namespace {

    SymbolAtom getName();

    /**
     * Find symbol declared in this context. Will search through parent contexts if symbol is not defined
     *
     * @param name symbol
     * @return value
     */
    Option<LispValue> findSymbol(SymbolAtom name);

    /**
     * Binds symbol to a value. Will overwrite current binding or create a new one
     *
     * @param name  name to bind to
     * @param value value
     */
    void bind(SymbolAtom name, LispValue value);
}
