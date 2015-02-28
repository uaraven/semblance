package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.smooth.utils.Option;

/**
 * Execution context
 * <p/>
 * Created on 26/02/15.
 */
public interface Context {

    /**
     * Context name.
     *
     * @return current context name. Empty string for nameless contexts
     */
    String getName();

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

    /**
     * Evaluates expression in current context
     *
     * @param expression Expression to evaluate
     * @return evaluated expression
     */
    LispValue evaluate(LispValue expression);

}
