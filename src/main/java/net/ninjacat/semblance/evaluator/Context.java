package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
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
     * @return current context name. {@link net.ninjacat.semblance.data.Constants#NONE} for nameless contexts
     */
    SymbolAtom getName();

    /**
     * Find symbol declared in this context. Will search through parent contexts if symbol is not defined
     *
     * @param name symbol
     * @return value
     */
    Option<LispValue> findSymbol(SymbolAtom name);

    /**
     * Retrieves namespace by its name.
     *
     * @param name Name of the namespace.
     * @return Namespace.
     */
    Option<Namespace> getNamespace(SymbolAtom name);

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

    /**
     * Evaluates list of values
     *
     * @param actualParameters list of values to evaluate
     * @return list of evaluated values
     */
    <T extends LispCollection> T evaluateList(T actualParameters);

    /**
     * Evaluates collection of expressions, returns last evaluation as a result
     *
     * @param expressions List of expressions to evaluate.
     * @return Result of the last expression.
     */
    LispValue evaluateBlock(LispCollection expressions);

    /**
     * Adds new namespace to this context. If namespace with the same name already exists in the context
     * it will be overwritten.
     *
     * @param namespace Namespace to add.
     */
    void addNamespace(Namespace namespace);


    /**
     * Checks if this context has parent context with give name. Will check this context name also.
     *
     * @param name name of parent context
     * @return {@code true} if this context's or this context's parent's name is equal to supplied parameter
     */
    boolean hasParent(SymbolAtom name);
}
