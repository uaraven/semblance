package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

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
    Optional<LispValue> findSymbol(SymbolAtom name);

    /**
     * Retrieves namespace by its name.
     *
     * @param name Name of the namespace.
     * @return Namespace.
     */
    Optional<Namespace> getNamespace(SymbolAtom name);

    /**
     * Retrieves namespace by its name. If namespace is not available in current context, continues searching through
     * parents
     *
     * @param name Name of the namespace.
     * @return Namespace or absent
     */
    Optional<Namespace> findNamespace(SymbolAtom name);


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

    /**
     * Updates already bound symbol. Looks in all parent context for a bound symbol. If no bound symbol found, binds
     * it in local context
     *
     * @param name  Symbol name.
     * @param value Value to bound to name
     */
    void update(SymbolAtom name, LispValue value);

    /**
     * Updates already bound symbol or tries to bind it in parent.
     *
     * @param symbolName Symbol name.
     * @param value      Value to bound to name.
     */
    void updateExisting(final SymbolAtom symbolName, final LispValue value);

    /**
     * Sets bindings in this context
     *
     * @param bindings collection of {@link Binding}
     */
    void setBindings(Collection<Binding> bindings);

    /**
     * @return List of folders to look for sources.
     */
    List<String> getSourceFolders();

    /**
     * Sets list of folders to look for sources.
     *
     * @param sourceFolders List of source folders.
     */
    void setSourceFolders(List<String> sourceFolders);

    /**
     * @param contextStack stack of context to fill
     */
    void fillContextStack(List<SymbolAtom> contextStack);

    /**
     * @return Strategy to resolve undefined functions.
     */
    UndefinedFunctionStrategy getUndefinedFunctionStrategy();

    /**
     * Sets strategy to handle undefined functions
     *
     * @param undefinedFunctionStrategy Instance of {@link UndefinedFunctionStrategy}
     */
    void setUndefinedFunctionStrategy(final UndefinedFunctionStrategy undefinedFunctionStrategy);
}
