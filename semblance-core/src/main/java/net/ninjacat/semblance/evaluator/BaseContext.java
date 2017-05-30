package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.data.special.ReturnValue;
import net.ninjacat.semblance.errors.runtime.FunctionExpectedException;
import net.ninjacat.semblance.errors.runtime.UnboundSymbolException;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Default context implementation. Supports parent contexts
 */
@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
abstract class BaseContext implements Context {

    private final SymbolAtom name;
    private final Context parent;
    private final Map<SymbolAtom, Namespace> namespaces;

    private Optional<UndefinedFunctionStrategy> undefinedFunctionStrategy;

    BaseContext(final SymbolAtom name,
                final Context parent,
                final Optional<UndefinedFunctionStrategy> undefinedFunctionStrategy) {
        this.name = name;
        this.parent = parent;
        this.undefinedFunctionStrategy = undefinedFunctionStrategy;
        namespaces = new ConcurrentHashMap<>();
        namespaces.put(Constants.NONE, new BaseNamespace(Constants.NONE));
    }

    @Nonnull
    @Override
    public SymbolAtom getName() {
        return name;
    }

    @Override
    public Optional<LispValue> findSymbol(final SymbolAtom symbolName) {
        return findInNamespace(symbolName);
    }

    @Override
    public void bind(final SymbolAtom symbolName, final LispValue value) {
        final Namespace namespace = namespaces.get(symbolName.getNamespace());
        namespace.bind(symbolName.getLocalName(), value);
    }

    @Override
    public void update(final SymbolAtom symbolName, final LispValue value) {
        final Optional<LispValue> bound = findSymbol(symbolName);
        if (bound.isPresent()) {
            updateExisting(symbolName, value);
        } else {
            bind(symbolName, value);
        }
    }

    @Override
    public void updateExisting(final SymbolAtom symbolName, final LispValue value) {
        final Namespace namespace = namespaces.get(symbolName.getNamespace());
        if (namespace.findSymbol(symbolName).isPresent()) {
            namespace.bind(symbolName, value);
        } else if (parent != null) {
            parent.updateExisting(symbolName, value);
        }
    }

    /**
     * Evaluates a single expression
     *
     * @param expression Expression to evaluate
     * @return Evaluated {@link LispValue}
     */
    @Override
    public LispValue evaluate(final LispValue expression) {
        if (isSymbol(expression)) {
            if (asSymbol(expression).repr().startsWith(":")) {
                return expression;
            } else {
                final Optional<LispValue> value = findSymbol(asSymbol(expression));
                if (value.isPresent()) {
                    return value.get();
                } else {
                    throw new UnboundSymbolException(asSymbol(expression), getSourceInfo(expression));
                }
            }
        } else if (isList(expression)) {
            final SList function = asSList(expression);
            if (function.isNil()) {
                return function;
            } else {
                return evaluateFunction(function);
            }
        } else if (isMap(expression)) {
            return asSMap(expression).evaluateValues(this);
        } else if (isVector(expression)) {
            return asVector(expression).evaluateValues(this);
        } else {
            return expression;
        }
    }

    /**
     * Evaluates a collection of values. This does not evaluates list as a function/special form, it simply
     * calls {@link #evaluate(LispValue)} for each element of collection
     *
     * @param params collection of values
     * @param <T>    Type of collection
     * @return Collection of evaluated values. Type of result is the same as type of original collection
     */
    @Override
    public <T extends LispCollection> T evaluateList(final T params) {
        final List<LispValue> evaluatedParams = new ArrayList<>(params.length());
        for (final LispValue value : params) {
            final LispValue evaluated = evaluate(value);
            evaluatedParams.add(evaluated);
        }
        return params.createSame(new SList(evaluatedParams));
    }

    /**
     * Evaluates a block of expressions, such as <pre>progn</pre>, <pre>block</pre> or function body.
     * <p>
     * evaluateBlock handles <pre>return</pre> statement.
     *
     * @param expressions List of expressions to evaluate.
     * @return Value of the last evaluated expression
     */
    @Override
    public LispValue evaluateBlock(final LispCollection expressions) {
        LispValue last = NilCollection.INSTANCE;
        for (final LispValue expr : expressions) {
            last = evaluate(expr);
            if (last.getType() == SemblanceType.RETURN) {
                final ReturnValue returnValue = asReturnValue(last);
                return evaluateReturnValue(returnValue);
            } else if (last.getType().isBreak()) {
                return last; // return immediately on break
            }
        }
        return last;
    }

    @Override
    public Optional<Namespace> getNamespace(final SymbolAtom namespaceName) {
        return namespaces.containsKey(namespaceName)
                ? Optional.ofNullable(namespaces.get(namespaceName))
                : Optional.empty();
    }

    @Override
    public Optional<Namespace> findNamespace(final SymbolAtom namespaceName) {
        final Optional<Namespace> namespace = getNamespace(namespaceName);
        if (!namespace.isPresent() && parent != null) {
            return parent.findNamespace(namespaceName);
        }
        return namespace;
    }

    @Override
    public void addNamespace(final Namespace namespace) {
        namespaces.put(namespace.getName(), namespace);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "{" +
                "name='" + name + '\'' +
                '}';
    }

    @Override
    public boolean hasParent(final SymbolAtom contextName) {
        return name.equals(contextName) || null != parent && parent.hasParent(contextName);
    }

    @Override
    public void setBindings(final Collection<Binding> bindings) {
        for (final Binding binding : bindings) {
            bind(binding.getName(), binding.getValue());
        }
    }

    @Override
    public List<String> getSourceFolders() {
        if (parent != null) {
            return parent.getSourceFolders();
        } else {
            return Collections.emptyList();
        }
    }

    @Override
    public void setSourceFolders(final List<String> sourceFolders) {
        if (parent != null) {
            parent.setSourceFolders(sourceFolders);
        }
    }

    @Override
    public UndefinedFunctionStrategy getUndefinedFunctionStrategy() {
        return undefinedFunctionStrategy.orElseGet(parent::getUndefinedFunctionStrategy);
    }

    @Override
    public void setUndefinedFunctionStrategy(final UndefinedFunctionStrategy undefinedFunctionStrategy) {
        this.undefinedFunctionStrategy = Optional.ofNullable(undefinedFunctionStrategy);
    }

    @Override
    public void fillContextStack(final List<SymbolAtom> contextStack) {
        if (parent != null) {
            parent.fillContextStack(contextStack);
        }
        contextStack.add(Constants.NONE.equals(getName()) ? symbol("[unknown]") : getName());
    }

    /**
     * Evaluates a function represented as list of function/symbol and parameters.
     * Firstly head of the list is checked for "evaluability", if it is a {@link LispCallable} or a symbol bound to
     * callable, then rest of the list is evaluated and function is called.
     * <p>
     * If head of the list is neither a callable nor a symbol bound to a callable, then <i>Undefined function strategy</i>
     * handler is called.
     *
     * @param function {@link SList} containing a function call
     * @return Evaluated value of the function
     */
    private LispValue evaluateFunction(final SList function) {
        final LispValue head = function.head();
        if (!isSymbol(head) && !isCallable(head)) {
            throw new FunctionExpectedException(function);
        }
        final Optional<LispValue> callable = isCallable(head)
                ? Optional.of(evaluate(head))
                : findSymbol(asSymbol(head));
        if (!callable.isPresent() || !isCallable(callable.get())) {
            return getUndefinedFunctionStrategy().handle(this, head, function.tail());
        }
        final LispCollection params = function.tail();
        final LispCallable func = asCallable(callable.get());
        return func.apply(this, params);
    }

    private LispValue evaluateReturnValue(final ReturnValue returnValue) {
        if (returnValue.isScoped() && !returnValue.getScope().equals(getName()) && !isRootContext()) {
            return returnValue;
        }
        return returnValue.getValue();
    }

    private Optional<LispValue> findInNamespace(final SymbolAtom symbolName) {
        final SymbolAtom rootNs = symbolName.getNamespace();
        if (!namespaces.containsKey(rootNs)) {
            return lookInParent(symbolName);
        } else {
            final Namespace namespace = namespaces.get(rootNs);
            final Optional<LispValue> symbol = namespace.findSymbol(symbolName.getLocalName());
            if (symbol.isPresent()) {
                return symbol;
            } else {
                return lookInParent(symbolName);
            }
        }
    }

    private boolean isRootContext() {
        return getName().equals(Constants.ROOT);
    }

    private Optional<LispValue> lookInParent(final SymbolAtom symbolName) {
        if (null != parent) {
            return parent.findSymbol(symbolName);
        } else {
            return Optional.empty();
        }
    }
}
