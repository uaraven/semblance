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
import net.ninjacat.smooth.functions.Provider;
import net.ninjacat.smooth.utils.Option;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Default context implementation. Supports parent contexts
 */
abstract class BaseContext implements Context {

    private final SymbolAtom name;
    private final Context parent;
    private final Map<SymbolAtom, Namespace> namespaces;

    private Option<UndefinedFunctionStrategy> undefinedFunctionStrategy;

    BaseContext(final SymbolAtom name,
                final Context parent,
                final Option<UndefinedFunctionStrategy> undefinedFunctionStrategy) {
        this.name = name;
        this.parent = parent;
        this.undefinedFunctionStrategy = undefinedFunctionStrategy;
        namespaces = new ConcurrentHashMap<>();
        namespaces.put(Constants.NONE, new BaseNamespace(Constants.NONE));
    }

    @Override
    public SymbolAtom getName() {
        return name;
    }

    @Override
    public Option<LispValue> findSymbol(final SymbolAtom symbolName) {
        return findInNamespace(symbolName);
    }

    @Override
    public void bind(final SymbolAtom symbolName, final LispValue value) {
        final Namespace namespace = namespaces.get(symbolName.getNamespace());
        namespace.bind(symbolName.getLocalName(), value);
    }

    @Override
    public void update(final SymbolAtom symbolName, final LispValue value) {
        final Option<LispValue> bound = findSymbol(symbolName);
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

    @Override
    public LispValue evaluate(final LispValue expression) {
        if (isSymbol(expression)) {
            if (asSymbol(expression).repr().startsWith(":")) {
                return expression;
            } else {
                final Option<LispValue> value = findSymbol(asSymbol(expression));
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

    @Override
    public <T extends LispCollection> T evaluateList(final T params) {
        final List<LispValue> evaluatedParams = new ArrayList<>((int) params.length());
        for (final LispValue value : params) {
            final LispValue evaluated = evaluate(value);
            evaluatedParams.add(evaluated);
        }
        return params.createSame(new SList(evaluatedParams));
    }

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
    public Option<Namespace> getNamespace(final SymbolAtom namespaceName) {
        return namespaces.containsKey(namespaceName) ? Option.of(namespaces.get(namespaceName)) : Option.<Namespace>absent();
    }

    @Override
    public Option<Namespace> findNamespace(final SymbolAtom namespaceName) {
        final Option<Namespace> namespace = getNamespace(namespaceName);
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
        return undefinedFunctionStrategy.orGet(new Provider<UndefinedFunctionStrategy>() {
            @Override
            public UndefinedFunctionStrategy get() {
                return parent.getUndefinedFunctionStrategy();
            }
        });
    }

    @Override
    public void setUndefinedFunctionStrategy(final UndefinedFunctionStrategy undefinedFunctionStrategy) {
        this.undefinedFunctionStrategy = Option.of(undefinedFunctionStrategy);
    }

    protected Option<LispValue> findInNamespace(final SymbolAtom symbolName) {
        final SymbolAtom rootNs = symbolName.getNamespace();
        if (!namespaces.containsKey(rootNs)) {
            return lookInParent(symbolName);
        } else {
            final Namespace namespace = namespaces.get(rootNs);
            final Option<LispValue> symbol = namespace.findSymbol(symbolName.getLocalName());
            if (symbol.isPresent()) {
                return symbol;
            } else {
                return lookInParent(symbolName);
            }
        }
    }

    private LispValue evaluateReturnValue(final ReturnValue returnValue) {
        if (returnValue.isScoped() && !returnValue.getScope().equals(getName()) && !isRootContext()) {
            return returnValue;
        }
        return returnValue.getValue();
    }

    private boolean isRootContext() {
        return getName().equals(Constants.ROOT);
    }

    private Option<LispValue> lookInParent(final SymbolAtom symbolName) {
        if (null != parent) {
            return parent.findSymbol(symbolName);
        } else {
            return Option.absent();
        }
    }

    private LispValue evaluateFunction(final SList function) {
        final LispValue head = function.head();
        if (!isSymbol(head) && !isCallable(head)) {
            throw new FunctionExpectedException(function);
        }
        final Option<LispValue> callable = isCallable(head) ? Option.of(evaluate(head)) : findSymbol(asSymbol(head));
        if (!callable.isPresent() || !isCallable(callable.get())) {
            return getUndefinedFunctionStrategy().handle(this, head, function.tail());
        }
        final LispCollection params = function.tail();
        final LispCallable func = asCallable(callable.get());
        return func.apply(this, params);
    }
}
