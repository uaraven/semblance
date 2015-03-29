package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.errors.runtime.FunctionExpectedException;
import net.ninjacat.semblance.errors.runtime.UnboundSymbolException;
import net.ninjacat.smooth.utils.Option;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Default context implementation. Supports parent contexts
 */
abstract class BaseContext implements Context {

    private final SymbolAtom name;
    private final Context parent;
    private final Map<SymbolAtom, Namespace> namespaces;

    BaseContext(final SymbolAtom name, final Context parent) {
        this.name = name;
        this.parent = parent;
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
            }
        }
        return last;
    }

    @Override
    public Option<Namespace> getNamespace(final SymbolAtom namespaceName) {
        return namespaces.containsKey(namespaceName) ? Option.of(namespaces.get(namespaceName)) : Option.<Namespace>absent();
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
        if (!isSymbol(head)) {
            throw new FunctionExpectedException(function);
        }
        final Option<LispValue> callable = findSymbol(asSymbol(head));
        if (!callable.isPresent() || !isCallable(callable.get())) {
            throw new FunctionExpectedException(head);
        }
        final LispCollection params = function.tail();
        final Callable func = asCallable(callable.get());
        return func.apply(this, params);
    }

}
