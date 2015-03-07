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

    private final String name;
    private final Context parent;

    private final Map<SymbolAtom, LispValue> bindings;

    BaseContext(final String name, final Context parent) {
        this.name = name;
        this.parent = parent;
        bindings = new ConcurrentHashMap<>();
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Option<LispValue> findSymbol(final SymbolAtom symbolName) {
        if (bindings.containsKey(symbolName)) {
            return Option.of(bindings.get(symbolName));
        } else if (null != parent) {
            return parent.findSymbol(symbolName);
        } else {
            return Option.absent();
        }
    }

    @Override
    public void bind(final SymbolAtom symbolName, final LispValue value) {
        bindings.put(symbolName, value);
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
            return evaluateFunction(asSList(expression));
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
        }
        return last;
    }

    /**
     * Creates a named child context.
     *
     * @param name The name of child context.
     * @return Newly created context.
     */
    protected abstract Context createChild(String name);

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
        return func.apply(createChild(func.name().asJavaObject().getValue()), params);
    }

}
