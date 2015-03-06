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
public class DefaultContext implements Context {

    private final String name;
    private final Context parent;

    private final Map<SymbolAtom, LispValue> bindings;

    protected DefaultContext(final String name, final Context parent) {
        this.name = name;
        this.parent = parent;
        bindings = new ConcurrentHashMap<>();
    }

    public static Context namelessChildContext(final Context parent) {
        return new DefaultContext("", parent);
    }

    public static Context namedChildContext(final String name, final Context parent) {
        return new DefaultContext(name, parent);
    }

    public static Context newContext() {
        return new DefaultContext("", null);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Option<LispValue> findSymbol(final SymbolAtom name) {
        if (bindings.containsKey(name)) {
            return Option.of(bindings.get(name));
        } else if (null != parent) {
            return parent.findSymbol(name);
        } else {
            return Option.absent();
        }
    }

    @Override
    public void bind(final SymbolAtom name, final LispValue value) {
        bindings.put(name, value);
    }

    @Override
    public LispValue evaluate(final LispValue expression) {
        if (isSymbol(expression)) {
            final Option<LispValue> value = findSymbol(asSymbol(expression));
            if (value.isPresent()) {
                return value.get();
            } else {
                throw new UnboundSymbolException(asSymbol(expression), getSourceInfo(expression));
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
        return func.apply(namedChildContext(func.name().asJavaObject().getValue(), this), params);
    }
}
