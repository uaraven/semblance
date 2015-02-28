package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.errors.FunctionExpectedException;
import net.ninjacat.semblance.errors.UnboundSymbolException;
import net.ninjacat.smooth.utils.Option;

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

    private DefaultContext(String name, Context parent) {
        this.name = name;
        this.parent = parent;
        bindings = new ConcurrentHashMap<>();
    }

    public static Context namelessChildContext(Context parent) {
        return new DefaultContext("", parent);
    }

    public static Context namedChildContext(String name, Context parent) {
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
    public Option<LispValue> findSymbol(SymbolAtom name) {
        if (bindings.containsKey(name)) {
            return Option.of(bindings.get(name));
        } else if (parent != null) {
            return parent.findSymbol(name);
        } else {
            return Option.absent();
        }
    }

    @Override
    public void bind(SymbolAtom name, LispValue value) {
        bindings.put(name, value);
    }

    @Override
    public LispValue evaluate(LispValue expression) {
        if (isSymbol(expression)) {
            Option<LispValue> value = findSymbol(asSymbol(expression));
            if (value.isPresent()) {
                return value.get();
            } else {
                throw new UnboundSymbolException(asSymbol(expression), getSourceInfo(expression));
            }
        } else if (isList(expression)) {
            return evaluateFunction(asList(expression));
        } else {
            return expression.self();
        }
    }

    private LispValue evaluateFunction(SList function) {
        LispValue head = function.head();
        if (!isSymbol(head)) {
            throw new FunctionExpectedException(function);
        }
        Option<LispValue> callable = findSymbol(asSymbol(head));
        if (!callable.isPresent() || !isCallable(callable.get())) {
            throw new FunctionExpectedException(head);
        }
        LispCollection params = function.tail();
        Callable func = asCallable(callable.get());
        return func.apply(namedChildContext(func.name().asJavaObject().getValue(), this), params);
    }
}
