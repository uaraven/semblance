package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.utils.Option;
import net.ninjacat.smooth.utils.Try;

import java.lang.reflect.Method;
import java.util.concurrent.Callable;

import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Simplifies implementation of {@link LispCallable} by Java classes.
 * <p>
 * Allows implementation to define methods with signature
 * <pre>
 *     LispValue method(Context context, LispCollection params)
 * </pre>
 * or
 * <pre>
 *     LispValue method(LispCollection params)
 * </pre>
 * and provides a method to dispatch calls to those methods.
 * <p>
 * Java class should implement {@link LispCallable} and delegate {@link LispCallable#apply(Context, LispCollection)}
 * to method {@link CallableDispatcher#dispatch(Object, Context, LispCollection)}
 */
public final class CallableDispatcher {

    private CallableDispatcher() {
    }

    /**
     * Searches for a suitable delegate method in wrapped callable and calls it with parameters.
     * <p>
     * Dispatcher will first look for the method that accepts {@link Context} and {@link LispCollection}. If such
     * method was found then it will be called and <strong>non-evaluated</strong> parameters will be passed to it.
     * <p>
     * If such method does not exist, then dispatcher will look for the method that acceps just {@link LispCollection}.
     * If that method is found, then parameters will be evaluated within the scope of context and evaluated parameters
     * will be passed to implementation.
     *
     * @param context    Interpreter {@link Context}
     * @param parameters List of call parameters. First element must contain a string atom with a name of the method
     *                   to call, rest of the collection will be treated as parameters for that method.
     * @return Method execution result
     * @throws JavaInteropException      if matching method was not found
     * @throws SemblanceRuntimeException if found method is not accessible or any exception happens during the execution.
     */
    public static LispValue dispatch(final Object delegate, final Context context, final LispCollection parameters) {
        final String functionName = asSymbol(parameters.head()).repr();
        final LispCollection params = parameters.tail();
        try {
            final Option<Method> method = findMethod(delegate, functionName, Context.class, LispCollection.class);
            if (method.isPresent()) {
                return (LispValue) method.get().invoke(delegate, context, params);
            }
            final Option<Method> funCall = findMethod(delegate, functionName, LispCollection.class);
            if (funCall.isPresent()) {
                final LispCollection evalParams = context.evaluateList(parameters.tail());
                return (LispValue) funCall.get().invoke(delegate, evalParams);
            }
            throw new JavaInteropException("Suitable method " + functionName
                    + " is not found in " + delegate.getClass().getCanonicalName());
        } catch (final Exception e) {
            throw new SemblanceRuntimeException(
                    String.format("Exception during call to %s.%s with parameters %s",
                            delegate.getClass().getCanonicalName(),
                            functionName,
                            params), parameters.getSourceInfo(), e);
        }
    }

    private static Option<Method> findMethod(final Object delegate, final String name, final Class<?>... paramTypes) {
        return Try.execute(new Callable<Method>() {
            @Override
            public Method call() throws Exception {
                return delegate.getClass().getMethod(name, paramTypes);
            }
        }).get();
    }
}
