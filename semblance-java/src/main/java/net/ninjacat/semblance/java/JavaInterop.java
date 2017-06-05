package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.types.CallHelpers;

import java.lang.reflect.Method;

import static net.ninjacat.semblance.java.types.CallHelpers.convertParameters;
import static net.ninjacat.semblance.utils.Values.*;

/**
 * Utilities for Java-Semblance interoperability
 */
public final class JavaInterop {

    private JavaInterop() {
    }

    /**
     * Converts java object into Semblance {@link LispValue}
     *
     * @param javaObject Object to convert
     * @return LispValue with wrapped java object
     */
    public static LispValue toLisp(final Object javaObject) {
        return new JavaWrapperValue(javaObject);
    }


    public static LispCallable methodToFunction(final Object owner, final Method method) {
        return new BuiltInFunction("java-" + method.getName()) {
            @Override
            protected LispValue applyFunction(final Context context, final LispCollection evaluated) {
                try {
                    return CallHelpers.toLispValue(method.invoke(owner,
                            convertParameters(method.getGenericParameterTypes(), evaluated)));
                } catch (final Exception e) {
                    throw new JavaInteropException("Failed to invoke method " + method, SourceInfo.UNKNOWN, e);
                }
            }
        };
    }

    public static LispValue callFunction(final Context context, final String name, final Object... parameters) {
        final LispCollection call = list(symbol(name)).append(smartList(parameters));
        return context.evaluate(call);
    }
}
