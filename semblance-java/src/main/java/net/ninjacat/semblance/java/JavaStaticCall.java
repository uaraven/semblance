package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.types.CallHelpers;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;
import net.ninjacat.smooth.utils.Option;
import net.ninjacat.smooth.utils.Try;

import java.lang.reflect.Method;
import java.util.List;
import java.util.concurrent.Callable;

import static net.ninjacat.semblance.java.types.CallHelpers.convertParameters;
import static net.ninjacat.semblance.java.types.CallHelpers.toLispValue;
import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Static method call.
 * <p/>
 * Calls static method of a Java class. Must provide fully qualified method name in form of {@code package.Class.name}
 * <p/>
 * Returns the result of the method or NIL if method returns void.
 */
public class JavaStaticCall extends SpecialForm {

    /**
     * Creates a new instance of {@code scall} function
     */
    public JavaStaticCall() {
        super("scall", "method", "&rest", "parameters");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final String staticName = asSymbol(parameters.head()).repr();
        final LispCollection params = context.evaluateList(parameters.tail());

        final Option<Class> clazz = extractClassName(staticName);
        if (!clazz.isPresent()) {
            throw new JavaInteropException(String.format("Cannot get class for static method %s.", staticName));
        }
        final Option<Method> method = getStaticMethod(clazz.get(), staticName, params);
        if (!method.isPresent()) {
            throw new JavaInteropException(String.format("Cannot find method %s.", staticName));
        }

        try {
            final Object[] args = convertParameters(method.get().getGenericParameterTypes(), params);
            return toLispValue(method.get().invoke(clazz.get(), args));
        } catch (final Exception e) {
            throw new JavaInteropException(String.format("Failed to execute method %s(%s)", staticName, params), SourceInfo.UNKNOWN, e);
        }
    }

    private static Option<Method> getStaticMethod(final Class aClass, final String staticName, final LispCollection params) {
        final int dot = staticName.lastIndexOf('.');
        final String methodName = staticName.substring(dot + 1);
        final List<Method> methods = Iter.of(aClass.getMethods()).filter(new Predicate<Method>() {
            @Override
            public boolean matches(final Method method) {
                return methodName.equals(method.getName());
            }
        }).toList();
        return CallHelpers.findMatchingMethod(methods, params);
    }

    private static Option<Class> extractClassName(final String staticName) {
        final int dot = staticName.lastIndexOf('.');
        return Try.execute(new Callable<Class>() {
            @Override
            public Class call() throws Exception {
                final String className = staticName.substring(0, dot);
                return Class.forName(className);
            }
        }).get();
    }
}
