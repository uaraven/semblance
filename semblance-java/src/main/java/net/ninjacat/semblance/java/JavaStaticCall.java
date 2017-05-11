package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.evaluator.Context;

import java.lang.reflect.Method;
import java.util.Optional;

import static net.ninjacat.semblance.java.types.CallHelpers.*;
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

        final StaticReference reference = new StaticReference(staticName);
        final Optional<Method> method = findMatchingMethod(reference.getMethods(), params);

        if (!method.isPresent()) {
            throw new JavaInteropException(String.format("Cannot find method %s.", staticName));
        }

        try {
            final Object[] args = convertParameters(method.get().getGenericParameterTypes(), params);
            return toLispValue(method.get().invoke(reference.getClazz(), args));
        } catch (final Exception e) {
            throw new JavaInteropException(String.format("Failed to execute method %s(%s)", staticName, params), SourceInfo.UNKNOWN, e);
        }
    }
}
