package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.types.CallHelpers;
import net.ninjacat.semblance.utils.Try;

import javax.annotation.Nonnull;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static net.ninjacat.semblance.java.types.CallHelpers.convertParameters;
import static net.ninjacat.semblance.utils.Values.*;

/**
 * Opaque value providing a way to call methods and access fields of the wrapped java object.
 */
public class JavaWrapperValue extends OpaqueValue<Object> implements LispCallable {

    private final Class<? extends Object> clazz;

    /**
     * Creates new instance of JavaWrapperValue
     *
     * @param value Java object to wrap
     */
    public JavaWrapperValue(@Nonnull final Object value) {
        super(value);
        clazz = value.getClass();
    }

    public static JavaWrapperValue asWrapper(final LispValue value) {
        if (value instanceof JavaWrapperValue) {
            return (JavaWrapperValue) value;
        } else {
            throw new SemblanceRuntimeException(String.format("Cannot use %s as Java object", value), getSourceInfo(value));
        }
    }

    @Override
    public SymbolAtom name() {
        return symbol(getValue().getClass().getCanonicalName());
    }

    @Override
    public LispValue apply(final Context context,
                           final LispCollection parameters) {
        final SymbolAtom objectProp = asSymbol(parameters.head());
        final LispCollection evaluatedParameters = context.evaluateList(parameters.tail());

        final Optional<Method> method = findMethod(objectProp, evaluatedParameters);

        if (method.isPresent()) {
            return runMethod(method.get(), evaluatedParameters);
        }
        final Optional<Field> field = findField(objectProp);
        if (field.isPresent()) {
            return runField(field.get(), evaluatedParameters);
        }
        throw new JavaInteropException(String.format("No field or method named %s in %s",
                objectProp, clazz.getCanonicalName()), SourceInfo.UNKNOWN);
    }

    private LispValue runMethod(final Method method, final LispCollection evaluatedParameters) {
        try {
            final Object result = method.invoke(getValue(), convertParameters(method.getGenericParameterTypes(), evaluatedParameters));
            return CallHelpers.toLispValue(result);
        } catch (final Exception e) {
            throw new JavaInteropException("Failed to invoke method " + method, SourceInfo.UNKNOWN, e);
        }
    }

    private LispValue runField(final Field field, final LispCollection evaluatedParameters) {
        try {
            if (evaluatedParameters.isNil()) {
                return CallHelpers.toLispValue(field.get(getValue()));
            } else {
                final LispValue newValue = evaluatedParameters.head();
                final Object value = CallHelpers.convertValue(field.getType(), newValue);
                field.set(getValue(), value);
                return newValue;
            }
        } catch (final Exception ex) {
            throw new JavaInteropException("Exception while accessing field " + field, SourceInfo.UNKNOWN, ex);
        }
    }

    private Optional<Field> findField(final SymbolAtom objectProp) {
        return Try.execute(() -> clazz.getField(objectProp.repr())).get();
    }

    private Optional<Method> findMethod(final SymbolAtom objectProp, final LispCollection params) {
        final List<Method> methods = Arrays.stream(clazz.getMethods())
                .filter(met -> met.getName().equals(objectProp.repr()))
                .collect(Collectors.toList());
        return CallHelpers.findMatchingMethod(methods, params);
    }
}
