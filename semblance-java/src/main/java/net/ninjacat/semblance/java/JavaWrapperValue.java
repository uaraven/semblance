package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.Callable;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.types.CallHelpers;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;
import net.ninjacat.smooth.utils.Option;
import net.ninjacat.smooth.utils.Try;

import javax.annotation.Nonnull;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import static net.ninjacat.semblance.utils.Values.asSymbol;
import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Opaque value providing a way to call methods and access fields of the wrapped java object.
 */
public class JavaWrapperValue extends OpaqueValue<Object> implements Callable {

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

    @Override
    public SymbolAtom name() {
        return symbol(getValue().getClass().getCanonicalName());
    }

    @Override
    public LispValue apply(final Context context,
                           final LispCollection parameters) {
        final SymbolAtom objectProp = asSymbol(parameters.head());
        final LispCollection evaluatedParameters = context.evaluateList(parameters.tail());

        final Option<Method> method = findMethod(objectProp);

        if (method.isPresent()) {
            return runMethod(method, evaluatedParameters);
        }
        final Option<Field> field = findField(objectProp);
        if (field.isPresent()) {
            return runField(field.get(), evaluatedParameters);
        }
        throw new JavaInteropException(String.format("No field or method named %s in %s",
                objectProp, clazz.getCanonicalName()), SourceInfo.UNKNOWN);
    }

    private LispValue runMethod(final Option<Method> method, final LispCollection evaluatedParameters) {
        throw new JavaInteropException("method execution is not implemented", SourceInfo.UNKNOWN);
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

    private Option<Field> findField(final SymbolAtom objectProp) {
        return Try.execute(new java.util.concurrent.Callable<Field>() {
            @Override
            public Field call() throws Exception {
                return clazz.getField(objectProp.repr());
            }
        }).get();
    }

    private Option<Method> findMethod(final SymbolAtom objectProp) {
        return Option.of(Iter.of(clazz.getMethods()).find(new Predicate<Method>() {
            @Override
            public boolean matches(final Method met) {
                return met.getName().equals(objectProp.toString());
            }
        }, null));
    }
}
