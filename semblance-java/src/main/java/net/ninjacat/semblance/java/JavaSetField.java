package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.types.CallHelpers;

import java.lang.reflect.Field;
import java.util.Optional;

import static java.lang.String.format;
import static net.ninjacat.semblance.utils.Values.asSymbol;
import static net.ninjacat.semblance.utils.Values.isSymbol;

/**
 * Sets the field value. Supports both object fields and static fields.
 */
public class JavaSetField extends SpecialForm {

    /**
     * Creates new instance of {@code java/set} command.
     */
    public JavaSetField() {
        super("set", "field", "value");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispValue head = parameters.head();
        if (isSymbol(head) && !canResolve(context, head)) {
            // treat as static field access
            return setStaticField(asSymbol(head), context.evaluateList(parameters.tail()));
        } else {
            // treat as object instance field
            final JavaWrapperValue javaObject = JavaWrapperValue.asWrapper(context.evaluate(head));
            return javaObject.apply(context, parameters.tail());
        }
    }

    private static boolean canResolve(final Context context, final LispValue head) {
        return context.findSymbol(asSymbol(head)).isPresent();
    }

    private static LispValue setStaticField(final SymbolAtom staticField, final LispCollection values) {
        final StaticReference reference = new StaticReference(staticField);
        final Optional<Field> field = reference.getField();
        if (!field.isPresent()) {
            throw new JavaInteropException(format("Failed to find static field %s", staticField.repr()));
        }

        final LispValue value = values.head();
        try {
            field.get().set(reference.getClazz(), CallHelpers.convertValue(field.get().getType(), value));
        } catch (final Exception ex) {
            throw new JavaInteropException(format("Failed to set static field %s to %s", staticField.repr(), value),
                    SourceInfo.UNKNOWN, ex);
        }

        return value;
    }
}
