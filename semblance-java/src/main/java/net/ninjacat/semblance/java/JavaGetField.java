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
import static net.ninjacat.semblance.utils.Values.*;

/**
 * Sets the field value. Supports both object fields and static fields.
 */
public class JavaGetField extends SpecialForm {

    /**
     * Creates new instance of {@code java/get} command.
     */
    public JavaGetField() {
        super("get", "field");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispValue head = parameters.head();
        if (isSymbol(head) && !canResolve(context, head)) {
            // treat as static field access
            return getStaticField(asSymbol(head));
        } else {
            // treat as object instance field
            final JavaWrapperValue javaObject = JavaWrapperValue.asWrapper(context.evaluate(head));
            return javaObject.apply(context, list(parameters.tail().head()));
        }
    }

    private static boolean canResolve(final Context context, final LispValue head) {
        return context.findSymbol(asSymbol(head)).isPresent();
    }

    private static LispValue getStaticField(final SymbolAtom staticField) {
        final StaticReference reference = new StaticReference(staticField);
        final Optional<Field> field = reference.getField();
        if (!field.isPresent()) {
            throw new JavaInteropException(format("Failed to find static field %s", staticField.repr()));
        }

        try {
            return CallHelpers.toLispValue(field.get().get(reference.getClazz()));
        } catch (final Exception ex) {
            throw new JavaInteropException(format("Failed to retrieve static field %s", staticField.repr()),
                    SourceInfo.UNKNOWN, ex);
        }
    }
}
