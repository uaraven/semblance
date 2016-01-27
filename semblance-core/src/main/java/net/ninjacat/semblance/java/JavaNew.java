package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;

/**
 * Special form to create new instance of java object
 */
public class JavaNew extends SpecialForm {

    /**
     * Creates new instance of JavaNew
     */
    public JavaNew() {
        super("java/new", "&rest", "parameters");
    }

    @Override
    public LispValue apply(final Context context,
                           final LispCollection parameters) {

        final String className = Values.asString(parameters.head()).getValue();

        try {
            return new OpaqueValue<>(Class.forName(className).newInstance());
        } catch (final Exception e) {
            throw new SemblanceRuntimeException("Failed to create instance of " + className,
                    parameters.getSourceInfo(), e);
        }
    }
}
