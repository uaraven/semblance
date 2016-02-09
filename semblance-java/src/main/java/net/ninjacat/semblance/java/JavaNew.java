package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.utils.Option;

import java.lang.reflect.Constructor;

import static net.ninjacat.semblance.java.types.CallHelpers.convertParameters;
import static net.ninjacat.semblance.java.types.CallHelpers.findMatchingConstructor;
import static net.ninjacat.semblance.utils.Values.asSymbol;

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

        final String className = asSymbol(parameters.head()).repr();

        final Class<?> aClass;
        try {
            aClass = Class.forName(className);
        } catch (final ClassNotFoundException e) {
            throw new JavaInteropException("Class not found " + className, parameters.getSourceInfo(), e);
        }

        final LispCollection constructorParams = context.evaluateList(parameters.tail());

        final Option<Constructor> constructor = findMatchingConstructor(aClass.getConstructors(), constructorParams);

        if (!constructor.isPresent()) {
            throw new JavaInteropException("Cannot find constructor matching " + parameters.repr(), parameters.getSourceInfo());
        }

        final Object[] initArgs = convertParameters(constructor.get().getGenericParameterTypes(), constructorParams);

        try {
            return new JavaWrapperValue(constructor.get().newInstance(initArgs));
        } catch (final Exception e) {
            throw new SemblanceRuntimeException("Failed to create instance of " + className,
                    parameters.getSourceInfo(), e);
        }
    }
}
