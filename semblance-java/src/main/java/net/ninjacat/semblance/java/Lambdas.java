package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;

import java.util.function.BiFunction;

/**
 * Java8-specific interop utility methods
 */
public final class Lambdas {

    private Lambdas() {
    }

    /**
     * Converts method to Semblance function. Method must follow {@link BuiltInFunction} calling convention.
     * <p>
     * Parameters will be evaluated before calling actual method.
     *
     * @param method Method method to wrap in {@link LispCallable}
     * @return Semblance function
     */
    public static LispCallable methodAsFunction(final BiFunction<Context, LispCollection, LispValue> method) {
        return new BuiltInFunction("java-method-as-function") {
            private static final long serialVersionUID = 3705566817180598441L;

            @Override
            protected LispValue applyFunction(final Context context, final LispCollection lispCollection) {
                return method.apply(context, lispCollection);
            }
        };
    }

    /**
     * Converts method to Semblance function. Method must follow {@link SpecialForm} calling convention.
     * <p>
     * Parameters <strong>will not</strong> be evaluated before calling actual method.
     *
     * @param method Method method to wrap in {@link LispCallable}
     * @return Semblance function
     */
    public static LispCallable methodAsSForm(final BiFunction<Context, LispCollection, LispValue> method) {
        return new SpecialForm("java-method-as-sform") {
            private static final long serialVersionUID = 3705566817180598441L;

            @Override
            public LispValue apply(final Context context, final LispCollection lispCollection) {
                return method.apply(context, lispCollection);
            }
        };
    }

}
