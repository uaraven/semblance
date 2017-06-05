package net.ninjacat.semblance.builtin.spforms.types;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Collectors;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Parses string into integer
 */
public class ParseNumber extends BuiltInFunction {

    private static final long serialVersionUID = 2885134008333295733L;

    /**
     * Creates a new instance of 'parse-number' function
     */
    public ParseNumber() {
        super("parse-number", "value");
    }

    @Override
    protected LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispValue head = evaluated.head();
        try {
            if (isCollection(head)) {
                final LispCollection collection = asCollection(head);
                return collection.stream()
                        .map(it -> asString(it).getValue())
                        .map(NumberAtom::make)
                        .map(NumberAtom::minify)
                        .collect(Collectors.toSameAs(collection));
            } else {
                return NumberAtom.make(asString(head).getValue()).minify();
            }
        } catch (final NumberFormatException nfe) {
            throw new SemblanceRuntimeException("Invalid parameter for parse-number: " + head.printIt(), evaluated.getSourceInfo(), nfe);
        }
    }
}
