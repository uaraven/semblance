package net.ninjacat.semblance.builtin.spforms.arithmetic;

import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import java.util.ArrayList;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Subtraction
 */
@SuppressWarnings("ClassNamingConvention")
public class Sub extends SpecialForm {

    /**
     * Creates new instance
     */
    public Sub() {
        super("-", "&rest", "values");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCollection evaluated = context.evaluateList(parameters);
        final LispValue head = evaluated.head();
        if (isCollection(head)) {
            return difference(evaluated);
        } else {
            return subtract(evaluated);
        }
    }

    private static LispValue difference(final LispCollection evaluated) {
        final LispCollection head = asCollection(evaluated.head());
        final List<LispValue> collection = new ArrayList<>(head.getCollection());
        for (final LispValue item : evaluated.tail()) {
            collection.removeAll(asCollection(item).getCollection());
        }
        return head.createNew(collection);
    }

    private LispValue subtract(final LispCollection evaluated) {
        NumberAtom accumulator = asNumber(evaluated.head());
        for (final LispValue value : evaluated.tail()) {
            //noinspection unchecked
            accumulator = accumulator.sub(asNumber(value));
        }
        return accumulator;
    }
}
