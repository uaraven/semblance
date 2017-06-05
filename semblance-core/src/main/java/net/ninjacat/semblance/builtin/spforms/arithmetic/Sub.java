package net.ninjacat.semblance.builtin.spforms.arithmetic;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SMap;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;

import java.util.ArrayList;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Subtraction
 */
@SuppressWarnings("ClassNamingConvention")
public class Sub extends BuiltInFunction {

    private static final long serialVersionUID = 1724505021004741708L;

    /**
     * Creates new instance
     */
    public Sub() {
        super("-", "&rest", "values");
    }

    @Override
    public LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispValue head = evaluated.head();
        if (isNumber(head)) {
            return subtract(evaluated);
        }
        if (isCollection(head)) {
            return difference(evaluated);
        }
        if (isMap(head)) {
            return mapDifference(evaluated);
        }
        throw new TypeMismatchException("NUMBER, COLLECTION or MAP", head, evaluated.getSourceInfo());
    }

    private static LispValue difference(final LispCollection evaluated) {
        final LispCollection head = asCollection(evaluated.head());
        final List<LispValue> collection = new ArrayList<>(head.getCollection());
        for (final LispValue item : evaluated.tail()) {
            collection.removeAll(asCollection(item).getCollection());
        }
        return head.createNew(collection);
    }

    private static LispValue mapDifference(final LispCollection evaluated) {
        final SMap result = asSMap(evaluated.head()).duplicate();
        for (final LispValue item : evaluated.tail()) {
            result.removeAll(asSMap(item));
        }
        return result;
    }

    private static LispValue subtract(final LispCollection evaluated) {
        NumberAtom accumulator = asNumber(evaluated.head());
        if (evaluated.length() == 1) {
            return accumulator.neg();
        }
        for (final LispValue value : evaluated.tail()) {
            //noinspection unchecked
            accumulator = accumulator.sub(asNumber(value));
        }
        return accumulator;
    }
}
