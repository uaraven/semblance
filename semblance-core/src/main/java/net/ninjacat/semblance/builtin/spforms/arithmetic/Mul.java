package net.ninjacat.semblance.builtin.spforms.arithmetic;

import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.ParameterException;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.iterators.Iter;

import java.util.ArrayList;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Multiplication
 */
@SuppressWarnings("ClassNamingConvention")
public class Mul extends BuiltInFunction {

    /**
     * Creates new instance
     */
    public Mul() {
        super("*", "&rest", "values");
    }

    @Override
    public LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispValue head = evaluated.head();
        if (isNumber(head)) {
            return numberMultiplication(evaluated);
        } else if (isCollection(head)) {
            return collectionMultiplication(evaluated);
        } else {
            throw new TypeMismatchException("NUMBER or COLLECTION", head, evaluated.getSourceInfo());
        }
    }

    private LispValue collectionMultiplication(final LispCollection evaluated) {
        if (evaluated.length() != 2) {
            throw new ParameterException("Exactly two parameters expected for collection multiplication", evaluated.getSourceInfo());
        }
        final LispValue other = evaluated.tail().head();
        final LispCollection collection = asCollection(evaluated.head());
        if (isNumber(other)) {
            return scalarProduct(collection, asNumber(other));
        } else if (isCollection(other)) {
            return cartesianProduct(collection, asCollection(other));
        } else {
            throw new TypeMismatchException("NUMBER or COLLECTION", other, getSourceInfo(other));
        }
    }

    private LispValue cartesianProduct(final LispCollection collection, final LispCollection other) {
        final List<LispValue> result = new ArrayList<>(collection.length());
        for (final LispValue item1 : collection) {
            for (final LispValue item2 : other) {
                result.add(list(item1, item2));
            }
        }
        return collection.createNew(result);
    }

    private LispValue scalarProduct(final LispCollection collection, final NumberAtom number) {
        return collection.createNew(Iter.of(collection.getCollection()).map(new Func<LispValue, LispValue>() {
            @SuppressWarnings("unchecked")
            @Override
            public LispValue apply(final LispValue value) {
                return asNumber(value).mul(number);
            }
        }).toList());
    }

    private LispValue numberMultiplication(final LispCollection evaluated) {
        NumberAtom accumulator = asNumber(evaluated.head());
        for (final LispValue value : evaluated.tail()) {
            //noinspection unchecked
            accumulator = accumulator.mul(asNumber(value));
        }
        return accumulator;
    }
}
