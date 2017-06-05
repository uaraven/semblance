package net.ninjacat.semblance.builtin.spforms.arithmetic;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SMap;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;

import java.util.ArrayList;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Addition, union and concatenation
 */
@SuppressWarnings("ClassNamingConvention")
public class Add extends BuiltInFunction {

    private static final long serialVersionUID = -2093648011052200445L;

    /**
     * Create instance of Add class
     */
    public Add() {
        super("+", "&rest", "values");
    }

    @Override
    public LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispValue head = evaluated.head();
        if (isNumber(head)) {
            return numericAdd(evaluated);
        }
        if (isString(head)) {
            return concatenate(evaluated);
        }
        if (isCollection(head)) {
            return collectionUnion(evaluated);
        }
        if (isMap(head)) {
            return mapUnion(evaluated);
        }
        throw new SemblanceRuntimeException("Invalid argument types", evaluated.getSourceInfo());
    }

    private static LispValue mapUnion(final LispCollection evaluated) {
        final SMap result = new SMap();
        for (final LispValue value : evaluated) {
            result.addAll(asSMap(value));
        }
        return result;
    }

    private static LispValue concatenate(final LispCollection evaluated) {
        final StringBuilder result = new StringBuilder();
        for (final LispValue value : evaluated) {
            result.append(asString(value).getValue());
        }
        return string(result.toString());
    }

    private static LispValue collectionUnion(final LispCollection evaluated) {
        final List<LispValue> result = new ArrayList<>();
        for (final LispValue value : evaluated) {
            result.addAll(asList(asCollection(value)));
        }
        return asCollection(evaluated.head()).createNew(result);
    }

    private static LispValue numericAdd(final LispCollection evaluated) {
        NumberAtom accumulator = asNumber(evaluated.head());
        for (final LispValue value : evaluated.tail()) {
            //noinspection unchecked
            accumulator = accumulator.add(asNumber(value));
        }
        return accumulator;
    }
}
