package net.ninjacat.semblance.builtin.spforms.types;

import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Collectors;
import net.ninjacat.semblance.utils.Values;

import java.util.function.Function;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Converts number (or collection of numbers) to integer
 */
public class AsInt extends BuiltInFunction {

    public AsInt() {
        super("as-int", "values");
    }

    @Override
    protected LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispValue param = evaluated.head();
        if (isCollection(param)) {
            final LispCollection collection = asCollection(param);
            return collection.stream()
                    .map(Values::asNumber)
                    .map(NumberAtom::bigIntValue)
                    .map(Values::bigN)
                    .map((Function<NumberAtom, NumberAtom>) NumberAtom::minify)
                    .collect(Collectors.toSameAs(collection));
        } else {
            return bigN(asNumber(param).bigIntValue()).minify();
        }
    }
}
