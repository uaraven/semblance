package net.ninjacat.semblance.lib;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;

import javax.annotation.Nonnull;
import java.util.Optional;

import static net.ninjacat.semblance.java.Lambdas.methodAsFunction;
import static net.ninjacat.semblance.utils.Values.*;

/**
 * String functions, including regular expressions
 */
public class StringLib implements ContextModifier {

    @Override
    public void modify(@Nonnull final Context context) {

        context.bind(symbol("index-of"), methodAsFunction(StringLib::indexOf));
        context.bind(symbol("substring"), methodAsFunction(StringLib::substring));
    }

    private static LispValue indexOf(final Context context, final LispCollection parameters) {
        final String text = asString(parameters.head()).getValue();
        final String subtext = asString(parameters.tail().head()).getValue();
        final int startingIndex = parameters.length() > 2
                ? (int) asNumber(parameters.tail().tail().head()).longValue() : 0;

        return number(text.indexOf(subtext, startingIndex));
    }

    private static LispValue substring(final Context context, final LispCollection parameters) {
        final String text = asString(parameters.head()).getValue();
        final int start = (int) asNumber(parameters.tail().head()).longValue();
        final Optional<Integer> endIndex = parameters.length() > 2
                ? Optional.of((int) asNumber(parameters.tail().tail().head()).longValue())
                : Optional.empty();

        return endIndex
                .map(integer -> string(text.substring(start, integer)))
                .orElseGet(() -> string(text.substring(start)));

    }
}
