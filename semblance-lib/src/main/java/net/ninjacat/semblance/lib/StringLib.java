package net.ninjacat.semblance.lib;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.errors.runtime.NotEnoughParametersException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;

import javax.annotation.Nonnull;
import java.util.Locale;
import java.util.Optional;
import java.util.stream.Collectors;

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
        context.bind(symbol("replace"), methodAsFunction(StringLib::replace));
        context.bind(symbol("replace-all"), methodAsFunction(StringLib::replaceAll));
        context.bind(symbol("split"), methodAsFunction(StringLib::split));
        context.bind(symbol("join"), methodAsFunction(StringLib::join));
        context.bind(symbol("to-lower"), methodAsFunction(StringLib::toLower));
        context.bind(symbol("to-upper"), methodAsFunction(StringLib::toUpper));
        context.bind(symbol("eq-ignore-case"), methodAsFunction(StringLib::eqCaseInsensitive));
    }

    private static LispValue toLower(final Context context, final LispCollection parameters) {
        final String text = asString(parameters.head()).getValue();
        final Locale locale = parameters.length() > 1
                ? new Locale(asString(parameters.tail().head()).getValue())
                : Locale.getDefault();

        return string(text.toLowerCase(locale));
    }

    private static LispValue toUpper(final Context context, final LispCollection parameters) {
        final String text = asString(parameters.head()).getValue();
        final Locale locale = parameters.length() > 1
                ? new Locale(asString(parameters.tail().head()).getValue())
                : Locale.getDefault();

        return string(text.toUpperCase(locale));
    }

    private static LispValue eqCaseInsensitive(final Context context, final LispCollection parameters) {
        final String text1 = asString(parameters.head()).getValue();
        final String text2 = asString(parameters.tail().head()).getValue();

        return text1.equalsIgnoreCase(text2) ? Constants.TRUE : Constants.FALSE;
    }

    private static LispValue indexOf(final Context context, final LispCollection parameters) {
        if (parameters.length() < 2) {
            throw new NotEnoughParametersException("(string sub-string) expected", parameters.getSourceInfo());
        }
        final String text = asString(parameters.head()).getValue();
        final String subtext = asString(parameters.tail().head()).getValue();
        final int startingIndex = parameters.length() > 2
                ? (int) asNumber(parameters.tail().tail().head()).longValue() : 0;

        return number(text.indexOf(subtext, startingIndex));
    }

    private static LispValue substring(final Context context, final LispCollection parameters) {
        if (parameters.length() < 2) {
            throw new NotEnoughParametersException("(string start-index [end-index]) expected", parameters.getSourceInfo());
        }
        final String text = asString(parameters.head()).getValue();
        final int start = (int) asNumber(parameters.tail().head()).longValue();
        final Optional<Integer> endIndex = parameters.length() > 2
                ? Optional.of((int) asNumber(parameters.tail().tail().head()).longValue())
                : Optional.empty();

        return endIndex
                .map(integer -> string(text.substring(start, integer)))
                .orElseGet(() -> string(text.substring(start)));
    }

    private static LispValue replace(final Context context, final LispCollection parameters) {
        if (parameters.length() < 3) {
            throw new NotEnoughParametersException("(string match replacement) expected", parameters.getSourceInfo());
        }
        final String text = asString(parameters.head()).getValue();
        final String match = asString(parameters.tail().head()).getValue();
        final String replaceWith = asString(parameters.tail().tail().head()).getValue();

        return string(text.replace(match, replaceWith));
    }

    private static LispValue replaceAll(final Context context, final LispCollection parameters) {
        if (parameters.length() < 3) {
            throw new NotEnoughParametersException("(string regex replacement) expected", parameters.getSourceInfo());
        }
        final String text = asString(parameters.head()).getValue();
        final String match = asString(parameters.tail().head()).getValue();
        final String replaceWith = asString(parameters.tail().tail().head()).getValue();

        return string(text.replaceAll(match, replaceWith));
    }

    private static LispValue split(final Context context, final LispCollection parameters) {
        final String text = asString(parameters.head()).getValue();
        final String splitter = parameters.length() > 1
                ? asString(parameters.tail().head()).getValue()
                : "\\s";

        return smartVector(text.split(splitter));
    }

    private static LispValue join(final Context context, final LispCollection parameters) {
        final String delimiter = parameters.length() > 1 ? asString(parameters.tail().head()).getValue() : " ";
        final LispCollection list = asCollection(parameters.head());
        return string(list.stream().map(LispValue::printIt).collect(Collectors.joining(delimiter)));
    }
}
