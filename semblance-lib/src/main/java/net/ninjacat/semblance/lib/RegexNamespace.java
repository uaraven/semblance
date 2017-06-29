package net.ninjacat.semblance.lib;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.Vector;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.BaseNamespace;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;
import java.util.Map;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static net.ninjacat.semblance.java.Lambdas.methodAsFunction;
import static net.ninjacat.semblance.utils.Values.*;

/**
 * Namespace for regular expression functions and constants
 */
@SuppressWarnings("MagicConstant")
class RegexNamespace extends BaseNamespace {
    /**
     * Creates a new 're' namespace
     */
    RegexNamespace() {
        super(symbol("re"));

        bind(symbol("unix-lines"), number(Pattern.UNIX_LINES));
        bind(symbol("case-insensitive"), number(Pattern.CASE_INSENSITIVE));
        bind(symbol("comments"), number(Pattern.COMMENTS));
        bind(symbol("multiline"), number(Pattern.MULTILINE));
        bind(symbol("literal"), number(Pattern.LITERAL));
        bind(symbol("dotall"), number(Pattern.DOTALL));
        bind(symbol("unicode-case"), number(Pattern.UNICODE_CASE));
        bind(symbol("canon-eq"), number(Pattern.CANON_EQ));
        bind(symbol("unicode-character-class"), number(Pattern.UNICODE_CHARACTER_CLASS));

        bind(symbol("compile"), methodAsFunction(RegexNamespace::compile));
        bind(symbol("match"), methodAsFunction(RegexNamespace::match));
    }


    private static LispValue compile(final Context context, final LispCollection parameters) {
        final int flags = collectFlags(parameters.tail());
        return new RegexWrapper(Pattern.compile(asString(parameters.head()).getValue(), flags));
    }

    private static LispValue match(final Context context, final LispCollection parameters) {
        final String input = asString(parameters.head()).getValue();
        final int flags = collectFlags(parameters.tail().tail());
        final Pattern pattern = Pattern.compile(asString(parameters.tail().head()).getValue(), flags);
        final Matcher matcher = pattern.matcher(input);
        if (matcher.matches()) {
            return matchToGroups(matcher);
        } else {
            return NilCollection.INSTANCE;
        }
    }

    private static int collectFlags(final LispCollection flags) {
        final Long fl = flags.stream().map(it -> asNumber(it).longValue()).reduce(0L, (x, y) -> x + y);
        return Math.toIntExact(fl);
    }

    private static LispValue matchToGroups(final Matcher matcher) {
        final ImmutableList.Builder<LispValue> groupBuilder = ImmutableList.builder();
        groupBuilder.add(string(matcher.group(0)));
        for (int i = 0; i < matcher.groupCount(); i++) {
            groupBuilder.add(string(matcher.group(i + 1)));
        }
        return new Vector(groupBuilder.build());
    }


    protected static class RegexWrapper extends OpaqueValue<Pattern> implements LispCallable {
        private static final long serialVersionUID = 1804372715341370995L;

        private final Map<String, Function<LispCollection, LispValue>> regexFunctions =
                ImmutableMap.<String, Function<LispCollection, LispValue>>builder()
                        .put("match", this::match)
                        .put("find", this::find)
                        .put("get-matcher", this::getMatcher)
                        .build();

        private RegexWrapper(@Nonnull final Pattern value) {
            super(value);
        }

        @Override
        public SymbolAtom name() {
            return symbol("regex(" + getValue() + ")");
        }

        @Override
        public LispValue apply(final Context context, final LispCollection parameters) {
            final String function = asSymbol(parameters.head()).repr();
            if (regexFunctions.containsKey(function)) {
                return regexFunctions.get(function).apply(parameters.tail());
            } else {
                throw new SemblanceRuntimeException("Unknown regex operation: " + function, parameters.getSourceInfo());
            }
        }

        /**
         * Matches parameter against this regex.
         * <p>
         * Returns:
         * <pre>
         *   - NIL if there is no match
         *   - vector of groups if there is a match. Group 0 is always present and is equal to whole match
         * </pre>
         *
         * @param collection list of parameters. First parameter must be string
         * @return result of the match
         */
        private LispValue match(final LispCollection collection) {
            final String input = asString(collection.head()).getValue();
            final Matcher matcher = getValue().matcher(input);
            if (matcher.matches()) {
                return matchToGroups(matcher);
            } else {
                return NilCollection.INSTANCE;
            }
        }

        /**
         * Same as {@link #match(LispCollection)}, but performs {@link Matcher#find()} instead of {@link Matcher#matches()}
         *
         * @param collection list of parameters. First parameter must be string
         * @return result of the match
         */
        private LispValue find(final LispCollection collection) {
            final String input = asString(collection.head()).getValue();
            final Matcher matcher = getValue().matcher(input);
            if (matcher.find()) {
                return matchToGroups(matcher);
            } else {
                return NilCollection.INSTANCE;
            }
        }

        /**
         * Returns a wrapper around {@link Matcher}
         *
         * @param collection list of parameters. First parameter must be string
         * @return matcher
         */
        private LispValue getMatcher(final LispCollection collection) {
            final String input = asString(collection.head()).getValue();
            final Matcher matcher = getValue().matcher(input);
            return new MatcherWrapper(matcher);
        }
    }


    protected static class MatcherWrapper extends OpaqueValue<Matcher> implements LispCallable {
        private static final long serialVersionUID = 1804372715341370995L;

        private final Map<String, Function<LispCollection, LispValue>> matcherFunctions =
                ImmutableMap.<String, Function<LispCollection, LispValue>>builder()
                        .put("match", this::match)
                        .put("find", this::find)
                        .build();

        MatcherWrapper(@Nonnull final Matcher value) {
            super(value);
        }

        @Override
        public SymbolAtom name() {
            return symbol("matcher");
        }

        @Override
        public LispValue apply(final Context context, final LispCollection parameters) {
            final String function = asSymbol(parameters.head()).repr();
            if (matcherFunctions.containsKey(function)) {
                return matcherFunctions.get(function).apply(parameters.tail());
            } else {
                throw new SemblanceRuntimeException("Unknown matcher operation: " + function, parameters.getSourceInfo());
            }
        }

        /**
         * Matches parameter against this regex.
         * <p>
         * Returns:
         * <pre>
         *   - NIL if there is no match
         *   - vector of groups if there is a match. Group 0 is always present and is equal to whole match
         * </pre>
         *
         * @param collection list of parameters. First parameter must be string
         * @return result of the match
         */
        private LispValue match(final LispCollection collection) {
            if (getValue().matches()) {
                return matchToGroups(getValue());
            } else {
                return NilCollection.INSTANCE;
            }
        }

        /**
         * Same as {@link #match(LispCollection)}, but performs {@link Matcher#find()} instead of {@link Matcher#matches()}
         *
         * @param collection list of parameters. First parameter must be string
         * @return result of the match
         */
        private LispValue find(final LispCollection collection) {
            if (getValue().find()) {
                return matchToGroups(getValue());
            } else {
                return NilCollection.INSTANCE;
            }
        }

    }
}
