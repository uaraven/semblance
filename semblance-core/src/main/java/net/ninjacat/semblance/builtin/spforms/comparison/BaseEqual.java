package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.collections.LispValue;

import java.util.function.Predicate;

import static net.ninjacat.semblance.utils.Values.asNumber;
import static net.ninjacat.semblance.utils.Values.isNumber;

/**
 * Base class for equality functions.
 */
public abstract class BaseEqual extends BaseComparison {

    /**
     * Create new instance.
     *
     * @param definition Function definition.
     */
    protected BaseEqual(final String... definition) {
        super(definition);
    }

    protected Predicate<LispValue> createComparatorFor(final LispValue first) {
        return isNumber(first)
                ? new NumericComparator(asNumber(first))
                : new AnyComparator(first);
    }

    private static class NumericComparator implements Predicate<LispValue> {
        private final NumberAtom first;

        private NumericComparator(final NumberAtom first) {
            this.first = first;
        }

        @Override
        public boolean test(final LispValue value) {
            //noinspection unchecked
            return isNumber(value) && asNumber(first).eq(asNumber(value));
        }
    }

    private static class AnyComparator implements Predicate<LispValue> {
        private final LispValue first;

        private AnyComparator(final LispValue first) {
            this.first = first;
        }

        @Override
        public boolean test(final LispValue value) {
            return first.equals(value);
        }
    }
}
