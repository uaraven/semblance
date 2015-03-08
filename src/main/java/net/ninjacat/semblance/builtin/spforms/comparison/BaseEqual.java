package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.smooth.functions.Predicate;

import static net.ninjacat.semblance.utils.Values.asNumber;
import static net.ninjacat.semblance.utils.Values.isNumber;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public abstract class BaseEqual extends SpecialForm {
    public BaseEqual(String... definition) {
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
        public boolean matches(final LispValue value) {
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
        public boolean matches(final LispValue value) {
            return first.equals(value);
        }
    }
}
