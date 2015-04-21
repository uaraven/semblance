package net.ninjacat.semblance.data;

import net.ninjacat.smooth.functions.Func2;
import org.hamcrest.CoreMatchers;

import static org.junit.Assert.assertThat;

public class ComparisionTestCase {

    private final Operation operation;
    private final NumberAtom op1;
    private final NumberAtom op2;
    private final boolean expected;

    private ComparisionTestCase(final Operation operation,
                                final NumberAtom op1,
                                final NumberAtom op2,
                                final Boolean expected) {
        this.operation = operation;
        this.op1 = op1;
        this.op2 = op2;
        this.expected = expected;
    }

    public static Builder mk(final Operation operation) {
        return new Builder(operation);
    }

    public void verifyResult() {
        final boolean actual = operation.apply(op1, op2);
        assertThat(String.format("%s %s %s should be equal to %s", op1, operation.getName(), op2, expected),
                actual, CoreMatchers.equalTo(expected));
    }

    public static class Builder {
        private final Operation operation;
        private NumberAtom op1;
        private NumberAtom op2;
        private boolean result;

        public Builder(final Operation action) {
            operation = action;
        }

        public Builder op1(final NumberAtom opr1) {
            op1 = opr1;
            return this;
        }

        public Builder op2(final NumberAtom opr2) {
            op2 = opr2;
            return this;
        }

        public Builder expect(final boolean res) {
            result = res;
            return this;
        }

        public ComparisionTestCase build() {
            return new ComparisionTestCase(operation, op1, op2, result);
        }
    }

    public abstract static class Operation implements Func2<Boolean, NumberAtom, NumberAtom> {
        private final String name;

        protected Operation(final String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

    }
}
