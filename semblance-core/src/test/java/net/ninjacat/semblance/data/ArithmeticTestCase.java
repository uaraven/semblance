package net.ninjacat.semblance.data;

import net.ninjacat.smooth.functions.Func2;
import org.hamcrest.CoreMatchers;

import static org.junit.Assert.assertThat;

public class ArithmeticTestCase {

    private final Operation operation;
    private final NumberAtom op1;
    private final NumberAtom op2;
    private final NumberAtom expected;

    private ArithmeticTestCase(final Operation operation,
                               final NumberAtom op1,
                               final NumberAtom op2,
                               final NumberAtom expected) {
        this.operation = operation;
        this.op1 = op1;
        this.op2 = op2;
        this.expected = expected;
    }

    @SuppressWarnings("StaticMethodNamingConvention")
    public static Builder mk(final Operation operation) {
        return new Builder(operation);
    }

    public void verifyResult() {
        final NumberAtom actual = operation.apply(op1, op2);
        assertThat("Result should be of type" + expected.getClass().getSimpleName(),
                actual, CoreMatchers.instanceOf(expected.getClass()));
        assertThat(String.format("%s %s %s should be equal to %s", op1, operation, op2, expected),
                actual, CoreMatchers.equalTo(expected));
    }

    public static class Builder {
        private final Operation operation;
        private NumberAtom op1;
        private NumberAtom op2;
        private NumberAtom result;

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

        public Builder expect(final NumberAtom res) {
            result = res;
            return this;
        }

        public ArithmeticTestCase build() {
            return new ArithmeticTestCase(operation, op1, op2, result);
        }
    }

    public abstract static class Operation implements Func2<NumberAtom, NumberAtom, NumberAtom> {
        private final String name;

        protected Operation(final String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

    }
}
