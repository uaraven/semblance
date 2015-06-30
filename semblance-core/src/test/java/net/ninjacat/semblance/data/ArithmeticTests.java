package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.ArithmeticTestCase.Operation;
import net.ninjacat.smooth.collections.Lists;
import org.junit.Test;

import java.math.BigInteger;
import java.util.List;

import static java.math.BigInteger.valueOf;
import static net.ninjacat.semblance.data.ArithmeticTestCase.mk;
import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

/**
 * Created on 03/03/15.
 */
@SuppressWarnings("DuplicateStringLiteralInspection")
public class ArithmeticTests {

    private static final Operation ADD = new Operation("+") {
        @SuppressWarnings("unchecked")
        @Override
        public NumberAtom apply(final NumberAtom op1, final NumberAtom op2) {
            return op1.add(op2);
        }
    };

    private static final Operation MUL = new Operation("*") {
        @SuppressWarnings("unchecked")
        @Override
        public NumberAtom apply(final NumberAtom op1, final NumberAtom op2) {
            return op1.mul(op2);
        }
    };

    private static final Operation DIV = new Operation("/") {
        @SuppressWarnings("unchecked")
        @Override
        public NumberAtom apply(final NumberAtom op1, final NumberAtom op2) {
            return op1.div(op2);
        }
    };

    private static final Operation SUB = new Operation("-") {
        @SuppressWarnings("unchecked")
        @Override
        public NumberAtom apply(final NumberAtom op1, final NumberAtom op2) {
            return op1.sub(op2);
        }
    };

    private static final List<ArithmeticTestCase> ADD_CASES = Lists.of(
            mk(ADD).op1(longN(42))
                    .op2(longN(42))
                    .expect(longN(84)).build(),

            mk(ADD).op1(longN(Long.MAX_VALUE))
                    .op2(longN(Long.MAX_VALUE))
                    .expect(bigN(valueOf(Long.MAX_VALUE).multiply(valueOf(2)))).build(),

            mk(ADD).op1(longN(42))
                    .op2(bigN("42000000000000000000000000000"))
                    .expect(bigN("42000000000000000000000000042")).build(),

            mk(ADD).op1(longN(42))
                    .op2(bigN("42"))
                    .expect(longN(84)).build(),

            mk(ADD).op1(longN(42))
                    .op2(doubleN(42))
                    .expect(doubleN(84)).build(),

            mk(ADD).op1(bigN("42"))
                    .op2(bigN("42000000000000000000000000000"))
                    .expect(bigN("42000000000000000000000000042")).build(),

            mk(ADD).op1(doubleN(42.42))
                    .op2(bigN("42000000000000000000000000000"))
                    .expect(doubleN(42000000000000000000000000042.42d)).build()
    );

    private static final List<ArithmeticTestCase> MUL_CASES = Lists.of(
            mk(MUL).op1(longN(42))
                    .op2(longN(2))
                    .expect(longN(84)).build(),

            mk(MUL).op1(longN(Long.MAX_VALUE))
                    .op2(longN(2))
                    .expect(bigN(valueOf(Long.MAX_VALUE).multiply(valueOf(2)))).build(),

            mk(MUL)
                    .op1(bigN("42"))
                    .op2(bigN("42000000000000000000000000000"))
                    .expect(bigN("1764000000000000000000000000000")).build(),

            mk(MUL)
                    .op1(longN(42))
                    .op2(bigN("2"))
                    .expect(longN(84)).build(),

            mk(MUL)
                    .op1(longN(42))
                    .op2(doubleN(2.2))
                    .expect(doubleN(92.4)).build(),

            mk(MUL)
                    .op1(bigN("42"))
                    .op2(doubleN(2.2))
                    .expect(doubleN(92.4)).build()
    );

    private static final List<ArithmeticTestCase> DIV_CASES = Lists.of(
            mk(DIV)
                    .op1(longN(42))
                    .op2(longN(2))
                    .expect(longN(21)).build(),

            mk(DIV)
                    .op1(longN(42))
                    .op2(longN(20))
                    .expect(longN(2)).build(),

            mk(DIV)
                    .op1(longN(42))
                    .op2(longN(22))
                    .expect(longN(1)).build(),

            mk(DIV)
                    .op1(longN(42))
                    .op2(longN(52))
                    .expect(longN(0)).build(),

            mk(DIV)
                    .op1(bigN(42))
                    .op2(bigN(20))
                    .expect(longN(2)).build(),

            mk(DIV)
                    .op1(bigN(42))
                    .op2(bigN(22))
                    .expect(longN(1)).build(),

            mk(DIV)
                    .op1(bigN(42))
                    .op2(bigN(52))
                    .expect(longN(0)).build(),

            mk(DIV)
                    .op1(longN(42))
                    .op2(bigN("2"))
                    .expect(longN(21)).build(),

            mk(DIV)
                    .op1(longN(42))
                    .op2(doubleN(2.0))
                    .expect(doubleN(21.0)).build(),

            mk(DIV)
                    .op1(bigN("42"))
                    .op2(doubleN(2.0))
                    .expect(doubleN(21.0)).build(),

            mk(DIV)
                    .op1(doubleN(42.0))
                    .op2(doubleN(2.0))
                    .expect(doubleN(21.0)).build(),

            mk(DIV)
                    .op1(bigN("1234567890123456789012345678901234567890"))
                    .op2(longN(2))
                    .expect(bigN("617283945061728394506172839450617283945")).build()

    );

    private static final List<ArithmeticTestCase> SUB_CASES = Lists.of(
            mk(SUB)
                    .op1(longN(42))
                    .op2(longN(2))
                    .expect(longN(40)).build(),

            mk(SUB)
                    .op1(longN(Long.MIN_VALUE))
                    .op2(longN(Long.MAX_VALUE))
                    .expect(bigN(BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.valueOf(Long.MAX_VALUE)))).build(),

            mk(SUB)
                    .op1(longN(42))
                    .op2(bigN("42000000000000000000000000042"))
                    .expect(bigN("-42000000000000000000000000000")).build(),

            mk(SUB)
                    .op1(bigN("-42000000000000000000000000042"))
                    .op2(bigN("42000000000000000000000000042"))
                    .expect(bigN("-84000000000000000000000000084")).build(),

            mk(SUB)
                    .op1(bigN("42000000000000000000000000042"))
                    .op2(bigN("42000000000000000000000000042"))
                    .expect(longN(0)).build(),

            mk(SUB)
                    .op1(doubleN(42))
                    .op2(longN(2))
                    .expect(doubleN(40)).build(),

            mk(SUB)
                    .op1(doubleN(42))
                    .op2(doubleN(41.5))
                    .expect(doubleN(0.5)).build(),

            mk(SUB)
                    .op1(doubleN(42))
                    .op2(bigN(40))
                    .expect(doubleN(2)).build()
    );

    @Test
    public void testAddition() throws Exception {
        for (final ArithmeticTestCase testCase : ADD_CASES) {
            testCase.verifyResult();
        }
    }

    @Test
    public void testMultiplication() throws Exception {
        for (final ArithmeticTestCase testCase : MUL_CASES) {
            testCase.verifyResult();
        }
    }

    @Test
    public void testDivision() throws Exception {
        for (final ArithmeticTestCase testCase : DIV_CASES) {
            testCase.verifyResult();
        }
    }

    @Test
    public void testSubtraction() throws Exception {
        for (final ArithmeticTestCase testCase : SUB_CASES) {
            testCase.verifyResult();
        }
    }

    @SuppressWarnings("unchecked")
    @Test(expected = ArithmeticException.class)
    public void testLongDivisionByZeroShouldFail() throws Exception {
        longN(10).div(longN(0));
    }

    @SuppressWarnings("unchecked")
    @Test(expected = ArithmeticException.class)
    public void testBigDivisionByZeroShouldFail() throws Exception {
        bigN(10).div(bigN(0));
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testDoubleDivisionByZeroShouldResultInInfinity() throws Exception {
        final NumberAtom result = doubleN(10).div(doubleN(0));
        assertThat("Should be equal to infinity", result.isInfinity(), is(true));
    }
}
