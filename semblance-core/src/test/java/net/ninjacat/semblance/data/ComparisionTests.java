package net.ninjacat.semblance.data;


import net.ninjacat.smooth.collections.Lists;
import org.junit.Test;

import java.util.List;

import static net.ninjacat.semblance.data.ComparisionTestCase.mk;
import static net.ninjacat.semblance.utils.Values.*;

@SuppressWarnings("ConstantNamingConvention")
public class ComparisionTests {

    private static final ComparisionTestCase.Operation EQ = new ComparisionTestCase.Operation("=") {
        @SuppressWarnings("unchecked")
        @Override
        public Boolean apply(final NumberAtom op1, final NumberAtom op2) {
            return op1.eq(op2);
        }
    };

    private static final ComparisionTestCase.Operation LT = new ComparisionTestCase.Operation("<") {
        @SuppressWarnings("unchecked")
        @Override
        public Boolean apply(final NumberAtom op1, final NumberAtom op2) {
            return op1.lt(op2);
        }
    };

    private static final ComparisionTestCase.Operation GT = new ComparisionTestCase.Operation(">") {
        @SuppressWarnings("unchecked")
        @Override
        public Boolean apply(final NumberAtom op1, final NumberAtom op2) {
            return op1.gt(op2);
        }
    };

    private static final List<ComparisionTestCase> EQ_CASES = Lists.of(
            mk(EQ).op1(longN(42)).op2(longN(42)).expect(true).build(),

            mk(EQ).op1(longN(42)).op2(bigN("42")).expect(true).build(),

            mk(EQ).op1(bigN("42")).op2(longN(42)).expect(true).build(),

            mk(EQ).op1(longN(42)).op2(doubleN(42.0)).expect(true).build(),

            mk(EQ).op1(doubleN(42.0)).op2(bigN("42")).expect(true).build(),

            mk(EQ).op1(longN(42)).op2(longN(41)).expect(false).build(),

            mk(EQ).op1(longN(42)).op2(bigN("41")).expect(false).build(),

            mk(EQ).op1(bigN("41")).op2(longN(42)).expect(false).build(),

            mk(EQ).op1(longN(42)).op2(doubleN(42.1)).expect(false).build(),

            mk(EQ).op1(doubleN(42.1)).op2(bigN("42")).expect(false).build()
    );

    private static final List<ComparisionTestCase> LT_CASES = Lists.of(
            mk(LT).op1(longN(1)).op2(longN(2)).expect(true).build(),

            mk(LT).op1(longN(1)).op2(bigN("2")).expect(true).build(),

            mk(LT).op1(bigN("1")).op2(longN(2)).expect(true).build(),

            mk(LT).op1(longN(1)).op2(doubleN(2.0)).expect(true).build(),

            mk(LT).op1(doubleN(1)).op2(bigN("2")).expect(true).build(),

            mk(LT).op1(longN(2)).op2(longN(1)).expect(false).build(),

            mk(LT).op1(longN(2)).op2(bigN("1")).expect(false).build(),

            mk(LT).op1(bigN("2")).op2(longN(1)).expect(false).build(),

            mk(LT).op1(longN(2)).op2(doubleN(1)).expect(false).build(),

            mk(LT).op1(doubleN(2.1)).op2(bigN("2")).expect(false).build()
    );

    private static final List<ComparisionTestCase> GT_CASES = Lists.of(
            mk(GT).op1(longN(1)).op2(longN(2)).expect(false).build(),

            mk(GT).op1(longN(1)).op2(bigN("2")).expect(false).build(),

            mk(GT).op1(bigN("1")).op2(longN(2)).expect(false).build(),

            mk(GT).op1(longN(1)).op2(doubleN(2.0)).expect(false).build(),

            mk(GT).op1(doubleN(1)).op2(bigN("2")).expect(false).build(),

            mk(GT).op1(longN(2)).op2(longN(1)).expect(true).build(),

            mk(GT).op1(longN(2)).op2(bigN("1")).expect(true).build(),

            mk(GT).op1(bigN("2")).op2(longN(1)).expect(true).build(),

            mk(GT).op1(longN(2)).op2(doubleN(1)).expect(true).build(),

            mk(GT).op1(doubleN(2.1)).op2(bigN("2")).expect(true).build()
    );

    @Test
    public void testEquality() throws Exception {
        for (final ComparisionTestCase testCase : EQ_CASES) {
            testCase.verifyResult();
        }
    }

    @Test
    public void testLessThan() throws Exception {
        for (final ComparisionTestCase testCase : LT_CASES) {
            testCase.verifyResult();
        }
    }

    @Test
    public void testGreaterThan() throws Exception {
        for (final ComparisionTestCase testCase : GT_CASES) {
            testCase.verifyResult();
        }
    }

}
