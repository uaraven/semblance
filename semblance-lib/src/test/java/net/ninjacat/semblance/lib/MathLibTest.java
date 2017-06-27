package net.ninjacat.semblance.lib;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.asNumber;
import static net.ninjacat.semblance.utils.Values.number;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * This test suite does not attempt to validate correctness of various math functions, it merely tests that
 * they are declared correctly and can be used.
 */
public class MathLibTest {

    private Interpreter interpreter;

    @Before
    public void setUp() throws Exception {
        interpreter = new Interpreter(new MathLib());
    }

    @Test
    public void shouldReturnPi() throws Exception {
        final LispValue result = interpreter.run("math/pi");

        assertThat(result, is(number(3.14159265358979323846)));
    }

    @Test
    public void shouldReturnE() throws Exception {
        final LispValue result = interpreter.run("math/e");

        assertThat(result, is(number(2.7182818284590452354)));
    }

    @Test
    public void shouldCalculateSin() throws Exception {
        final LispValue result = interpreter.run("(math/sin (/ math/pi 2))");

        assertThat(result, is(number(1.0)));
    }

    @Test
    public void shouldCalculateCos() throws Exception {
        final LispValue result = interpreter.run("(math/cos (/ math/pi 2))");

        assertThat(asNumber(result).doubleValue(), Matchers.lessThan(1e-15));
    }


    @Test
    public void shouldCalculateTan() throws Exception {
        final LispValue result = interpreter.run("(math/tan 0)");

        assertThat(asNumber(result).doubleValue(), Matchers.lessThan(1e-15));
    }

    @Test
    public void shouldCalculateAtan2() throws Exception {
        final LispValue result = interpreter.run("(math/atan2 1 2)");

        assertThat(asNumber(result).doubleValue(), Matchers.is(0.4636476090008061));
    }

    @Test
    public void shouldCalculateAsin() throws Exception {
        final LispValue result = interpreter.run("(math/asin 0.5)");

        assertThat(Math.abs(asNumber(result).doubleValue() - Math.PI / 6), Matchers.lessThan(1e-7));
    }

    @Test
    public void shouldCalculateAcos() throws Exception {
        final LispValue result = interpreter.run("(math/acos 0.5)");

        assertThat(Math.abs(asNumber(result).doubleValue() - Math.PI / 3), Matchers.lessThan(1e-7));
    }


    @Test
    public void shouldCalculateAtan() throws Exception {
        final LispValue result = interpreter.run("(math/atan 1)");

        assertThat(Math.abs(asNumber(result).doubleValue() - Math.PI / 4), Matchers.lessThan(1e-7));
    }


    @Test
    public void shouldCalculateSqrt() throws Exception {
        final LispValue result = interpreter.run("(math/sqrt 9)");

        assertThat(asNumber(result).doubleValue(), Matchers.is(3.0));
    }

    @Test
    public void shouldCalculateLog() throws Exception {
        final LispValue result = interpreter.run("(math/log math/e)");

        assertThat(asNumber(result).doubleValue(), Matchers.is(1.0));
    }

    @Test
    public void shouldCalculateExp() throws Exception {
        final LispValue result = interpreter.run("(math/exp 2)");

        assertThat(Math.abs(asNumber(result).doubleValue() - Math.E * Math.E), Matchers.lessThan(1e-7));
    }

    @Test
    public void shouldConvertDegreesToRadians() throws Exception {
        final LispValue result = interpreter.run("(math/to-radians 180)");

        assertThat(asNumber(result).doubleValue(), Matchers.is(Math.PI));
    }

    @Test
    public void shouldConvertRadiansToDegrees() throws Exception {
        final LispValue result = interpreter.run("(math/to-degrees math/pi)");

        assertThat(result, Matchers.is(number(180.0)));
    }

}