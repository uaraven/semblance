package net.ninjacat.semblance.lib;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.Vector;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import org.junit.Before;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class RegexWrapperTest {
    private Interpreter interpreter;

    @Before
    public void setUp() throws Exception {
        interpreter = new Interpreter(new RegexLib());
    }

    @Test
    public void shouldMatchRegex() throws Exception {
        final LispValue result = interpreter.run("(re/match \"This is a test\" \".*is a(.*st)$\")");

        assertThat(isVector(result), is(true));

        final Vector resultVector = asVector(result);

        assertThat(resultVector.length(), is(2));
        assertThat(resultVector.get(0), is(string("This is a test")));
        assertThat(resultVector.get(1), is(string(" test")));
    }


    @Test
    public void shouldNotMatchRegex() throws Exception {
        final LispValue result = interpreter.run("(re/match \"This is a test\" \".*no-match$\")");

        assertThat(result, is(NilCollection.INSTANCE));
    }

    @Test
    public void shouldMatchIgnoringCase() throws Exception {
        final LispValue result = interpreter.run("(re/match \"This IS a Test\" \".*is a(.*st)$\" re/case-insensitive)");

        assertThat(isVector(result), is(true));

        final Vector resultVector = asVector(result);

        assertThat(resultVector.length(), is(2));
        assertThat(resultVector.get(0), is(string("This IS a Test")));
        assertThat(resultVector.get(1), is(string(" Test")));
    }


    @Test
    public void shouldMatchCompiledRegex() throws Exception {
        final LispValue result = interpreter.run("(let ((regex (re/compile \".*is a(.*st)$\" re/case-insensitive)))" +
                "(regex match \"THIS IS A TEST\"))");

        assertThat(isVector(result), is(true));

        final Vector resultVector = asVector(result);

        assertThat(resultVector.length(), is(2));
        assertThat(resultVector.get(0), is(string("THIS IS A TEST")));
        assertThat(resultVector.get(1), is(string(" TEST")));
    }


    @Test
    public void shouldNotMatchCompiledRegex() throws Exception {
        final LispValue result = interpreter.run("(let ((regex (re/compile \".*is a(.*st)$\" re/case-insensitive)))" +
                "(regex match \"Completely different text\"))");
        assertThat(result, is(NilCollection.INSTANCE));
    }

    @Test(expected = SemblanceRuntimeException.class)
    public void shouldFailForInvalidRegexOperation() throws Exception {
        interpreter.run("(let ((regex (re/compile \".*is a(.*st)$\" re/case-insensitive)))" +
                "(regex match-all \"Completely different text\"))");
    }

    @Test
    public void shouldNotFindCompiledRegex() throws Exception {
        final LispValue result = interpreter.run("(let ((regex (re/compile \".*is a(.*st)$\" re/case-insensitive)))" +
                "(regex find \"Completely different text\"))");
        assertThat(result, is(NilCollection.INSTANCE));
    }

    @Test
    public void shouldFindInCompiledRegex() throws Exception {
        final LispValue result = interpreter.run("(let ((regex (re/compile \"te.t\" re/case-insensitive)))" +
                "(regex find \"IS THIS A TEST or a test\"))");

        assertThat(isVector(result), is(true));

        final Vector resultVector = asVector(result);

        assertThat(resultVector.length(), is(1));
        assertThat(resultVector.get(0), is(string("TEST")));
    }

    @Test
    public void shouldMatchWithMatcher() throws Exception {
        final LispValue result = interpreter.runHere("(set1 regex (re/compile \".*is a(.*st)$\" re/case-insensitive))" +
                "(set1 matcher (regex get-matcher \"THIS IS A TEST\"))" +
                "(matcher match)");

        assertThat(isVector(result), is(true));

        final Vector resultVector = asVector(result);

        assertThat(resultVector.length(), is(2));
        assertThat(resultVector.get(0), is(string("THIS IS A TEST")));
        assertThat(resultVector.get(1), is(string(" TEST")));

        final LispValue result2 = interpreter.runHere("(matcher find)");

        assertThat(result2, is(NilCollection.INSTANCE));
    }

    @Test(expected = SemblanceRuntimeException.class)
    public void shouldFailForInvalidMatcherOperation() throws Exception {
        interpreter.run("(set1 regex (re/compile \".*is a(.*st)$\" re/case-insensitive))" +
                "(set1 matcher (regex get-matcher \"THIS IS A TEST\"))" +
                "(matcher match-all)");
    }

    @Test
    public void shouldFindWithMatcher() throws Exception {
        final LispValue result = interpreter.runHere("(set1 regex (re/compile \"te.t\" re/case-insensitive))" +
                "(set1 matcher (regex get-matcher \"THIS IS A TEST AND a test\"))" +
                "(matcher find)");

        assertThat(isVector(result), is(true));

        final Vector resultVector = asVector(result);

        assertThat(resultVector.length(), is(1));
        assertThat(resultVector.get(0), is(string("TEST")));

        final LispValue result2 = interpreter.runHere("(matcher find)");
        final Vector resultVector2 = asVector(result2);

        assertThat(resultVector2.length(), is(1));
        assertThat(resultVector2.get(0), is(string("test")));

    }

}