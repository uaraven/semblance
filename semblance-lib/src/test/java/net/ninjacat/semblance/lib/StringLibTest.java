package net.ninjacat.semblance.lib;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.errors.runtime.NotEnoughParametersException;
import org.junit.Before;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class StringLibTest {

    private Interpreter interpreter;

    @Before
    public void setUp() throws Exception {
        interpreter = new Interpreter(new StringLib());
    }

    @Test
    public void shouldFindIndexOfSubstring() throws Exception {
        final LispValue result = interpreter.run("(index-of \"Hello world\" \"world\")");

        assertThat(result, is(number(6)));
    }

    @Test
    public void shouldNotFindIndexOfNonExistingSubstring() throws Exception {
        final LispValue result = interpreter.run("(index-of \"Hello world\" \"cruel\")");

        assertThat(result, is(number(-1)));
    }

    @Test(expected = NotEnoughParametersException.class)
    public void indexOfShouldFailWhenNotEnoughParameters() throws Exception {
        interpreter.run("(index-of \"Hello world\")");
    }

    @Test(expected = NotEnoughParametersException.class)
    public void substringShouldFailWhenNotEnoughParameters() throws Exception {
        interpreter.run("(substring \"Hello world\")");
    }

    @Test
    public void shouldExtractSubstringFromStartIndex() throws Exception {
        final LispValue result = interpreter.run("(substring \"Hello world\" 6)");

        assertThat(result, is(string("world")));
    }

    @Test
    public void shouldExtractSubstringBetweenIndices() throws Exception {
        final LispValue result = interpreter.run("(substring \"Hello world\" 6 9)");

        assertThat(result, is(string("wor")));
    }

    @Test
    public void shouldReplaceSubstring() throws Exception {
        final LispValue result = interpreter.run("(replace \"Hello world\" \"Hello\" \"Goodbye\")");

        assertThat(result, is(string("Goodbye world")));
    }

    @Test
    public void shouldNotReplaceSubstringIfMatchNotFound() throws Exception {
        final LispValue result = interpreter.run("(replace \"Hello world\" \"bro\" \"Goodbye\")");

        assertThat(result, is(string("Hello world")));
    }

    @Test(expected = NotEnoughParametersException.class)
    public void replaceShouldFailWhenNotEnoughParameters() throws Exception {
        interpreter.run("(replace \"Hello world\" \"Goodbye\")");
    }

    @Test
    public void shouldReplaceAllSubstrings() throws Exception {
        final LispValue result = interpreter.run("(replace-all \"Hello Hello Hello world\" \"Hello\" \"Goodbye\")");

        assertThat(result, is(string("Goodbye Goodbye Goodbye world")));
    }

    @Test
    public void shouldReplaceAllByRegex() throws Exception {
        final LispValue result = interpreter.run("(replace-all \"Hello cruel world\" \"\\\\s\" \"!\")");

        assertThat(result, is(string("Hello!cruel!world")));
    }

    @Test
    public void shouldNotReplaceAllSubstringsIfMatchNotFound() throws Exception {
        final LispValue result = interpreter.run("(replace-all \"Hello world\" \"bro\" \"Goodbye\")");

        assertThat(result, is(string("Hello world")));
    }

    @Test(expected = NotEnoughParametersException.class)
    public void replaceAllShouldFailWhenNotEnoughParameters() throws Exception {
        interpreter.run("(replace-all \"Hello world\" \"Goodbye\")");
    }

    @Test
    public void shouldSplitWithDefaultSplitter() throws Exception {
        final LispValue result = interpreter.run("(split \"a b c d\")");
        assertThat(result, is(smartVector("a", "b", "c", "d")));
    }

    @Test
    public void shouldSplitWithCustomSplitter() throws Exception {
        final LispValue result = interpreter.run("(split \"a;b c;d\" \";\")");
        assertThat(result, is(smartVector("a", "b c", "d")));
    }

    @Test
    public void shouldJoinWithDefaultDelimiter() throws Exception {
        final LispValue result = interpreter.run("(join [1 2 3 4])");
        assertThat(result, is(string("1 2 3 4")));
    }

    @Test
    public void shouldJoinWithCustomDelimiter() throws Exception {
        final LispValue result = interpreter.run("(join [1 2 3 4 \"abc\"] \"-\")");
        assertThat(result, is(string("1-2-3-4-abc")));
    }

    @Test
    public void shouldLowerCaseAscii() throws Exception {
        final LispValue result = interpreter.run("(to-lower \"tHiS tExT\")");
        assertThat(result, is(string("this text")));
    }

    @Test
    public void shouldLowerCaseUnicode() throws Exception {
        final LispValue result = interpreter.run("(to-lower \"ЦеЙ теКсТ iS iN UkrAiniAn\" \"uk-UA\")");
        assertThat(result, is(string("цей текст is in ukrainian")));
    }

    @Test
    public void shouldUpperCaseAscii() throws Exception {
        final LispValue result = interpreter.run("(to-upper \"tHiS tExT\")");
        assertThat(result, is(string("THIS TEXT")));
    }

    @Test
    public void shouldUpperCaseUnicode() throws Exception {
        final LispValue result = interpreter.run("(to-upper \"ЦеЙ теКсТ iS iN UkrAiniAn\" \"uk-UA\")");
        assertThat(result, is(string("ЦЕЙ ТЕКСТ IS IN UKRAINIAN")));
    }

    @Test
    public void shouldCompareStringsIgnoringCase() throws Exception {
        final LispValue result = interpreter.run("(eq-ignore-case \"SaMe TeXt\" \"sAmE tExT\")");
        assertThat(result, is(Constants.TRUE));
    }

    @Test
    public void shouldCompareNonEqualStringsIgnoringCase() throws Exception {
        final LispValue result = interpreter.run("(eq-ignore-case \"SaMe TeXt\" \"diFferenT tExT\")");
        assertThat(result, is(Constants.FALSE));
    }
}