package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.errors.compile.UnterminatedStringException;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

@SuppressWarnings({"NonBooleanMethodNameMayNotStartWithQuestion", "DuplicateStringLiteralInspection", "InstanceMethodNamingConvention"})
public class ReaderStreamTest {

    @Test
    public void shouldTokenizeSymbol() throws Exception {
        final ReaderStream stream = ReaderStream.readString("symbol");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be symbol", tokens.get(0).getType(), is(Token.TokenType.Symbol));
        assertThat("Token value should be 'symbol'", tokens.get(0).getValue(), is("symbol"));
    }

    @Test
    public void shouldTokenizeNumber() throws Exception {
        final ReaderStream stream = ReaderStream.readString("42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be integer", tokens.get(0).getType(), is(Token.TokenType.Integer));
        assertThat("Token value should be '42'", tokens.get(0).getValue(), is("42"));
    }

    @Test
    public void shouldTokenizeNegativeNumber() throws Exception {
        final ReaderStream stream = ReaderStream.readString("-42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be integer", tokens.get(0).getType(), is(Token.TokenType.Integer));
        assertThat("Token value should be '-42'", tokens.get(0).getValue(), is("-42"));
    }

    @Test
    public void shouldTokenizeDouble() throws Exception {
        final ReaderStream stream = ReaderStream.readString("42.42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '42.42'", tokens.get(0).getValue(), is("42.42"));
    }

    @Test
    public void shouldTokenizeNegativeDouble() throws Exception {
        final ReaderStream stream = ReaderStream.readString("-42.42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '-42.42'", tokens.get(0).getValue(), is("-42.42"));
    }

    @Test
    public void shouldTokenizeExplicitPositiveDouble() throws Exception {
        final ReaderStream stream = ReaderStream.readString("+42.42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '+42.42'", tokens.get(0).getValue(), is("+42.42"));
    }

    @Test
    public void shouldTokenizeScientificDouble() throws Exception {
        final ReaderStream stream = ReaderStream.readString("42e42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '4.2E43'", tokens.get(0).getValue(), is("4.2E43"));
    }

    @Test
    public void shouldTokenizeNegativeScientificDouble() throws Exception {
        final ReaderStream stream = ReaderStream.readString("-42e42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '-4.2E43'", tokens.get(0).getValue(), is("-4.2E43"));
    }

    @Test
    public void shouldTokenizeComplicatedScientificDouble() throws Exception {
        final ReaderStream stream = ReaderStream.readString("-42.33e+42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '-4.233E43'", tokens.get(0).getValue(), is("-4.233E43"));
    }


    @Test
    public void shouldTokenizeScientificDoubleWithNegativeExponent() throws Exception {
        final ReaderStream stream = ReaderStream.readString("42e-42");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '4.2E-41'", tokens.get(0).getValue(), is("4.2E-41"));
    }

    @Test
    public void shouldTokenizeDoubleEvenWhenFractionalPartIsZero() throws Exception {
        final ReaderStream stream = ReaderStream.readString("42.0");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '42.0'", tokens.get(0).getValue(), is("42.0"));
    }

    @Test
    public void shouldTokenizeNegativeDoubleEvenWhenFractionalPartIsZero() throws Exception {
        final ReaderStream stream = ReaderStream.readString("-42.0");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '-42.0'", tokens.get(0).getValue(), is("-42.0"));
    }

    @Test
    public void shouldTokenizeStringWithQuote() throws Exception {
        final ReaderStream stream = ReaderStream.readString("\"str\\\"ing\"");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be 'str\"ing'", tokens.get(0).getValue(), is("str\"ing"));
    }

    @Test
    public void shouldTokenizeStringWithEscapes() throws Exception {
        final ReaderStream stream = ReaderStream.readString("\"s\\t\\r\\\"i\\n\\g\"");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be correct", tokens.get(0).getValue(), is("s\t\r\"i\ng"));
    }


    @Test
    public void shouldTokenizeString() throws Exception {
        final ReaderStream stream = ReaderStream.readString("\"string\"");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be 'string'", tokens.get(0).getValue(), is("string"));
    }

    @Test(expected = UnterminatedStringException.class)
    public void shouldFailOnUnterminatedString() throws Exception {
        final ReaderStream stream = ReaderStream.readString("\"string string2");

        stream.tokenize();
    }

    @Test
    public void shouldTokenizeOpenParens() throws Exception {
        final ReaderStream stream = ReaderStream.readString("(");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
    }

    @Test
    public void shouldTokenizeCloseParens() throws Exception {
        final ReaderStream stream = ReaderStream.readString(")");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be CloseParens", tokens.get(0).getType(), is(Token.TokenType.CloseParens));
    }

    @Test
    public void shouldTokenizeSpecialChars() throws Exception {
        final ReaderStream stream = ReaderStream.readString("'#");
        stream.registerSpecial('\'');
        stream.registerSpecial('#');
        final List<Token> tokens = stream.tokenize();

        assertThat("Should have two tokens", tokens.size(), is(2));
        assertThat("Token type should be special", tokens.get(0).getType(), is(Token.TokenType.Special));
        assertThat("Token type should be special", tokens.get(1).getType(), is(Token.TokenType.Special));
        assertThat("Token value should be '", tokens.get(0).getValue(), is("'"));
        assertThat("Token value should be #", tokens.get(1).getValue(), is("#"));
    }

    @Test
    public void shouldTokenizeSymbolsWithComma() throws Exception {
        final ReaderStream stream = ReaderStream.readString(",symbol");
        stream.registerSpecial(',');

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 2 token", tokens.size(), is(2));
        assertThat("Should have parse as symbol", tokens.get(0).getType(), is(Token.TokenType.Special));
        assertThat("Should have parse as symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
    }

    @Test
    public void shouldTokenizeSymbolsWithAt() throws Exception {
        final ReaderStream stream = ReaderStream.readString("@symbol");
        stream.registerSpecial('@');

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 1 token", tokens.size(), is(2));
        assertThat("Should have parse as symbol", tokens.get(0).getType(), is(Token.TokenType.Special));
        assertThat("Should have parse as symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
    }


    @Test
    public void shouldTokenizeOneElementList() throws Exception {
        final ReaderStream stream = ReaderStream.readString("( symbol )");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 3 tokens", tokens.size(), is(3));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be CloseParens", tokens.get(2).getType(), is(Token.TokenType.CloseParens));
    }

    @Test
    public void shouldTokenizeSymbolWithHyphen() throws Exception {
        final ReaderStream stream = ReaderStream.readString(":symbol-two");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 1 token", tokens.size(), is(1));
        assertThat("Token type should be symbol", tokens.get(0).getType(), is(Token.TokenType.Symbol));
    }

    @Test
    public void shouldTokenizeMultiElementList() throws Exception {
        final ReaderStream stream = ReaderStream.readString("( symbol \"String\" 15)");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 5 tokens", tokens.size(), is(5));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be string", tokens.get(2).getType(), is(Token.TokenType.String));
        assertThat("Token type should be integer", tokens.get(3).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be CloseParens", tokens.get(4).getType(), is(Token.TokenType.CloseParens));
    }

    @Test
    public void shouldTokenizeMultiElementList2() throws Exception {
        final ReaderStream stream = ReaderStream.readString("( symbol \"String\" 15 -33.2)");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 6 tokens", tokens.size(), is(6));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be string", tokens.get(2).getType(), is(Token.TokenType.String));
        assertThat("Token type should be integer", tokens.get(3).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be integer", tokens.get(4).getType(), is(Token.TokenType.Double));
        assertThat("Token type should be CloseParens", tokens.get(5).getType(), is(Token.TokenType.CloseParens));
    }

    @Test
    public void shouldTokenizeNestedList() throws Exception {
        final ReaderStream stream = ReaderStream.readString("( symbol (\"String\") -33.2)");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 7 tokens", tokens.size(), is(7));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be OpenParens", tokens.get(2).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be string", tokens.get(3).getType(), is(Token.TokenType.String));
        assertThat("Token type should be integer", tokens.get(4).getType(), is(Token.TokenType.CloseParens));
        assertThat("Token type should be integer", tokens.get(5).getType(), is(Token.TokenType.Double));
        assertThat("Token type should be CloseParens", tokens.get(6).getType(), is(Token.TokenType.CloseParens));
    }

    @Test
    public void shouldTokenizeVector() throws Exception {
        final ReaderStream stream = ReaderStream.readString("[1 2]");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 4 tokens", tokens.size(), is(4));
        assertThat("Token type should be OpenBracket", tokens.get(0).getType(), is(Token.TokenType.OpenBracket));
        assertThat("Token type should be integer", tokens.get(1).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be integer", tokens.get(2).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be CloseBracket", tokens.get(3).getType(), is(Token.TokenType.CloseBracket));
    }

    @Test
    public void shouldParseReaderMacro() throws Exception {
        final ReaderStream stream = ReaderStream.readString("'symbol");
        stream.registerSpecial('\'');

        final List<Token> tokens = stream.tokenize();
        assertThat("Should have 2 tokens", tokens.size(), is(2));
        assertThat("Token type should be special", tokens.get(0).getType(), is(Token.TokenType.Special));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));

    }

    @Test
    public void shouldParseReaderMacroBeforeList() throws Exception {
        final ReaderStream stream = ReaderStream.readString("'()");
        stream.registerSpecial('\'');

        final List<Token> tokens = stream.tokenize();
        assertThat("Should have 3 tokens", tokens.size(), is(3));
        assertThat("Token type should be special", tokens.get(0).getType(), is(Token.TokenType.Special));
        assertThat("Token type should be OpenParen", tokens.get(1).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be CloseParen", tokens.get(2).getType(), is(Token.TokenType.CloseParens));

    }

    @Test
    public void shouldParseArithmeticCall() throws Exception {
        final ReaderStream stream = ReaderStream.readString("(+ 2 3)");

        final List<Token> tokens = stream.tokenize();
        assertThat("Should have 4 tokens", tokens.size(), is(5));
        assertThat("Token type should be OpenParen", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be Symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be Int", tokens.get(2).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be Int", tokens.get(3).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be CloseParen", tokens.get(4).getType(), is(Token.TokenType.CloseParens));

    }

    @Test
    public void shouldParseMap() throws Exception {
        final ReaderStream stream = ReaderStream.readString("{:one 1 :two 2}");

        final List<Token> tokens = stream.tokenize();
        assertThat("Should have 6 tokens", tokens.size(), is(6));
        assertThat("Token type should be OpenBrace", tokens.get(0).getType(), is(Token.TokenType.OpenBrace));
        assertThat("Token type should be Symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be Int", tokens.get(2).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be Symbol", tokens.get(3).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be Int", tokens.get(4).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be CloseParen", tokens.get(5).getType(), is(Token.TokenType.CloseBrace));

    }


    @Test
    public void shouldParseMultilineStrings() throws Exception {
        final ReaderStream stream = ReaderStream.readString("\"\"\"Multiline\nString\"\"\"");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 1 token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be multiline string", tokens.get(0).getValue(), is("Multiline\nString"));

    }

    @Test
    public void shouldParseMultilineStringsWithQuotes() throws Exception {
        final ReaderStream stream = ReaderStream.readString("\"\"\"Multiline\"\"String\"\"\"");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 1 token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be multiline string", tokens.get(0).getValue(), is("Multiline\"\"String"));
    }

    @Test
    public void shouldNotParseEscapesInMultiline() throws Exception {
        final ReaderStream stream = ReaderStream.readString("\"\"\"Multiline\\\"\\t\\nString\"\"\"");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 1 token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be multiline string", tokens.get(0).getValue(), is("Multiline\\\"\\t\\nString"));
    }

    @Test
    public void shouldParseMultilineStringsWithOtherText() throws Exception {
        final ReaderStream stream = ReaderStream.readString("Look \"\"\"Multiline\nString\"\"\" Wow");

        final List<Token> tokens = stream.tokenize();

        assertThat("Should have 1 token", tokens.size(), is(3));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be string", tokens.get(1).getType(), is(Token.TokenType.String));
        assertThat("Token type should be string", tokens.get(2).getType(), is(Token.TokenType.Symbol));
        assertThat("Token value should be Look", tokens.get(0).getValue(), is("Look"));
        assertThat("Token value should be multiline string", tokens.get(1).getValue(), is("Multiline\nString"));
        assertThat("Token value should be Wow", tokens.get(2).getValue(), is("Wow"));

    }

}
